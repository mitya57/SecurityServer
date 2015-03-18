;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :secsrv)

;;;--- TODO(serg): Add correct processing of filtering conditions
(defun access-path->prepared-sql (access-path)
  "Generate SQL query that selects attribute specified by
`ACCESS-PATH'. The result is a FORMAT template string with ~A
placeholder in place of top level object's identifier.
"
  (assert (not (null (access-path-model access-path))))
  (log-message :info "Generating SQL template for ~A using model ~A."
               (access-path-source access-path)
               (model-name (access-path-model access-path)))
  (let ((parsed-path (access-path-parsed access-path))
        (sql-select)
        (sql-from (format nil "FROM ~A T0 " (model-table (access-path-model access-path))))
        (sql-where)
        (joins))
    (loop
       :initially
       (setf sql-where (format nil " WHERE T0.~A = ~~A"
                               (attribute-column-name (model-primary-key (access-path-model access-path)))))
       :for attribute :in (rest parsed-path)
       :for previous-attribute = (first parsed-path) :then attribute
       :for current-model = (attribute-model attribute)
       :for seq :from 1
       :do
       (push (format nil " JOIN ~A T~D ON (T~D.~A=T~D.~A) "
                     ;; joined table and it's alias
                     (model-table current-model) seq
                     ;; join column
                     seq
                     (if (attribute-reverse-attribute previous-attribute)
                         (attribute-column-name (attribute-reverse-attribute previous-attribute))
                         (attribute-column-name (model-primary-key current-model)))
                     ;; join column in previous table
                     (1- seq) (attribute-column-name previous-attribute))
             joins)
       :finally
       (setf sql-select
             (format nil "SELECT T~D.~A "
                     (if (rest parsed-path) seq 0)   ; Use T0 if no other tables were joined
                     (attribute-column-name (or attribute (first parsed-path))))))
    (concatenate 'string
                 sql-select sql-from
                 (apply #'concatenate 'string joins)
                 sql-where)))

(defun parse-filtering-expression (expression model)
  "Parse EXPRESSION assuming that it is applied to an object of type `MODEL'."
  (declare (ignore model))
  (check-type expression string "a filtering expression")
  '())


(defun estimate-access-path-cardinality (access-path)
  (check-type access-path access-path)
  (if (some #'attribute-reverse-attribute (access-path-parsed access-path))
      :set
      :single))


;;;--- TODO(serg@msu.ru): parse filtering conditions att[<filter expression>]
(defun parse-access-path-source (access-path top-level-model)
  "Parse access path expression stored in 'source slot of the
ACCESS-PATH.

Output: updates 'parsed' and 'main-model' slots of ACCESS-PATH
argument.

Parsed representation of the expression is a list of either
an `attribute' object, or an sublist of the form (`attribute'
parsed-filter), if filtering expression 'att[filter]' was applied to
the attribute.

Main-model is a reference to the first's attribute model, as defined
by TOP-lEVEL-MODEL.
"
  (check-type access-path access-path "a valid access-path")
  (check-type top-level-model (or model list function) "a valid top-level-model argument")
  (check-type *current-policy* policy)
  (let ((access-path-string (access-path-source access-path))
        (skip-first (or (listp top-level-model) (functionp top-level-model)))
        (first-model (if (typep top-level-model 'model)
                         top-level-model
                         nil))
        parsed)
    (loop
       :with current-model = first-model
       :for count :from 1
       :for start = 0 :then (1+ pos)
       :for pos = (position-if #'(lambda (c) (member c '(#\. #\[)))
                               access-path-string
                               :start start)
       :for attribute-name = (subseq access-path-string start pos)
       ;;--- TODO(serg@msu.ru) Validate name of the skipped item
       ;; :when (and skip-first (= 1 count)) :do ...
       :when (and skip-first (= 1 count)) :do
       (unless current-model
         (when (listp top-level-model)
           (setf first-model (cdr (assoc attribute-name top-level-model :test #'string-equal))))
         (when (functionp top-level-model)
           (setf first-model (funcall top-level-model attribute-name)))
         (setf current-model first-model))
       (when (null current-model)
         (error 'malformed-statement-error
                :text (format nil "Unable to parse access path expression '~A': undefined model '~A'"
                              access-path-string attribute-name)))
       :when (or (null skip-first) (< 1 count)) :do
       (let ((attribute (find-attribute attribute-name current-model)))
         (push attribute parsed)
         (if attribute
             (setf current-model (attribute-target-model attribute))
             (error 'malformed-statement-error
                    :text (format nil "Unable to parse access path expression '~A': undefined attribute '~A'"
                                  access-path-string attribute-name))))
       :while pos)
    (setf (slot-value access-path 'main-model) first-model)
    (setf (slot-value access-path 'target-attribute) (car parsed))
    (setf (slot-value access-path 'parsed) (nreverse parsed))
    parsed))


(defmethod parse ((access-path access-path) source &key (top-level-model nil))
  "Parse access path expression SOURCE, assuming that it is applied to
an object of `model' specified by TOP-LEVEL-MODEL argument. If
TOP-LEVEL-MODEL may be an instance of `model' class, an alist that
maps attribute names to models, or a function that returns a model by
attribute name.

If TOP-LEVEL-MODEL is an instance of `model', then the first attribute
of the access path is searched in that model. If TOP-LEVEL-MODEL is an
alist, then this alist represents a mapping between attribute names
and models. The first attribute of the access path effectively means
model name. This feature supports access paths like 'object.attribute'
or 'user.attribute', where names 'object' and 'user' are mapped to
appropriate models depending on the context where access path
expression appeared. Finally, if TOP-LEVEL-MODEL is a function, then
it is called with the only argument - the name of the first
attribute."
  (check-type top-level-model (or model list function) "a valid top-level-model argument")

  (setf (slot-value access-path 'source) source)
  (parse-access-path-source access-path top-level-model)
  (setf (slot-value access-path 'cardinality) (estimate-access-path-cardinality access-path))
  (setf (slot-value access-path 'prepared-sql) (access-path->prepared-sql access-path)))
