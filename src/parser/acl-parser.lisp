;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; Input file parsers

(in-package :secsrv)

(alexandria:define-constant +relational-operators-names+
    '("=")
  :test #'equal
  :documentation "List of all known relational operators of the ACL definition language.")

(alexandria:define-constant +logical-operators-names+
    '("and" "or")
  :test #'equal
  :documentation "List of all known logical operators of the ACL definition language.")

;;; Conditions

(define-condition malformed-statement-error (parsing-error)
  ()       ; no extra slots defined so far
  (:documentation "Represent specific parsing error, such as unknown command."))

(alexandria:define-constant +primary-key-default-attribute+ "id"
  :test #'string-equal
  :documentation "Model's attribute with this name is treated as a primary key.")



(defmethod parse ((the-model model) statement &key)
  "Parses STATEMENT defining application level database table
and return instance of `model', or signal `malformed-statement-error'
condition."
  (destructuring-bind (command &optional name . declarations) statement
    (assert (string-equal command "defmodel"))
    (setf (model-name the-model) name)
    (loop :for (dec value) :in declarations :do
       (alexandria:switch ((string-downcase dec) :test #'string=)
         ("table" (setf (model-table the-model) value))
         ("operations" (setf (model-operations the-model) value))
         ("attributes"
          (loop
             ;; parse '(attribute column [[references] model])'
             :for (attribute-name db-column-name nil target-name) :in value
             :for attribute = (make-attribute attribute-name (symbol-name db-column-name)
                                              :target-model target-name)
             :do
             (add-attribute attribute the-model)
             (when (string-equal attribute-name +primary-key-default-attribute+)
               (setf (model-primary-key the-model) attribute))))
         (t (error 'malformed-statement-error
                   :text (format nil "Unknown declaration `~A` in object `~A`." dec name)))))
    the-model))


(defmethod parse ((the-role role) statement &key)
  "Parse STATEMENT definig a role, or signal `malformed-statement-error' condition."
  (destructuring-bind (command &optional name . parameters) statement
    (assert (string-equal command "defrole"))
    ;;(format t "role-name=~A parameters=~A~%" name parameters)
    (setf (role-name the-role) name)
    (setf (slot-value the-role 'parameters)
          (mapcar #'(lambda (p)
                      "Find referenced model or signal an error."
                      (let ((model (find-model p)))
                        (when (null model)
                          (error 'malformed-statement-error
                                 :text (format nil "Unknown model '~A' found in definition role '~A'" p name)))
                        model))
                  parameters))
    the-role))


;;;
;;; Rules
;;;


(defun parse-constraint (expression top-level-model &optional (return-constraint-object t))
  "Parse rule constraint from an s-expression EXPRESSION. The constraint
is a part of rule defined for objects of type `MODEL'.

If RETURN-CONSTRAINT-OBJECT is not NIL, then expressions of form '(= a
b)' are treated as '(and (= a b))', so `relation' object is covered
by dummy `constrint'.
"
  (destructuring-bind (operation . parameters) expression
    (cond
      ((member operation +logical-operators-names+ :test #'string-equal)
       (make-constraint (if (string-equal "and" operation) :and :or)
                        (mapcar #'(lambda (p)
                                    (parse-constraint p top-level-model nil))
                                parameters)))
      ((member operation +relational-operators-names+ :test #'string-equal)
       (flet ((parse-operand (expr)
                "Parses access paths and return everything else as is."
                (typecase expr
                  (symbol
                   (let ((ap (make-access-path))
                         (expr-string (format nil "~A" expr)))
                     (parse ap expr-string :top-level-model top-level-model)
                     ap))
                  (t expr))))
         (let ((parsed-relation (make-relation operation
                                               (mapcar #'parse-operand parameters))))
           (if return-constraint-object
               (make-constraint :and parsed-relation)
               parsed-relation)))))))



(defmethod parse ((the-rule rule) statement &key)
  "Parse S-expression STATEMENT and return `rule' object, or raise
condition of type `malformed-statement-error'."
  (destructuring-bind (command &optional name . declarations) statement
    (assert (string-equal command "rule"))
    (setf (slot-value the-rule 'name) name)
    (loop
       :with top-model-resolver = #'(lambda (name)
                                      (cond
                                        ((string-equal name "object") (rule-model the-rule))
                                        (t (find-model name))))
       :for (dec value) :in declarations :do
       (alexandria:switch (dec :test #'string-equal)
         ("comment" (setf (rule-comment the-rule) value))
         ("access" (setf (rule-mode the-rule)
                         (cond
                           ((string-equal value "allow") :allow)
                           ((string-equal value "deny") :deny)
                           (t (error 'malformed-statement-error
                                     :text (format nil "Unknown access type '~A' found." value))))))
         ("model"
          (setf (rule-model the-rule) (find-model value)))
         ("grantees"
          (loop :for (grantee-type grantee-name . parameters) :in (list value) :do
             (alexandria:switch ((string-downcase grantee-type) :test #'string=)
               ("user" (push (rule-users the-rule) grantee-name))
               ("role"
                (print parameters)
                (push (cons (find-role grantee-name)
                            (mapcar #'(lambda (p)
                                        (make-access-path (format nil "~A" p) top-model-resolver))
                                    parameters))
                      (rule-roles the-rule)))
               (t (error 'malformed-statement-error
                         :text "Unexpected grantee type.")))))
         ("operations" (setf (rule-operations the-rule) value))
         ("constraint"
          (setf (rule-constraint the-rule)
                (parse-constraint value top-model-resolver)))
         (t (error 'malformed-statement-error
                   :text (format nil "Unknown rule declaration '~A' found." dec)))))
    the-rule))


(defun parse-statment (statement)
  "Parse STATMENT and return corresponding object. Raise
`parsing-error' or `malformed-statement-error' condition if statment
can not be parsed."
  (let ((command-name (first statement)))
    (alexandria:switch ((string-downcase command-name) :test #'string=)
      ("defrole" (parse (make-role) statement))
      ("defmodel" (parse (make-model) statement))
      ("rule" (parse (make-acl-rule) statement))
      (t (error 'parsing-error
                :text (format nil "Unknown clause '~A' found." command-name))))))
