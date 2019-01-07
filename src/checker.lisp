;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :cl-user)

(defpackage secsrv.checker
  (:nicknames :checker)
  (:use :cl :secsrv.policy :secsrv.sys)
  (:import-from :cl-log
                #:log-message)
  (:import-from :secsrv.policy
                #:<constraint>
                #:<access-path>
                #:<concept>
                #:find-concept)
  #+(or)(:import-from :secsrv
                      #:user-id-by-name
                      #:evaluate-object-related-query)
  (:export #:<checker>
           #:<object>
           #:object-id
           #:object-entity
           #:has-access
           #:list-operations
           #:with-checker
           #:clear-statistics))

(in-package :secsrv.checker)


(defgeneric has-access (checker user model object operation)
  (:documentation "Verify that access may be granted for request under
  the `policy' and database associated to CHECKER."))

(defgeneric list-operations (checker user model object)
  (:documentation "Find all operations that the USER is granted for
  OBJECT of class MODEL under access control `policy' associated to
  CHECKER."))

(defclass <checker> ()
  ((policy
    :type <policy>
    :initarg :policy
    :initform (error "Must specify a valid policy for a checker.")
    :accessor checker-policy)
   (database-connection
    :initform nil
    :initarg :connection
    :accessor checker-connection
    :documentation "Current database connection. NIL if not connected."))
  (:documentation "All information required for access control checks,
  including the `policy' and database connection."))

(defclass <object> ()
  ((entity :type <entity> :initarg :entity :accessor object-entity)
   (id :type '(or integer string) :initarg :id :accessor object-id)))

(defclass <request> ()
  ((user-name :type string :initarg :user-name :accessor request-user)
   (object :type <object> :initarg :object :accessor request-object)
   (operation :type string :initarg :operation :accessor request-operation)
   (cache
    :initform (cl-containers:make-container 'containers:simple-associative-container
                                            :test #'equal)
    :accessor request-cache
    :documentation "Hash table for storing all SQL results computed
    during the evaluation of this request."))
  (:documentation "A collection of values representing user's request."))

(defun make-access-request (user-name entity object-id operation)
  (make-instance '<request>
                 :user-name user-name
                 :object (make-instance '<object> :id object-id :entity entity)
                 :operation operation))

(defgeneric request-object-id (request)
  (:method ((request <request>))
    (object-id (request-object request))))

(defgeneric request-entity (request)
  (:method ((request <request>))
    (object-entity (request-object request))))


(defun make-checker (&key (policy *current-policy*) connection)
  "Create an ACL checker associated with a database CONNECTION and `<policy>' POLICY."
  (make-instance '<checker> :policy policy :connection connection))


(defmacro with-checker ((checker-var connection-maker
                                     &key (policy '*current-policy*)
                                     reuse-database-connection)
                        &body body)
  "Evaluate BODY with the checker assigned to CHECKER-VAR. Connect and
disconnect the checker to the database using connection created by
CONNECTION-MAKER, if database connection is not provided by th
REUSE-DATABASE-CONNECTION keyword."
  `(let ((,checker-var (make-checker :policy ,policy)))
     (with-accessors ((connection checker-connection))
         ,checker-var
       (setf *dbcon*
             (or ,reuse-database-connection
                 (funcall ,connection-maker)))
       (unwind-protect
            (progn ,@body)
         (unless ,reuse-database-connection
           (dbi:disconnect *dbcon*))))))

(defun resolve-relational-operation (operation)
  (alexandria:eswitch (operation :test #'string=)
    ("=" #'=)
    ("<" #'<)
    (">" #'>)
    ("<=" #'<=)
    (">=" #'>=)
    ("!=" #'/=)))


(defun simple-access-path-p (access-path)
  (= (length (access-path-items access-path)) 1))

(defgeneric simple-operand-p (operand)
  (:method ((operand <access-path>))
    (and
     (= (length (access-path-items operand)) 1)
     (null (ap-item-constraint (first (access-path-items operand))))))
  (:method ((operand string))
    (declare (ignore operand))
    t)
  (:method ((operand number))
    (declare (ignore operand))
    t)
  (:method ((operand <constraint>))
    (declare (ignore operand))
    nil))


(defun logical-operation-p (operation)
  (member operation '("and" "or" "not") :test #'string-equal))

(defun unary-logical-operation-p (operation)
  (member operation '("not" "exists") :test #'string-equal))

(defun comparing-operation-p (operation)
  (member operation '("=" "<" ">" "<=" ">=" "!=") :test #'string-equal))

(defun quantifier-operation-p (operation)
  (member operation '("some" "every" "the" "count") :test #'string-equal))

(defun access-path-p (arg)
  (subtypep (type-of arg) '<access-path>))

(defun constraint-p (arg)
  (subtypep (type-of arg) '<constraint>))

(defun quantified-access-path-p (arg)
  (and (constraint-p arg)
       (quantifier-operation-p (constraint-operation arg))
       (access-path-p (constraint-first-argument arg))
       (= 1 (length (constraint-arguments arg)))))


(defun simple-constraint-p (constraint)
  "A constraint is simple, if it refers to direct attributes
only. Effectively, simple constraints are just logical expressions
over entity attributes."
  (cond
    ((logical-operation-p (constraint-operation constraint))
     (every #'simple-constraint-p (constraint-arguments constraint)))
    ((quantifier-operation-p (constraint-operation constraint))
     nil)
    ((comparing-operation-p (constraint-operation constraint))
     (every #'simple-operand-p (constraint-arguments constraint)))
    (t (error "Unknown constraint operation"))))


(defun simple-operand-expr (operand table-alias)
  (assert (simple-operand-p operand) (operand) "Operand is not simple!")
  (etypecase operand
    (<access-path>
     (format nil "~A.~(~A~)"
             table-alias
             (entity-attribute-column (ap-item-attribute (first (access-path-items operand))))))
    (string (format nil "'~A'" operand))
    (number operand)))

(declaim (ftype function access-path->sql))

(defun operand-sql-expression (operand table-alias &key initial)
  (cond
    ((simple-operand-p operand)
     (simple-operand-expr operand table-alias))
    ((typep operand '<access-path>)
     (format nil "(~A)"
             (access-path->sql operand
                               :initial (or initial (error "Initial attribute not specified where expected."))
                               :min-seq (+ 100 (random 10000)))))
    (t (error "Unsupported operand"))))

(defun ap-constraint->sql (constraint table-alias initial-record)
  "For the given access path CONSTRAINT build the expression to be
appended to WHERE clause of an SQL query. TABLE-ALIAS defines the
name used in the higher level SQL query for addressing a table, where
CONSTRAINT's attributes appear. INITIAL-RECORD is the name of an SQL
query attribute representing the particular record of
TABLE-ALIAS. Used if constraint contains access-path expressions."
  (check-type constraint <constraint>)
  (cond
    ((logical-operation-p (constraint-operation constraint))
     (if (unary-logical-operation-p (constraint-operation constraint))
         (format nil "(~:@(~A~) ~A)" ; capitalize logical connector
                 (constraint-operation constraint)
                 (ap-constraint->sql (constraint-first-argument constraint) table-alias initial-record))
         (format nil "(~A ~:@(~A~) ~A)" ; capitalize logical connector
                 (ap-constraint->sql (constraint-first-argument constraint) table-alias initial-record)
                 (constraint-operation constraint)
                 (ap-constraint->sql (constraint-second-argument constraint) table-alias initial-record))))
    ((comparing-operation-p (constraint-operation constraint))
     (format nil "(~A ~A ~A)"
             (operand-sql-expression (constraint-first-argument constraint) table-alias :initial initial-record)
             (constraint-operation constraint)
             (operand-sql-expression (constraint-second-argument constraint) table-alias :initial initial-record)))
    (t "")))

;;--- FIXME: check that start-concept passed to/from mutualy recursive calls correctly.
(defun access-path->sql (access-path &key initial (min-seq 0) start-concept)
  "Generate SQL query that selects attribute specified by the
`ACCESS-PATH'. Access-path starts at START-CONCEPT. The result is a
FORMAT template string with ~A placeholder in place of top level
object's identifier.
"
  (check-type access-path <access-path> "not a valid access-path")
  (assert (not (null (access-path-concept access-path))))
  #+(or)(log-message :info "Generating SQL query for ~A using initial concept ~A."
               (access-path-expression access-path)
               (concept-name (access-path-concept access-path)))
  (let ((parsed-path (or (access-path-items access-path)
                         (verify-access-path-expression
                          (or start-concept (access-path-concept access-path))
                          (access-path-expression access-path))))
        sql-select sql-from sql-where joins)
    (loop
       :initially
       (let ((first-entity (attribute-concept (ap-item-attribute (first parsed-path)))))
         (push (format nil " T~D.~A = ~A"
                       min-seq
                       (entity-attribute-column (entity-primary-key first-entity))
                       (or initial "~A"))
             sql-where)
         (setf sql-from (format nil "FROM ~A T~D " (entity-table first-entity) min-seq)))
       :for items-remained :on parsed-path
       :for ap-item = (first items-remained)
       :for attribute = (ap-item-attribute ap-item) :and constraint = (ap-item-constraint ap-item)
       :and previous-attribute = nil :then attribute
       :for current-model = (attribute-concept attribute)
       :for seq :from min-seq
       :for current-alias = (format nil "T~D" seq) :and next-alias = (format nil "T~D" (1+ seq))
       :when (< min-seq seq)
       :do
       (push (format nil " JOIN ~A T~D ON (T~D.~A=T~D.~A) "
                     ;; joined table and it's alias
                     (entity-table current-model) seq
                     ;; join column
                     seq
                     (if (entity-attribute-reverse-attribute previous-attribute)
                         (entity-attribute-column (entity-attribute-reverse-attribute previous-attribute))
                         (entity-attribute-column (entity-primary-key current-model)))
                     ;; join column in the previous table
                     (1- seq) (entity-attribute-column previous-attribute))
             joins)
       :do
       (when (and constraint items-remained)
         (let ((foreign-key-name (format nil "~A.~A" current-alias (entity-attribute-column attribute))))
           (push (ap-constraint->sql constraint next-alias foreign-key-name)
                 sql-where)))
       :finally
       (setf sql-select
             (format nil "SELECT T~D.~A "
                     (if (rest parsed-path) seq 0)   ; Use T0 if no other tables were joined
                     (entity-attribute-column (or attribute (first parsed-path))))))
    ;; Compose the resulting query
    (let ((sql (concatenate 'string
                            sql-select
                            sql-from
                            (apply #'concatenate 'string (reverse joins))
                            (format nil " WHERE ~{~A~^ AND~}" sql-where))))
      #+(or)(log-message :trace "~&*********************************~%~A~%*********************************~%" sql)
      sql)))


(defun evaluate-access-path (access-path object &optional object-concept)
  "Evaluates SQL query defined by the ACCESS-PATH starting from the OBJECT."
  (let* ((ap-expression (access-path-expression access-path))
         (qualifier (access-path-qualifier ap-expression)))
    (cond
      ((string-equal qualifier "user")
       (let ((user-attributes-names (cdr (mapcar #'first (cdr ap-expression)))))
         (alexandria:switch ((first user-attributes-names) :test #'string-equal)
           ("username" (request-user *current-request*))
           ("man_id" (secsrv::worker-id-by-user-name (request-user *current-request*)))
           ("id" (secsrv::user-id-by-name (request-user *current-request*)))
           ;;--- TODO: make user entity a confifurable parameter
           (t (let ((user-id (secsrv::user-id-by-name (request-user *current-request*)))
                    (user-entity (find-concept "user")))
                (secsrv::evaluate-object-related-query
                 (access-path->sql access-path :start-concept user-entity)
                 (make-instance '<object> :id user-id :entity user-entity)))))))
      (t (secsrv::evaluate-object-related-query
          (access-path->sql access-path :start-concept object-concept) object)))))

(defgeneric relational-argument-value (argument object object-concept)
  (:documentation "Returns value of relation's ARGUMENT. Argument
could be a constant, or an `<aceess-path>'. The returned value is
always a list, regardless of the result's actual cardinality.")
  (:method ((argument <access-path>) (object <object>) (object-concept <concept>))
    (evaluate-access-path argument object object-concept))
  (:method ((argument <constraint>) (object <object>) (object-concept <concept>))
    #+debug(assert (quantified-access-path-p argument))
    (evaluate-access-path (constraint-first-argument argument) object object-concept))
  (:method (argument (object <object>) object-concept)
    (declare (ignore object object-concept))
    #+debug(check-type argument (or number string))
    argument))

(defun first-and-only (v)
  (cond
    ((listp v)
     (when (cdr v)
       (log-message :error "first-and-only: non-singleton list supplied: ~A" v))
     (car v))
    ((atom v) v)
    (t (log-message :error "first-and-only: unexpected argument ~A" v) nil)))

(defun compare-values (operation quantifiers values)
  "Compare VALUES using relational OPERATION taking QUANTIFIERS into account."
  (assert (= (length quantifiers) (length values)))
  (when (and values (every #'identity values))
    (labels ((op (x y)
               (when (and x y)
                 (funcall (resolve-relational-operation operation) x y)))
             (cmp (quantifiers values &optional (val nil val-supplied))
               (if (null values)
                   t
                   (alexandria:eswitch ((or (first quantifiers) "the") :test #'string-equal)
                     ("the" (and (or (not val-supplied) (op val (first-and-only (first values))))
                                 (cmp (rest quantifiers) (rest values) (first-and-only (first values)))))
                     ("count" (and (or (not val-supplied)
                                       (op val (length (first values))))
                                   (cmp (rest quantifiers) (rest values) (length (first values)))))
                     ("some" (loop
                                :for x :in (first values)
                                :when (and (or (not val-supplied) (op val x))
                                           (cmp (rest quantifiers) (rest values) x))
                                :do (return t)))
                     ("every" (not (loop
                                      :for x :in (first values)
                                      :when (not (and (or (not val-supplied) (op val x))
                                                      (cmp (rest quantifiers) (rest values) x)))
                                      :do (return t))))))))
      (cmp quantifiers values))))

(defun match-constraint (constraint object object-concept)
  "Check that `<CONSTRAINT>' is satisfied by the OBJECT, assuming it is
an instance of OBJECT-CONCEPT."
  (with-accessors ((entity object-entity)
                   (id object-id))
      object
    (log-message :trace "Checking constraint ~A for object=(~A ~A) as an instance of ~A."
                 (constraint-expression constraint)
                 (concept-name (object-entity object))
                 (object-id object)
                 (concept-name object-concept))
    (with-accessors ((operation constraint-operation)
                     (arg-1 constraint-first-argument)
                     (arg-2 constraint-second-argument)
                     (args constraint-arguments))
        constraint
      (cond
        ;;--- TODO: check other permission; add (has-permission...) clause to the language
        ((logical-operation-p operation)
         ;; boolean connectors
         (alexandria:eswitch (operation :test #'string-equal)
           ("and" (and (match-constraint arg-1 object object-concept) (match-constraint arg-2 object object-concept)))
           ("or" (or (match-constraint arg-1 object object-concept) (match-constraint arg-2 object object-concept)))
           ("not" (not (match-constraint arg-1 object object-concept)))
           ("exists" (not (secsrv::empty-query-result-p (access-path->sql arg-1) object)))))
        ((comparing-operation-p operation)
         ;; relations
         (let ((quantifiers (mapcar #'(lambda (arg)
                                        (when (quantified-access-path-p arg)
                                          (constraint-operation arg)))
                                    args))
               (args-values (mapcar #'(lambda (arg)
                                        (relational-argument-value arg object object-concept))
                                    args)))
             (log-message :debug "Comparing (~A ~@[~A~]~A ~A)"
                          operation (first quantifiers) (first args-values) (second args-values))
           (let ((cmp-result (compare-values operation quantifiers args-values)))
             #+(or)(log-message :debug "Comparing (~A ~A ~A) ==> ~A."
                          operation (first args-values) (second args-values) cmp-result)
             (log-message :debug "   ==> ~A." cmp-result)
             cmp-result)))))))


(defun find-matching-concepts (object)
  "Walk the concept's hierarchy tree starting at OBJECT's `<entity>' and find
all concepts that the object belongs to. Returns a list of `<concept>'s."
  (generators:make-generator ()
    (with-accessors ((entity object-entity)
                     (object-id id))
        object
      (labels ((match (concept)
                 (or (null (concept-constraint concept))
                     (match-constraint (concept-constraint concept) object (object-entity object))))
               (tree-search (concept)
                 (when (match concept)
                   (generators:yield concept)
                   (loop :for sub-concept :in (ho-children concept)
                      :do (tree-search sub-concept)))))
      (tree-search entity)
      (generators:yield nil)
      t))))


(defun match-rule-by-concepts (user object-concepts operation rule)
  "Check that the `RULE' matches the access request specified by the USER,
CONCEPT, and OPERATION. Rule's constraint is not evaluated. Returns T or NIL."
  (with-accessors ((granted-users rule-users)
                   (granted-roles rule-roles)
                   (rule-concept rule-concept)
                   (rule-operations rule-operations)
                   (rule-constraint rule-constraint))
      rule
    (let* ((matching-concepts
            ;; The list of the most specific object concepts in a subtree rooted at rule-concept
            (delete-if-not (let ((sc (ho-walk rule-concept)))
                             #'(lambda (obj-c)
                                 (member obj-c sc)))
                           (sort object-concepts #'subtypep :key #'class-of)))
           (treat-object-as (car (last matching-concepts))))
      (and
       ;; This rule is defined for the right concept and operation
       (not (null matching-concepts))
       (member operation rule-operations :test #'string-equal)
       ;; Subject should belongs to one of the grantees, if the rule defines any
       (or (and (not granted-users) (not granted-roles))
           (and granted-users (member user granted-users :test #'string=))
           ;; User is a member of one of parameterized roles, if any
           (some #'(lambda (grantee-role)
                     (apply #'secsrv::user-has-role user (car grantee-role)
                            (mapcar #'(lambda (ap)
                                        (evaluate-access-path ap (request-object *current-request*)
                                                              treat-object-as))
                                    (rest grantee-role))))
                 granted-roles))))))

(defun match-rule (user object-concepts operation rule)
  "Check that the `RULE' matches the access request specified by the USER,
CONCEPT, and OPERATION. Returns T or NIL."
  (with-accessors ((granted-users rule-users)
                   (granted-roles rule-roles)
                   (rule-concept rule-concept)
                   (rule-operations rule-operations)
                   (rule-constraint rule-constraint))
      rule
    (log-message :trace "--------> Checking rule ~A <-------------" (rule-name rule))
    (log-message :debug "rule-concept=~A, object concepts=~{~A~^, ~}"
                 (concept-name rule-concept)
                 (mapcar #'concept-name object-concepts))
    (let* ((matching-concepts
            ;; The list of the most specific object concepts in a subtree rooted at rule-concept
            (delete-if-not (let ((sc (ho-walk rule-concept)))
                             #'(lambda (obj-c)
                                 (member obj-c sc)))
                           (sort object-concepts #'subtypep :key #'class-of)))
           (treat-object-as (car (last matching-concepts)))
           (matching-result
            (and
             ;; This rule is defined for the right concept and operation
             (not (null matching-concepts))
             (member operation rule-operations :test #'string-equal)
             ;; Subject should belongs to one of the grantees, if the rule defines any
             (or (and (not granted-users) (not granted-roles))
                 (and granted-users (member user granted-users :test #'string=))
                 ;; User is a member of one of parameterized roles, if any
                 (some #'(lambda (grantee-role)
                           (apply #'secsrv::user-has-role user (car grantee-role)
                                  (mapcar #'(lambda (ap)
                                              (evaluate-access-path ap (request-object *current-request*)
                                                                    treat-object-as))
                                          (rest grantee-role))))
                       granted-roles))
             ;; Rule's additional constraint is satisfied
             (or (not rule-constraint)
                 (match-constraint rule-constraint
                                   (request-object *current-request*)
                                   treat-object-as)))))
      (log-message :trace "--------> RESULT for checking rule ~A on ~A is ~A."
                   (rule-name rule) (and treat-object-as (concept-name treat-object-as)) matching-result)
      matching-result)))

(defparameter *rule-statistics* (containers:make-container 'containers:simple-associative-container))

(defclass <rule-usage-statistics> ()
  ((hits :type integer :initform 0 :accessor stats-hits)
   (decisions :type integer :initform 0 :accessor stats-decisions)
   (total-time :type double-float :initform 0d0 :accessor stats-time)))

(defun update-rule-stats (rule time deciding-p)
  (let* ((stats (containers:item-at *rule-statistics* rule))
         (local-stats (or stats (make-instance '<rule-usage-statistics>))))
    (incf (stats-hits local-stats))
    (incf (stats-time local-stats) time)
    (when deciding-p (incf (stats-decisions local-stats)))
    (when (null stats)
      (setf (containers:item-at *rule-statistics* rule) local-stats))
    t))

(defun rule-usefulness (rule)
  "Returns a number from 0 to infinity. The higher it is, the earlier the
  rule should be sorted."
  (let ((stats (containers:item-at *rule-statistics* rule)))
    (if (and stats (< 0 (stats-hits stats)))
        (/ (stats-decisions stats) (stats-time stats))
        0)))

(defun clear-statistics ()
  (setf *rule-statistics* (containers:make-container 'containers:simple-associative-container))
  t)

(defmethod has-access ((checker <checker>) (user string) (entity-name string) (object-id integer) (operation string))
  "Check that USER has permission to evaluate OPERATION on object of
`model' MODEL with id OBJECT-ID.

Return multiple values:
   access status T or NIL,
   elapsed time (in seconds),
   number of SQL queries processed."
  (log-message :trace "****** Checking access for ~A of ~A, id = ~S ******"
               operation entity-name object-id)
  (let* ((start-time (get-internal-real-time))
         (*sql-query-count* 0)
         (policy (checker-policy checker))
         (entity (find-concept entity-name :policy policy))
         (*current-request* (make-access-request user entity object-id operation))
         (concepts-generator (find-matching-concepts (request-object *current-request*)))
         (concepts (loop :for concept = (generators:next concepts-generator) :while concept :collect concept))
         (operation (string-downcase operation)))
    (log-message :debug "Object belongs to ~A concepts." (mapcar #'concept-name concepts))
    (let* ((matched-rules
            (delete-if-not #'(lambda (r)
                               (match-rule-by-concepts user concepts operation r))
                           (policy-rules policy)))
           (access-check-result
            (loop
               :for positive-rule-found = nil :then (or positive-rule-found (and positive-rule-p match-p))
               :for rule :in (sort matched-rules #'> :key #'rule-usefulness)
               :for positive-rule-p = (eq (rule-mode rule) :allow)
               :for (time skipped match-p) = (let* ((skip (and positive-rule-found positive-rule-p))
                                                    (start-time (get-internal-real-time))
                                                    (result (or skip (match-rule user concepts operation rule))))
                                               (list (/ (- (get-internal-real-time) start-time) internal-time-units-per-second)
                                                     skip
                                                     result))
               :when (not skipped)
               :do (update-rule-stats (car matched-rules) time match-p)
               :when (and (not positive-rule-p) match-p) :do (return nil)
               :finally (return positive-rule-found))))
      (log-message :info "~&*** Access check (~:[DENIED~;ALLOWED~]) took ~F seconds and ~D SQL queries. ***~%"
                   access-check-result
                   (/ (- (get-internal-real-time) start-time) internal-time-units-per-second)
                   *sql-query-count*)
      (values access-check-result
              (/ (- (get-internal-real-time) start-time) internal-time-units-per-second)
              *sql-query-count*))))
