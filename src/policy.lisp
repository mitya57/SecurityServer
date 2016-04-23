;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;;
;;;; CBAC policy classes.
;;;;


(in-package :cl-user)

(defpackage secsrv.policy
  (:nicknames :policy)
  (:use :cl :secsrv.sys)
  (:import-from :cl-log
                #:log-message)
  (:import-from :containers
                #:find-item
                #:item-at)
  (:import-from #:alexandria
                #:define-constant
                #:when-let)
  (:export
   ;; every class or function is exported by the code at the end of this file
   )
)

(in-package :secsrv.policy)

;;;
;;; Classes
;;;


(defclass <hierarchical-object> ()
  ((children
    :type (list-of-type <hierarchical-object>)
    :initform nil
    :reader ho-children
    :documentation "Auto-maintained list of all direct children.")
   (parents
    :type (list-of-type <hierarchical-object>)
    :initarg :parents
    :initform nil
    :accessor ho-parents)))

(defgeneric ho-add-parent (object parent)
  (:documentation "Adds another parent to the hierarchical object OBJECT.")
  (:method ((object <hierarchical-object>) parent)
    ;; Check that OBJECT and PARENT belong to one hierarchy
    (assert (or (subtypep (class-of object) (class-of parent))
                (subtypep (class-of parent) (class-of object))))
    (pushnew object (slot-value parent 'children))
    (pushnew parent (slot-value object 'parents))))

(defmethod (setf ho-parents) :around (new-value (object <hierarchical-object>))
  (loop :for parent :in (slot-value object 'parents)
     :do (delete object (slot-value parent 'children)))
  (loop :for parent :in new-value
     :do (ho-add-parent object parent))
  (call-next-method))

(defmethod initialize-instance :after ((object <hierarchical-object>) &key)
  (loop :for parent :in (slot-value object 'parents)
     :do (ho-add-parent object parent)))


(defgeneric ho-walk (hierarchical-object &optional function &key direction)
  (:documentation "Walks the hierarchy starting at OBJECT in the
  specified DIRECTION. If FUNCTION is not nil then it is a function
  with one argument to be called on every node. The method returns the
  list of nodes visited. DIRECTION could be either :SUB-CONCEPTS
  or :SUPER-CONCEPTS.")
  (:method ((object <hierarchical-object>) &optional function &key (direction :sub-concepts))
    (loop :for direct-subconcept :in (ecase direction
                                       (:sub-concepts (ho-children object))
                                       (:super-concepts (ho-parents object)))
       :when function :do (funcall function direct-subconcept)
       :nconc (cons direct-subconcept (ho-walk direct-subconcept function :direction direction)))))

(defgeneric ho-ascendant-p (object parent &key test)
  (:documentation "Check that parent and object are connected by a chain.")
  (:method ((object <hierarchical-object>) (parent <hierarchical-object>) &key (test #'eq))
    (member parent (ho-walk object nil :direction :super-concept) :test test)))


(defclass <policy> (<hierarchical-object>)
  ((rules :initform nil
          :accessor policy-rules
          :documentation "List of all policy rules.")
   (roles :initform nil
          :accessor policy-roles
          :documentation "List of all roles.")
   (concepts
    :initform (containers:make-container 'containers:simple-associative-container :test #'equal)
    :accessor policy-concepts
    :documentation "List of all database models."))
  (:documentation "Policy is a collection of concepts, roles, and access control rules."))

(defclass <concept> (<hierarchical-object>)
  ((name :type string
         :initarg :name
         :reader concept-name)
   (docstring :type (or null string)
              :initform nil
              :initarg :docstring
              :reader concept-docstring)
   (attributes
    :initform (containers:make-container 'containers:simple-associative-container :test #'equal)
    :accessor concept-attributes
    :documentation "Set of named attributes defined at this
    class. Derived attributes are not included.")
   (excluded-attributes
    :initarg :excluded-attributes
    :initform (containers:make-container 'containers:set-container :test #'equal)
    :reader concept-excluded-attributes
    :documentation "Set of excluded attributes names.")
   (constraint
    :type (or null <constraint>)
    :initarg :constraint
    :initform nil
    :reader concept-constraint
    :documentation "Filtering constraint as a LISP-like list. First
    item of the list or sublist is the node type, which is
    either :OPERATOR, or :CONST, or :ACCESS-PATH. For example:

        (:OPERATOR \"<=\" (:ACCESS-PATH (\"journal\")) (:CONST . 0))"))
  (:documentation "Generic notion of a Concept."))

(defgeneric concept-constraint-expression (concept)
  (:documentation "Direct access to constraint expression embedded in constraint instance."))

(defgeneric find-attribute (concept attribute-name)
  (:documentation "Find attribute in the CONCEPT or its base concepts."))

(defclass <entity> (<concept>)
  ((table-name
    :type string
    :initarg :table-name
    :accessor entity-table
    :documentation "Name of the database table that contains instances of this class.")
   (primary-key
    :type (or null <entity-attribute>)
    :accessor entity-primary-key
    :initform nil
    :documentation "Reference to an attribute representing the primary key."))
  (:documentation "Description of a database table and its
  relations. Can not be derived from another <entity>, directly or
  indirectly."))


(defclass <role> ()
  ((name
    :initarg :name
    :initform (error "Must specify a name for the role.")
    :accessor role-name
    :documentation "Name of the role.")
   (parameters
    :type (list-of-type <concept>)
    :initarg :parameters
    :initform nil
    :reader role-parameters
    :documentation "Accepted parameters. List of `<concept>'s."))
  (:documentation "Parameterized access control role."))

;;;
;;; Rules
;;;
(defclass <rule> ()
  ((name
    :type (or string null)
    :initarg :name
    :initform nil
    :accessor rule-name)
   (mode
    :type '(member :allow :deny)
    :accessor rule-mode
    :initarg :mode)
   (concept
    :type <concept>
    :initarg :concept
    :accessor rule-concept
    :initform nil)
   (operations
    :type (list-of-type string)
    :initarg :operations
    :accessor rule-operations
    :documentation "List of operations affected by the rule.")
   (roles
    :initarg :roles
    :accessor rule-roles
    :initform nil
    :documentation "List of roles associated with this rule. Each role
    is a list such that its CAR is the name of role, and CDR is a list
    of `<access-path>' objects (parsed expressions).")
   (users
    :initarg :users
    :accessor rule-users
    :initform nil
    :documentation "List of users assigned to this rule.")
   (constraint
    :type (or <constraint> null)
    :initarg :constraint
    :accessor rule-constraint
    :initform nil)
   (comment
    :type (or string null)
    :initform nil
    :accessor rule-comment))
  (:documentation "Access control rule."))

(defclass <attribute> ()
  ((name
    :type string
    :initarg :name
    :accessor attribute-name)
   (concept
    :type <concept>
    :initarg :concept
    :reader attribute-concept
    :documentation "Reference to the concept that defines this attribute."))
  (:documentation "Generic class representing concept's named attribute."))

(defclass <entity-attribute> (<attribute>)
  ((column-name
    :type string
    :initarg :column-name
    :accessor entity-attribute-column)
   (data-type :initarg :data-type)
   (target-entity
    :type (or <entity> null)
    :initform nil
    :initarg :target-entity
    :accessor entity-attribute-target
    :documentation "If the attribute is a foreign key, this is a referenced entity.")
   (m2m-entity
    :type (or <entity> null)
    :initarg :m2m-entity
    :initform nil
    :accessor entity-attribute-m2m-entity
    :documentation "If the attribute is a shortened m2m-key, this is a
    referenced to the entity representing the relation.")
   (reverse-attribute
    :type (or null <entity-attribute>)
    :initarg :reverse-attribute
    :accessor entity-attribute-reverse-attribute
    :initform nil
    :documentation "If non NIL, this entity is referenced from
    target-entity and this attribute represents 'reverse' foreign
    keys. Reverse-attribute is the foreign key attribute in
    target-entity."))
  (:documentation "Application level object's attribute, basically, a database
  column."))

(defclass <relational-attribute> (<attribute>)
  ((access-path
    :type <access-path>
    :reader attribute-access-path
    :initarg :access-path))
  (:documentation "Access-path defined attribute."))

;;;
;;; Constraints and Access paths
;;;

(defclass <constraint> ()
  ((expression
    :type list
    :initarg :expression
    :reader constraint-expression
    :documentation "Tree representation of a boolean expression as a
    LISP-like list. At the top level, it is a list of the
    form (:operator operation arg1 [arg2]), where :operator is
    predefined header, operation is a string. If the operation is a
    boolean connector (and, or, nor), then the arguments are
    subexpression of the same :operator form. If the operation is a
    relation, then each arg is an atomic expression, i.e. constant,
    access-path, or an application of set-function to an access-path.
    First item of the atomic expression is either :ACCESS-PATH,
    or :CONST. For example:

        (:OPERATOR \"<=\" (:ACCESS-PATH (\"journal\")) (:CONST . 0))

    Operations are: <, >, =, <=, >= etc.")
   (operation
    :type string
    :reader constraint-operation
    :initarg :operation)
   (arguments
    :type list ;;(list-of-type '(or <access-path> <constraint> string number))
    :reader constraint-arguments
    :initarg :arguments)
   (first-argument
    :type '(or <access-path> <constraint> string number)
    :initarg :first-arg
    :reader constraint-first-argument
    :documentation "Direct link to the first item of arguments list.")
   (second-argument
    :type '(or <access-path> <constraint> string number null)
    :initarg :second-arg
    :reader constraint-second-argument
    :documentation "Direct link to the second argument, if any."))
  (:documentation ""))

(defclass <simple-constraint> (<constraint>)
  ()
  (:documentation "Constraint that refers to direct entity's
  attributes only, i.e. arbitrary logical expression on attributes
  that do not jumping to anothe entities. Simple constraints might map
  to WHERE conditions of an SQL query."))

(defclass <access-path> ()
  ((main-concept
    :type <concept>
    :initarg :concept
    :reader access-path-concept
    :documentation "The `<concept>' of objects this access path may be applied to.")
   (target-attribute
    :type (or attribute null)
    :reader access-path-target-attribute
    :documentation "The last `attribute' selected by the expression.")
   (cardinality
    :type (member :single :set)
    :reader access-path-cardinality
    :initform :set
    :documentation "How many values are expected to be addressed by
    the access path expression. Essentially, if reverse foreign keys
    are traversed along the path, the cardinality is :SET")
   (expression
    :initarg :expression
    :reader access-path-expression
    :documentation "Parsed representation of access path. List of
    conses (name-string . filtering-expression), prefixed
    by :ACCESS-PATH keyword. Filtering expression is a tree structure
    defined by expression slot of `<constraint>'.")
   (items
    :type (list-of-type <access-path-item>)
    :initform nil
    :reader access-path-items
    :documentation "Bound representation of the access path. Each
    subexpression, :access-path or :operation, is substituted by
    instances of <constraint> or <access-path>. Items changes when
    main-concept changes, bacause different concepts might have
    attributes with same names, and starting concept determine the
    specific chain of attributes.")
   (prepared-sql
    :reader access-path-prepared-sql
    :initform nil
    :documentation "Prepared or pregenerated, if the underling backend
    does not support prepared statements, SQL statement for this
    access path."))
  (:documentation ""))

(defclass <access-path-item> ()
  ((attribute
    :type <entity-attribute>
    :reader ap-item-attribute
    :initarg :attribute)
   (constraint
    :type (or <constraint> null)
    :initarg :constraint
    :reader ap-item-constraint))
  (:documentation "Atomic expression of the access path."))

(defmethod concept-constraint-expression ((concept <concept>))
  (when-let (constraint (concept-constraint concept))
    (constraint-expression constraint)))



(defmethod find-attribute ((concept <concept>) attribute-name)
  (unless (find-item (concept-excluded-attributes concept) attribute-name)
    ;; Attribute name was not explicitely excluded in the CONCEPT. Search for it.
    (let ((direct-attribute (item-at (concept-attributes concept) attribute-name)))
      (if (and (null direct-attribute) (ho-parents concept))
          (loop :for super :in (ho-parents concept)
             :for att = (find-attribute super attribute-name)
             :when att :do (return att))
          direct-attribute))))

(defun find-concept (name &key (policy *current-policy*))
  "Find a `<concept>' by it's NAME in a `<policy>' specified by POLICY
keyword. NAME is case-insensitive."
  (item-at (slot-value policy 'concepts) name))


(defun make-policy ()
  (make-instance '<policy>))

(defun make-constraint (expression operation arguments)
  (make-instance '<constraint>
                 :expression expression
                 :operation operation
                 :arguments arguments
                 :first-arg (first arguments)
                 :second-arg (second arguments)))

(defun make-access-path-item (attribute constraint)
  (make-instance '<access-path-item>
                 :attribute attribute
                 :constraint constraint))

(defun make-access-path (concept expression)
  (let ((access-path
         (make-instance '<access-path>
                        :concept concept
                        :expression expression)))
    (declaim (ftype function verify-access-path-expression))
    (handler-case
        (setf (slot-value access-path 'items)
              (verify-access-path-expression concept expression))
      (simple-error (c)
        (log-message :error "~A; Dynamic access path assumed." c)))
    access-path))

(defun make-concept (name &key super constraint)
  (make-instance '<concept>
                 :name name
                 :parents super
                 :constraint constraint))


(defun make-entity (name table-name &key super)
  (make-instance '<entity>
                 :name name
                 :table-name table-name
                 :parents super))

(defun add-concept (policy name concept)
  ;;(log-message :trace "Adding concept ~A to the policy~%" name)
  (setf (item-at (slot-value policy 'concepts) name) concept))



(defun find-role (name &key (policy *current-policy*))
  "Find a `role' by it's NAME in a `policy' specified by POLICY keyword."
  (find name
        (policy-roles policy)
        :test #'string-equal
        :key #'role-name))

(defun policy-add-role (policy role-name &rest parameters)
  (unless (find-role role-name :policy policy)
    (let ((role (make-instance '<role> :name role-name :parameters parameters)))
      (push role (policy-roles policy))
      role)))



(defun add-attribute (concept name attribute)
  (setf (item-at (slot-value concept 'attributes) name) attribute))


(defun make-entity-attribute (name entity column-name &key (reverse-attribute nil) primary-key-p)
  (let ((att (make-instance '<entity-attribute>
                            :name name
                            :concept entity
                            :column-name column-name
                            :reverse-attribute reverse-attribute)))
    ;;--- FIXME: move it to add method, or call add-attribute here?
    (when primary-key-p
      (setf (entity-primary-key entity) att))
    att))

(defun make-relational-attribute (name concept access-path-expression)
  (let ((access-path (make-access-path concept access-path-expression)))
    (make-instance '<relational-attribute>
                   :name name
                   :concept concept
                   :access-path access-path)))

(defun register-fk-relation (entity-attribute target-entity)
  "Marks ENTITY-ATTRIBUTE as a foreign key to TARGET-ENTITY and creates reverse attribute in TARGET-ENTITY."
  ;; Resolve target reference for foreign key attribute, and
  ;; add 'reverse' foreign key attribute to target model.
  (assert (subtypep (type-of target-entity) '<entity>))
  (when (not (entity-primary-key target-entity))
    (inconsistent-policy-error "No primary key found for entity `~A'. Required by the `~A.~A' foreign key attribute."
                               (concept-name target-entity)
                               (concept-name (attribute-concept entity-attribute))
                               (attribute-name entity-attribute)))
  (let* ((entity (attribute-concept entity-attribute))
         (reverse-fk-name (format nil "~As" (concept-name entity)))
         (reverse-fk-column-name
          (entity-attribute-column (entity-primary-key target-entity)))
         (back-attribute (make-entity-attribute reverse-fk-name target-entity reverse-fk-column-name)))
    ;; Mark attribute as FK
    (setf (entity-attribute-target entity-attribute) target-entity)
    ;; Add reverse attribute to TARGET-ENTITY
    (setf (entity-attribute-target back-attribute) entity
          (entity-attribute-reverse-attribute back-attribute) entity-attribute)
    (add-attribute target-entity reverse-fk-name back-attribute)))


(declaim (ftype function verify-access-path-expression)) ; forward declaration

(defun verify-filtering-constraint (concept constraint-expression)
  "Verify that CONSTRAINT-EXPRESSION forms a valid condition, i.e. only
accessable attributes are addressed by access path exressions. Returns an
instance of `<constraint>', or signal `inconsistent-policy-error' condition."
  (when constraint-expression
    (ecase (car constraint-expression)
      (:operator (destructuring-bind (operator-prefix operation . arguments)
                     constraint-expression
                   (declare (ignore operator-prefix))
                   (make-constraint constraint-expression
                                    operation
                                    (mapcar #'(lambda (arg)
                                                (verify-filtering-constraint concept arg))
                                            arguments))))
      (:const (cdr constraint-expression))
      (:access-path (make-access-path
                     concept
                     constraint-expression)))))

(defun access-path-qualifier (access-path-expression)
  (when access-path-expression
    (cond
      ((string-equal (caadr access-path-expression) "object") :object)
      ((string-equal (caadr access-path-expression) "user") :user)
      ((string-equal (caadr access-path-expression) "context") :context)
      (t nil))))

(defun drop-access-path-qualifier (access-path-expression)
  (destructuring-bind (ap-prefix (qualifier) . rest) access-path-expression
    (declare (ignore qualifier))
    (cons ap-prefix rest)))


(defun verify-access-path-expression (concept access-path-expr)
  "Verify that the ACCESS-PATH-EXPR forms a valid attributes chain
assuming the initial `<concept>' is CONCEPT. Return: list of
<access-path-item>s, or signals `inconsistent-policy-error' condition."
  (let ((access-path-expr (if (member (access-path-qualifier access-path-expr)
                                      '(:object :user))
                              (drop-access-path-qualifier access-path-expr)
                              access-path-expr)))
    (loop :with current-concept = concept
       :for (attribute-name . filtering) :in (cdr access-path-expr)
       :for att = (and current-concept (find-attribute current-concept attribute-name))
       :when (null current-concept) :do
       (inconsistent-policy-error "Attribute `~A' requested for atomic value."
                                  attribute-name)
       :when (null att) :do
       (inconsistent-policy-error "Unknown attribute `~A.~A'."
                                  (concept-name current-concept)
                                  attribute-name)
       ;; It current attribute is an access path then we should jump to
       ;; the last entity attribute adressed by the access path.
       ;;
       ;;--- FIXME: use full access path, not the first
       ;; attribute. This path should be appended to the result.
       :if (subtypep (type-of att) '<relational-attribute>)
       :do
       (destructuring-bind (ap-keyword (att-name . constraint) &rest items)
           (access-path-expression (attribute-access-path att))
         (declare (ignore ap-keyword constraint items))
         (setf att (find-attribute current-concept att-name))
         (when (null att)
           (inconsistent-policy-error "Attribute `~A' (substituted for the non-entity attribute `~A') does not exists defined for the concept `~A'."
                                      att-name
                                      attribute-name
                                      (concept-name current-concept))))
       :do
       (progn
         ;; jump to the next entity
         (setf current-concept (entity-attribute-target att))
         ;; Check that filtering constrinat is valid
         (when (and filtering (null current-concept))
           (inconsistent-policy-error "Filtering constraint on a non foreign key attribute `~A.~A'."
                                      (concept-name (attribute-concept att))
                                      attribute-name)))
       :collect (make-access-path-item att
                                       (if filtering
                                           (verify-filtering-constraint current-concept filtering)
                                           nil)))))

(defun set-concept-constraint (concept constraint-expression)
  (when-let (constraint (verify-filtering-constraint concept constraint-expression))
    (setf (slot-value concept 'constraint) constraint)))


(defun policy-add-create-rule (policy rule-name access-mode concept operations
                               &key granted-users granted-roles constraint)
  "Create new rule from the specified parameters and add it to the POLICY."
  (let ((rule (make-instance '<rule>
                             :name rule-name
                             :mode access-mode
                             :concept concept
                             :constraint (verify-filtering-constraint concept constraint)
                             :operations operations
                             ;;:roles granted-roles
                             :users granted-users)))
    ;; Check that granted roles match roles definitions (access paths select expected objects)
    (loop :for (role-name . access-paths) :in granted-roles
       :for role = (find-role role-name :policy policy)
       :if role
       :do
       (loop :for ap :in access-paths :and parameter-concept :in (role-parameters role)
          :do
          (log-message :debug "Checking granted role '~A' parameter ~A.~%" role-name (concept-name parameter-concept))
          ;;--- FIXME: implement some static access path checking (add target concept for generic attributes?)
          #+(or)(verify-access-path-expression concept ap)
          :finally (push (cons role (mapcar #'(lambda (ap-expr)
                                                (make-access-path concept ap-expr))
                                            access-paths))
                         (rule-roles rule)))
       :else :do
       (inconsistent-policy-error "Undefined role `~A' referenced in the rule `~A'."
                                  role-name
                                  rule-name))
    (push rule (policy-rules policy))
    rule))



(defmethod print-object ((the-policy <policy>) stream)
  "Pretty printing of policy objects."
  (format stream "{~% A POLICY with ~r role~:P, ~r model~:P, and ~r access rule~:p ~%}"
          (length (policy-roles the-policy))
          (containers:size (policy-concepts the-policy))
          (length (policy-rules the-policy))))


(eval-when (:compile-toplevel :load-toplevel)
  (let ((p (find-package :secsrv.policy)))
    (do-symbols (s p)
      (when (and (eql (symbol-package s) p)
                 (or (fboundp s)
                     (find-class s nil))
                 (not (or (string= "%" (string s) :end2 1))))
        ;;(format t "Exporting ~A~%" s)
        (export s)))))
