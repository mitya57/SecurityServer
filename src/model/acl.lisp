;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; Definitions of classes related to roles, rules and policy

(in-package :secsrv)

;;;
;;; Roles
;;;
(defclass role (acl-entity)
  ((name
    :initarg :name
    :initform (error "Must specify a name for the role.")
    :accessor role-name
    :documentation "Name of the role.")
   (parameters
    :initarg :parameters
    :initform nil
    :reader role-parameters
    :documentation "Accepted parameters. List of `model's."))
  (:documentation "Parameterized access control role."))

(defun make-role (&key (name nil) (parameters nil))
  (make-instance 'role :entity-name name :name name :parameters parameters))

;;;
;;; Constraints and Access paths
;;;
(defclass access-path (acl-entity)
  ((source
    :type string
    :reader access-path-source
    :documentation "Textual represntation of access-path expression.")
   (main-model
    :type model
    :reader access-path-model
    :documentation "The `model' of objects this access path may be applied to.")
   (target-attribute
    :type (or attribute null)
    :reader access-path-target-attribute
    :documentation "The last `attribute' selected by the expression.")
   (cardinality
    :type (member :single :set)
    :reader access-path-cardinality
    :initform :set
    :documentation "How many values may be addressed by the access
    path expression. Essentially, if reverse foreign keys are
    traversed along the path, the cardinality is :SET")
   (parsed
    :reader access-path-parsed
    :documentation "Parsed representation of access path, a list of `attribute's.")
   (prepared-sql
    :reader access-path-prepared-sql
    :initform nil
    :documentation "Prepared or pregenerated, if the underling backend
    does not support prepared statements, SQL statement for this
    access path."))
  (:documentation ""))

(defun make-access-path (&optional source first-model)
  (let ((access-path (make-instance 'access-path)))
    (when source
      (parse access-path source :top-level-model first-model))
    access-path))

(defmethod print-object ((obj access-path) stream)
  (format stream "~A" (access-path-source obj)))

(defclass relation ()
  ((operator
    :initarg :operator
    :reader relation-operator)
   (arguments
    :initarg :arguments
    :reader relation-arguments
    :type (vector (member string number access-path))
    :documentation "Array of arguments. Each argument is either a
    constant, or an istance of `access-path'."))
  (:documentation "Relational operator, e.g. equality of two values,
  or membership of a value in the set defined by an
  `access-path'. Need not to be a binary relation in general."))

(defun make-relation (operator arguments)
  (let ((arguments-array (make-array (length arguments))))
    (do ((k 0 (1+ k))
         (a arguments (cdr a)))
        ((null a))
      (setf (aref arguments-array k) (car a)))
    (make-instance 'relation
                   :operator operator
                   :arguments arguments-array)))

(defmethod print-object ((obj relation) stream)
  (format stream "(~A ~{ ~A~})"
          (relation-operator obj)
          (coerce (relation-arguments obj) 'list)))


(defun relation-argument (relation k)
  (check-type relation relation)
  (aref (slot-value relation 'arguments) k))


(defclass constraint (acl-entity)
  ((logical-operator
    :type (member :and :or)
    :initarg :logical-operator
    :reader constraint-logical-operator)
   (items
    :initarg :items
    :reader constraint-items
    :documentation "List of arguments. Each argument is an instance of either
    `constraint', or `relation'."))
  (:documentation "Parsed representation of rule constrint."))

(defun make-constraint (logical-operator &rest items)
  (make-instance 'constraint :logical-operator logical-operator
                 :items items))

(defmethod print-object ((the-constraint constraint) stream)
  (format stream "{ Constraint ~A ~{~A~} }"
          (constraint-logical-operator the-constraint)
          (constraint-items the-constraint)))

;;;
;;; Rules
;;;
(deftype access-mode () '(member :allow :deny))

(defclass rule (acl-entity)
  ((mode
    :type (or rule-mode null)
    :accessor rule-mode
    :initarg :mode
    :initform nil)
   (model
    :type (or model null)
    :initarg :model
    :accessor rule-model
    :initform nil)
   (operations
    :accessor rule-operations
    :initform nil
    :documentation "List of operations affected by the rule.")
   (roles
    :initarg :roles
    :accessor rule-roles
    :initform nil
    :documentation "List of roles associated with this rule. Each role
    is a list such that its CAR is the name of role, and CDR is the list
    of `access-path' objects (parsed expressions).")
   (users
    :initarg :users
    :accessor rule-users
    :initform nil
    :documentation "List of users assigned to this rule.")
   (constraint
    :type 'string
    :initarg :constraint
    :accessor rule-constraint
    :initform nil)
   (comment
    :type (or string null)
    :initform nil
    :accessor rule-comment))
  (:documentation "Access control rule."))

(defun make-acl-rule ()
  "Create empty rule."
  (make-instance 'rule
                 :mode :allow
                 :model nil))


(defmethod print-object ((the-rule rule) stream)
  (format stream "{~% ~s ~s ~%}"
          (type-of the-rule)
          (loop :for i :in '(mode comment condition)
             :collect (cons i (slot-value the-rule i)))))
