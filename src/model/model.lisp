;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :secsrv)

(defclass attribute ()
  ((name :initarg :name :accessor attribute-name)
   (column-name
    :type string
    :initarg :column-name
    :accessor attribute-column-name)
   (data-type :initarg :data-type)
   (model :initform nil :accessor attribute-model
          :documentation "The model this attribute belongs to.")
   (target-model
    :type (or string symbol model null)
    :initarg :target-model
    :accessor attribute-target-model
    :documentation "If attribute is a foreign key, this is a referenced model.")
   (reverse-attribute
    :initarg :reverse-attribute
    :accessor attribute-reverse-attribute
    :initform nil
    :documentation "If non NIL, this model is referenced from
    target-model and this attribute represents 'reverse' foreign
    keys. Reverse-attribute is the foreign key attribute in
    target-model."))
  (:documentation "Application level object's attribute, basically, a database
  column."))

(defun make-attribute (name column-name &key target-model data-type reverse-attribute)
  (make-instance 'attribute
                 :name name
                 :column-name column-name
                 :target-model target-model
                 :data-type data-type
                 :reverse-attribute reverse-attribute))

(defmethod print-object ((obj attribute) stream)
  (let ((model (attribute-model obj)))
    (format stream "Attribute ~A.~A ~:[T~;NIL~]"
            (if model (model-name model) "NIL")
            (attribute-name obj)
            t)))


(defclass model (acl-entity)
  ((name
    :initarg :name
    :initform nil
    :accessor model-name
    :documentation "Name of the application-level class.")
   (table-name
    :initarg :table-name
    :initform nil
    :accessor model-table
    :documentation "Master table that contains instances of this class.")
   (primary-key
    :accessor model-primary-key
    :initform nil
    :documentation "Name of the primary key database column.")
   (operations
    :initarg :operations
    :initform nil
    :accessor model-operations
    :documentation "List of possible operations for this class.")
   (attributes
    :initform nil
    :accessor model-attributes
    :type (or null list)
    :documentation "Description of object's attributes, mapping to
    database columns and so on. List of `attribute'. If another model
    X has a reference to this one, an backward attribute named
    'Xs' (plural name of referencing model) added to this list. This
    backward attribude is mapped to the primary key database column in
    order to simplify table joins."))
  (:documentation "A model describes application level class."))


(defun make-model (&key name)
  (make-instance 'model :entity-name name :name name))


(defun add-attribute (attribute model)
  (setf (attribute-model attribute) model)
  (push attribute (model-attributes model)))

(defun find-attribute (attribute-name the-model)
  (find-if #'(lambda (a) (string-equal (attribute-name a) attribute-name))
           (model-attributes the-model)))
