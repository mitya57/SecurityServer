;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-


(in-package :secsrv)


(defclass acl-entity ()
  ((name
    :initarg :entity-name
    :initform ""
    :accessor entity-name
    :documentation "Name of the object."))
  (:documentation "Base class for all ACL-related classes, e.g. rules,
  models, and roles."))

(define-condition parsing-error (error)
  ((text :initarg :text :reader text))
  (:documentation "Represent general parsing error."))


(defgeneric validate (entity &key policy)
  (:documentation "Validate correctness of `acl-entity' ENTITY with
  respect to a `policy' specified by the keyword POLICY. If POLICY is
  NIL, then only a 'self-correctness' of ENTITY is checked.

Returns (values T NIL) if there are no errors found, and (values NIL
  list-of-error) otherwise.")
  (:method ((entity t) &key (policy nil))
    (declare (ignore policy))
    "Dummy validator that does not perfoms any check and always return T."
    (log-message :warning
                 "Validation method is not implemented for ~A. Skipping ~A."
                 (class-of entity) (entity-name entity))
    (values t nil)))


(defgeneric parse (entity source &key)
  (:documentation "Parse ENTITY from given SOURCE, typically a string,
  or S-expression. Raise condition of type `parsing-error' or it's
  subcalss if SOURCE does not contain a valid description for
  entity."))


(defgeneric register (entity policy)
  (:documentation "Register ACL-related ENTITY to the POLICY."))
