;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :secsrv)


(defclass checker ()
  ((policy
    :type policy
    :initarg :policy
    :initform (error "Must specify a valid policy for a checker.")
    :accessor checker-policy)
   (database-info
    :initarg :database-info
    :initform (error "Must provide a database connect string.")
    :accessor checker-database-info
    :documentation "Relevant information required to establish database connection.")
   (database-connection
    :initform nil
    :accessor checker-connection
    :documentation "Current database connection. NIL if not connected."))
  (:documentation "All information required for access control checks,
  including the `policy' and database connection."))


(defun make-checker (&key database-info (policy *current-policy*))
  "Create an ACL checker associated with database connection specified
  by DATABASE-INFO and `policy' POLICY."
  (make-instance 'checker :database-info database-info :policy policy))


(defmacro with-checker ((checker-var database-info &key (policy '*current-policy*))
                        &body body)
  "Evaluate BODY with the checker assigned to CHECKER-VAR. Connect and
disconnect the checker from database."
  `(let ((,checker-var
          (make-instance 'checker :database-info ,database-info :policy ,policy)))
     (with-accessors ((db-info checker-database-info)
                      (connection checker-connection)) ,checker-var
       (setf connection
             (clsql:connect (getf db-info :connect-string)
                            :database-type (getf db-info :type)
                            :if-exists :warn-old))
       (unwind-protect
            (progn ,@body)
         (clsql:disconnect :database connection)))))



(defgeneric has-access (checker user model object operation)
  (:documentation "Verify that access may be granted for request under
  the `policy' and database associated to CHECKER."))

(defgeneric list-operations (checker user model object)
  (:documentation "Find all operations that the USER is granted for
  OBJECT of class MODEL under access control `policy' associated to
  CHECKER."))


(defclass access-request ()
  ((user-name :initarg :user-name :accessor request-user)
   (model-name :initarg :model-name :accessor request-model)
   (object-id :initarg :object-id :accessor request-object-id)
   (operation :initarg :operation :accessor request-operation))
  (:documentation "A collection of values representing user's request."))

(defun make-access-request (user-name model-name object-id operation)
  (make-instance 'access-request
                 :user-name user-name
                 :model-name model-name
                 :object-id object-id
                 :operation operation))
