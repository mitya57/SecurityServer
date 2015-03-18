;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :secsrv)


;;;
;;; Policy
;;;
(defclass policy ()
  ((rules :initform nil
          :accessor policy-rules
          :documentation "List of all policy rules.")
   (roles :initform nil
          :accessor policy-roles
          :documentation "List of all roles")
   (models :initform nil
           :accessor policy-models
           :documentation "List of all models"))
  (:documentation "Collection of all rules."))


(defun make-policy ()
  (make-instance 'policy))


(defmethod print-object ((the-policy policy) stream)
  "Pretty printing of policy objects."
  (format stream "{~% A POLICY with ~r role~:P, ~r model~:P, and ~r access rule~:p ~%}"
          (length (policy-roles the-policy))
          (length (policy-models the-policy))
          (length (policy-rules the-policy))))

(defun find-model (name &key (policy *current-policy*))
  "Find a `model' by it's NAME in a `policy' specified by POLICY
keyword. NAME is case-insensitive."
  (find-if #'(lambda (m) (string-equal name (model-name m)))
           (policy-models policy)))


(defun find-role (name &key (policy *current-policy*))
  "Find a `role' by it's NAME in a `policy' specified by POLICY keyword."
  (find-if #'(lambda (r) (string-equal name (role-name r)))
           (policy-roles policy)))
