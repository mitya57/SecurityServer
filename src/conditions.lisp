;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-


(in-package :secsrv.sys)


(define-condition parsing-error (simple-error)
  ((text :initarg :text :reader text))
  (:documentation "Represents general parsing error."))

(defun parsing-error (format &rest args)
  (error 'parsing-error
         :format-control format
         :format-arguments args))

(define-condition malformed-statement-error (parsing-error)
  ()       ; no extra slots defined so far
  (:documentation "Signaled when the parser can not recognize a statement."))

(defun malformed-statement-error (format &rest args)
  (error 'malformed-statement-error
         :format-control format
         :format-arguments args))

(define-condition inconsistent-policy-error (parsing-error)
  ()       ; no extra slots defined so far
  (:documentation "Signalled when the policy is inconsistant."))

(defun inconsistent-policy-error (format &rest args)
  (error 'inconsistent-policy-error
         :format-control format
         :format-arguments args))
