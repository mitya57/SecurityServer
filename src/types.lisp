;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :secsrv)

(defun elements-are-of-type (seq type)
  (every #'(lambda (x) (typep x type)) seq))

(deftype list-of-type (type)
  "Usage: (typep '(1 2 3) '(list-of-type integer)) ==> T"
  (let ((predicate (gensym)))
    (setf (symbol-function predicate)
      #'(lambda (seq) (elements-are-of-type seq type)) )
    `(and list (satisfies ,predicate)) ))
