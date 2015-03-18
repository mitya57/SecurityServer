;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :secsrv)


(defun permitted-operations (user model object-id)
  "Find the list of all operations that user USER may perform on
   object of `model' MODEL with id OBJECT-ID."
  (declare (ignorable user model object-id))
  '())
