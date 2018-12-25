;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Encoding: utf-8; -*-

(defpackage :secsrv-test-asd
  (:use :common-lisp :asdf))

(in-package :secsrv-test-asd)

(asdf:defsystem :secsrv-test
  :description "Tests for CBAC Security server"
  :author "serg@msu.ru"
  :depends-on ("secsrv" "lift")
  :pathname "test"
  :serial t
  :components
  ((:file "package")
   (:file "test-parser")
   (:file "test-queryset")
   (:file "populate-db")
   (:file "test-policy")))
