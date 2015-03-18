;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Encoding: utf-8; -*-

(defpackage :secsrv-test-asd
  (:use :common-lisp :asdf))

(in-package :secsrv-test-asd)

(asdf:defsystem :secsrv-test
  :description "Tests for ABAC Security server"
  :author "serg@msu.ru"
  :depends-on ((:require "secsrv")
               (:require "lift"))
  :components
  ((:module "test"
            :serial t
            :components
            ((:file "package")
             (:file "test-access-path")))))
