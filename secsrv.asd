;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Encoding: utf-8; -*-

(defpackage #:secsrv-asd
  (:use :common-lisp :asdf))

(in-package :secsrv-asd)

(pushnew :debug *features*)

#+(and :ccl :debug)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim
   (optimize
    (safety 3)	                ; Error checking
    (speed 0)	                ; Speed of the compiled code
    (compilation-speed 0)       ; Speed of compilation
    (space 1)                   ; Space of both intermediate files and object
    (debug 3))))


;;; Define some 'production' compilation parameters

#+:production
(proclaim '(optimize
            (safety 0)		        ; Run time error checking level
            (speed 3)			; Speed of the compiled source
            (compilation-speed 0)	; Speed of compilation
            (space 0)			; Space of both intermediate files and object
            (debug 0)))

#+:production
(declaim (inline find-attribute))


(asdf:defsystem :secsrv
  :name "ABAC Security Server"
  :version "0.1"
  :author "serg@msu.ru"
  :depends-on ("cl-log"
               "clsql" "clsql-sqlite3" "clsql-uffi" "clsql-odbc"
               ;; "clack"
               "cl-ppcre" "alexandria")
  :components
  ((module src
           :serial t
           :components
           ((:file "package")
            (:file "specials")

            ;; useful tools
            (:module utils
                     :components
                     ((:file "logging")))

            ;; basic data structures
            (:module model
                     :serial t
                     :components
                     ((:file "entity")
                      (:file "model")
                      (:file "acl")
                      (:file "role")
                      (:file "policy")))

            ;; database access level
            (:module dbaccess
                     :serial t
                     :components
                     ((:file "db-access")))

            ;; rule parser
            (:module parser
                     :depends-on (model)
                     :serial t
                     :components
                     ((:file "acl-parser")
                      (:file "validators")
                      (:file "load")
                      (:file "access-path")))

            ;; policy checker
            (:module checker
                     :serial t
                     :components
                     ((:file "checker")
                      (:file "has-access")
                      (:file "permitted-operations")))

            ;; server
            (:module server
                     :serial t
                     :components
                     ((:file "server")))

            ;; top-level function that starts everything
            (:file "main")))))
