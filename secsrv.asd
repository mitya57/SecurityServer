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
  :name "CBAC Security Server"
  :description "Concept-based access control (CBAC) Security Server."
  :version "0.1"
  :author "serg@msu.ru"
  :depends-on ("cl-log"
               "cl-containers"
               "cl-utilities"
               "cl-dbi" "dbd-sqlite3" ; database access
               "hunchentoot" "clack" "quri" ; server
               "alexandria" "esrap" "trivial-types")
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "types")
               (:file "specials")
               (:file "conditions")

               ;; useful tools
               (:module utils
                        :components
                        ((:file "logging")))

               (:file "policy") ; data structures and helper functions for policy manipulations

               (:file "db-access") ; database access level
               (:file "parser")  ; access control language parser
               (:file "checker") ; policy checker

               (:file "server") ; server

               ;; top-level function that starts everything
               (:file "main")))
