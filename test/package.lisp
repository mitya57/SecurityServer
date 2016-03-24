(in-package :cl-user)

(defpackage :secsrv-test
  (:use :cl :lift :secsrv)
  (:export #:run-all-tests)
  (:documentation
   "This package contains unit tests of the CBAC Security Server."))

(in-package :secsrv-test)

(deftestsuite root ()
  ()
  (:documentation
   "Root unit test suite for the CBAC Security Server."))


(defun run-all-tests ()
  (lift:run-tests :suite 'root :break-on-errors? nil))
