(defpackage :secsrv.sys
  (:use :cl)
  (:export #:*dbcon*
           #:*sql-trace*
           #:*sql-query-count*
           #:*current-policy*
           #:*current-checker*
           #:*current-request*
           #:inconsistent-policy-error))


(defpackage :secsrv
  (:use :cl :secsrv.sys)
  (:import-from :cl-log :log-message)
  (:import-from #:alexandria
                #:define-constant
                #:when-let)
  (:import-from #:containers
                #:make-container
                #:simple-associative-container
                #:item-at
                #:find-item)
  (:export #:has-access
           #:permitted-operations
           #:load-policy
           #:main))
