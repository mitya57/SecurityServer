;;;
;(asdf:operate 'asdf:load-op 'clsql)
;(asdf:operate 'asdf:load-op 'clsql-sqlite3)
;(asdf:operate 'asdf:load-op 'clsql-uffi)
;(asdf:operate 'asdf:load-op 'clsql-odbc)
;(asdf:operate 'asdf:load-op 'clack)


(defpackage :secsrv
  (:use :cl)
  (:import-from :cl-log :log-message)
  (:export
   :has-access
   :permitted-operations
   :load-policy
   :main))
