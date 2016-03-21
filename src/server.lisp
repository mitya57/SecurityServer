;;;

(in-package :secsrv)


(defun start-server ()
  (log-message :info "Starting server")
  (db-setup-connection)
  (print (test-db))
  (db-close-connection)
  t)


#|

(asdf:operate 'asdf:load-op 'clsql)
(asdf:operate 'asdf:load-op 'clsql-sqlite3)
(asdf:operate 'asdf:load-op 'clsql-odbc)
(asdf:operate 'asdf:load-op 'clack)


(defpackage :secsrv
  (:use :cl)
  (:export :has-access :permitted-operations))


(defun app (env)
  (declare (ignore env))
  '(200 ; HTTP status
    (:content-type "text/plain")
    ("Hello, World!"))) ; body

(clack:clackup #'app)


(defpackage clack-sample
  (:use :cl
        :clack
        :clack.app.route))
(in-package :clack-sample)

(defroutes app
  (GET "/" #'index)
  (GET "/login" #'login)
  (POST "/login" #'authorize)
  (GET "/member/:id" #'member))

(clackup #'app)


;; export TNS_ADMIN, TWO_TASK=<tnsname>
(use-package :plain-odbc)
(setf *con* (connect "vcalc" "science" "tttpwd"))
(exec-query *con* "select count(*) from article")
(commit)
(close-connection *con*)


;; clsql-odbc
(clsql:connect '("vcalc" "science" "tttpwd") :database-type :odbc)
(clsql:query "select count(*) from book")
(clsql-sys:rollback)
(clsql-sys:disconnect)

(asdf:operate 'asdf:load-op 'clsql-sqlite3)
(clsql:connect '(":memory:") :database-type :sqlite3)


;; no DSN on the lisp host, specify connection information via :connection-string
> (clsql:connect '("friendly-server-name" "friendly-username" ""
		    :connection-string "DRIVER={FreeTDS};SERVER=mssql-server;DATABASE=database-name;UID=database-user;PWD=database-password;PORT=1433;TDS_Version=8.0;APP=clsql")
               :database-type :odbc)

(defvar *oracle-database*
  (make-instance 'hu.dwim.rdbms.oracle:oracle
		 :connection-specification '(:datasource "(DESCRIPTION = (ADDRESS =
                                                           (PROTOCOL = TCP)
                                                           (HOST = localhost)
                                                           (PORT = 1521))
                                                           (CONNECT_DATA = (SERVICE_NAME = orcl)))"
					     :user-name "science"
					     :password "tttpwd")))
(defun test-db ()
  (with-database *oracle-database*
    (with-transaction
      (loop for k :from 1 :to 20 :do
	   (execute "SELECT 1324 FROM dual")))))
|#
