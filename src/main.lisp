(in-package :secsrv)

(defun main ()
  (let (;;(model-filename "test/policies/istina.model.acl")
        (acl-filename "test/policies/test.acl")
        ;; for oracle use :odbc and connect string like ("host" "username" "password")
        (dbd.oracle:*foreign-library-search-paths* '(#p"/opt/oracle/"))
        (db-info (if nil
                     '(:type :sqlite3
                       :connect-string "test/databases/istina.sqlite")
                     '(:type :oracle
                       :connect-string "localhost:1521/orcl"
                       :username "science"
                       :password "tttpwd"))))
    (flet ((connection-maker ()
             (ecase (getf db-info :type :oracle)
               (:oracle (dbi:connect :oracle
                                     :database-name (getf db-info :connect-string)
                                     :username (getf db-info :username)
                                     :password (getf db-info :password)))
               (:sqlite3 (dbi:connect :sqlite3
                                      :database-name (getf db-info :connect-string))))))
      (setup-logging)
      (log-message :info "Starting")

      (setf *sql-trace* t)
      (setf *current-policy* (secsrv.parser::parse-file acl-filename))

      (start-server #'connection-maker :port 8135)

      (with-checker (the-checker #'connection-maker
                                 :policy *current-policy*)
        (print (time (has-access the-checker "safonin" "article" 211444 "delete")))
        (print (time (has-access the-checker "safonin" "book" 211916 "delete"))))
      )))
