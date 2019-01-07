(in-package :secsrv)

(defun get-home-directory ()
  cl-user::*secsrv-home*)

(defun load-config (name &key (directory (get-home-directory)))
  (let ((filename (merge-pathnames name directory))
        (config (py-configparser:make-config)))
    (log-message :info "Loading config file ~A" filename)
    (py-configparser:read-files config (list filename))))

(defun get-option (config section-name option-name &optional default)
  (py-configparser:get-option config section-name option-name :defaults default))


(defun main ()
  (setup-logging)
  (log-message :info "Starting")
  (let* ((config (load-config #p"local.cfg"))
         (acl-filename (get-option config "Policy" "rules"))
         (dbd.oracle:*foreign-library-search-paths*
          (let ((path (get-option config "Database" "library-path" "")))
            (when path
             (list (pathname path))))))
    (log-message :info "Using database ~A" (get-option config "Database" "type"))
    (flet ((connection-maker ()
             (alexandria:eswitch ((get-option config "Database" "type") :test #'string=)
               ("oracle" (dbi:connect (intern "oracle" :keyword)
                                     :database-name (get-option config "Database" "name")
                                     :username (get-option config "Database" "username")
                                     :password (get-option config "Database" "password")))
               ("sqlite3"
                (let* ((dbf (get-option config "Database" "name"))
                       (relativep (char/= #\/ (aref dbf 0)))
                       (home (get-home-directory))
                       (complete-dbf (format nil "~A" (merge-pathnames
                                                       (pathname dbf)
                                                       (when relativep home))))
                       (conn
                        (dbi:connect :sqlite3
                                     :database-name complete-dbf)))
                  (log-message :info "Connected to ~A: ~A~%" complete-dbf conn)
                  conn)))))
      (setf *sql-trace* t)
      (setf *current-policy* (secsrv.parser::parse-file acl-filename))

      (checker:clear-statistics)

      (server:start-server #'connection-maker
        :port (py-configparser:get-option config "Server" "port" :defaults 8135 :type :number))

      (checker:with-checker (the-checker #'connection-maker
                                        :policy *current-policy*)
        (print (time (checker:has-access the-checker "safonin" "article" 211535 "delete")))
        (print (time (checker:has-access the-checker "safonin" "book" 211916 "delete"))))
      )))
