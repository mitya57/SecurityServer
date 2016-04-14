(in-package :secsrv)

(defun load-config (name &key (directory cl-user::*secsrv-home*))
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
          (let ((path (get-option config "Database" "library-path")))
            (when path
             (pathname path)))))
    (log-message :info "Using database ~A" (get-option config "Database" "type"))
    (flet ((connection-maker ()
             (alexandria:eswitch ((get-option config "Database" "type") :test #'string=)
               ("oracle" (dbi:connect (intern "oracle" :keyword)
                                     :database-name (get-option config "Database" "name")
                                     :username (get-option config "Database" "username")
                                     :password (get-option config "Database" "password")))
               ("sqlite3" (dbi:connect :sqlite3
                                       :database-name (get-option config "Database" "name"))))))
      (setf *sql-trace* t)
      (setf *current-policy* (secsrv.parser::parse-file acl-filename))

      (start-server #'connection-maker :port 8135)

      (with-checker (the-checker #'connection-maker
                                 :policy *current-policy*)
        (print (time (has-access the-checker "safonin" "article" 211444 "delete")))
        (print (time (has-access the-checker "safonin" "book" 211916 "delete"))))
      )))
