;;;

(in-package :secsrv)

(defvar *clack-handle* nil "Instance of the running clack Web-server.")

(defun stop-clack ()
  (prog1
      (when *clack-handle*
        (clack:stop *clack-handle*))
    (setf *clack-handle* nil)))


(defun start-server (connection-maker &key (port 8135))
  (log-message :info "Starting HTTP server on port ~D." port)
  (stop-clack)
  (setf *clack-handle*
        (clack:clackup
         #'(lambda (env)
             (handler-case
                 (destructuring-bind (kw-check user-name operation entity-name object-id . tail)
                     (cdr (cl-utilities:split-sequence #\/ (getf env :path-info)))
                   (declare (ignore kw-check tail))
                   (with-checker (the-checker connection-maker :policy *current-policy*)
                     (multiple-value-bind (granted-p elapsed-time sql-queries-count)
                         (has-access the-checker user-name entity-name (parse-integer object-id) operation)
                       (list hunchentoot:+http-ok+
                             '(:content-type "text/plain")
                             (list
                              (if granted-p "ALLOW" "DENY")
                              (format nil "~%~F seconds; ~A SQL requests" elapsed-time sql-queries-count)
                              "")))))
               (error (condition)
                 (list hunchentoot:+http-internal-server-error+
                       '(:content-type "text/plain")
                       (list (format nil "~A" condition)
                             "")))))
         :port port
         :server :hunchentoot)))
