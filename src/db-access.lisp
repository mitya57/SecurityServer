;;;;
;;;; Database interface functions
;;;;
(in-package :secsrv)


(defun run-query (sql-statement)
  (dbi:with-transaction *dbcon*
    (incf *sql-query-count*)
    (when *sql-trace*
      (log-message :trace "Evaluating query:~% ~30,,,'*@A~% ~A~% ~30,,,'*@A~%" "*" sql-statement "*"))
    (loop
       :with result = (dbi:execute (dbi:prepare *dbcon* sql-statement))
       :for row = (dbi:fetch result)
       :while row
       :collect (rest row))))

(defun user-id-by-name (user-name)
  "Returns user-id by USER-NAME."
  (let* ((sql (format nil "~
              SELECT au.id ~
              FROM auth_user au ~
              WHERE au.username='~A'" user-name))
         (res (run-query sql)))
    (caar res)))


(defun worker-id-by-user-name (user-name)
  "Returns user-id by USER-NAME."
  (let* ((sql (format nil "~
              SELECT wp.worker_id ~
              FROM auth_user au ~
                   JOIN workers_profile wp on (au.id=wp.user_id) ~
              WHERE au.username='~A'" user-name))
         (res (run-query sql)))
    (caar res)))


(defun user-roles (user &key (department nil))
  "Retruns list of roles the USER is a membor of."
  (declare (ignorable user department))
  '())


(defun user-has-role (user role &key (parameters '()))
  (declare (ignore parameters))
  (check-type role <role>)
  (log-message :trace "Checking ROLE ~A for user ~A"
               (role-name role)
               user)
  t)


(defun evaluate-object-related-query (query object &rest objects)
  "Evaluate prepared SQL query of the `ACCESS-PATH' with top-level
object bound to an object of `MODEL' identified by OBJECT-ID."
  (declare (ignore objects))
  (check-type object <object>)
  (let* ((populated-query (format nil query (object-id object)))
         (result (run-query populated-query)))
    ;; access path adresses single attribute, so take first value from
    ;; each row
    (log-message :trace "Query returned ~D records." (length result))
    (mapcar #'first result)))

(defun empty-query-result-p (query object &rest objects)
  (null (apply #'evaluate-object-related-query query object objects)))
