;;;;
;;;; Database interface functions
;;;;
(in-package :secsrv)

(defun %run-query (sql-statement &rest params)
  ;;--- FIXME: this function assumes that the SQL query always selects only one column
  (dbi:with-transaction *dbcon*
    (incf *sql-query-count*)
    (when *sql-trace*
      (log-message :trace "Evaluating query:~% ~30,,,'*@A~% ~A~% ~30,,,'*@A~%~
                           ~@[Parameters are: ~A~]" "*" sql-statement "*" params))
    (loop
       :with parsed = (dbi:prepare *dbcon* sql-statement)
       :with result = (apply #'dbi:execute parsed params)
       :for row = (dbi:fetch result)
       :while row
       :collect (rest row))))

(defun run-query (sql-statement &rest params)
  "Evaluates SQL statement and put the result into current request cache."
  (flet ((digest-as-string (string)
           (let ((digest (md5:md5sum-string string)))
             (format nil
                     "~(~{~2,'0X~}~)"
                     (map 'list #'identity digest)))))
    (let* ((digest (digest-as-string (format nil "~A ~A" sql-statement params)))
           (chached-value (item-at (checker::request-cache *current-request*) digest)))
      (if chached-value
          chached-value
          (setf (item-at (checker::request-cache *current-request*) digest)
                (apply #'%run-query sql-statement params))))))

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


(defun user-has-role (user role &rest parameters)
  (check-type role policy:<role>)
  (log-message :trace "Checking ROLE `~A' for user `~A'."
               (policy:role-name role)
               user)
  (let* ((sql (format nil "~
              SELECT count(*) AS cnt ~
              FROM v_granted_roles ~
              WHERE user_id = ? AND f_permissionstypes_name = ? ~
                ~@[AND f_department_id IN (~{~A~^, ~})~]"
                      (first parameters)))
         (res (run-query sql (user-id-by-name user) (policy:role-name role))))
    (or (string= (policy:role-name role) "registered-user")
        (<= 1 (caar res)))))


(defun evaluate-object-related-query (query object &rest objects)
  "Evaluate prepared SQL query of the `ACCESS-PATH' with top-level
object bound to an object of `MODEL' identified by OBJECT-ID."
  (declare (ignore objects))
  (check-type object checker:<object>)
  (let* ((populated-query (format nil query (checker:object-id object)))
         (result (run-query populated-query)))
    ;; access path adresses single attribute, so take first value from
    ;; each row
    (log-message :trace "Query returned ~D records." (length result))
    (mapcar #'first result)))

(defun empty-query-result-p (query object &rest objects)
  (null (apply #'evaluate-object-related-query query object objects)))
