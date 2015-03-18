;;;;
;;;; Database interface functions
;;;;
(in-package :secsrv)


(defun run-query (sql-statement)
  (clsql:with-transaction ()
    (when *sql-trace*
      (log-message :trace "Run-query: Evaluating query:~%  ~A~%" sql-statement))
    (clsql:query sql-statement)))


(defun get-object-direct-attribute (model object-id attribute-name)
  "Return value of object's attribute."
  (let (sql
        res
        (attribute (find-attribute attribute-name model)))
    (setf sql (format nil "SELECT ~A FROM ~A WHERE F_~A_ID = ~A"
                      (attribute-column-name attribute)
                      (model-table model)
                      (model-table model) ; primary key t_<table>_id
                      object-id))
    (when *sql-trace* (print sql))
    (clsql:with-transaction ()
      (setf res (clsql:query sql)))
    (mapcar #'car res)))


(defun get-object-attribute (model object-id access-path-string)
  "Return value of object's attribute specified by ACCESS-PATH."
  (check-type access-path-string string "an access-path")
  (let ((access-path (make-access-path access-path-string model)))
    (evaluate-access-path access-path model object-id)))


(defun user-id-by-name (user-name)
  "Returns user-id by USER-NAME."
  (let ((sql (format nil "~
              SELECT au.id ~%~
              FROM auth_user au ~%~
              WHERE au.username='~A'" user-name))
        res)
    (clsql:with-transaction ()
      (setf res (clsql:query sql)))
    (when res (setf res (caar res)))
    res))


(defun worker-id-by-user-name (user-name)
  "Returns user-id by USER-NAME."
  (let ((sql (format nil "~
              SELECT wp.worker_id ~%~
              FROM auth_user au ~%~
                   JOIN workers_profile wp on (au.id=wp.user_id) ~%~
              WHERE au.username='~A'" user-name))
        res)
    (clsql:with-transaction ()
      (setf res (clsql:query sql)))
    (when res (setf res (caar res)))
    res))



(defun user-roles (user &key (department nil))
  "Retruns list of roles the USER is a membor of."
  (declare (ignorable user department))
  '())


(defun user-has-role (user role &key (parameters '()))
  (declare (ignore user parameters))
  (check-type role role)
  t)



(defun evaluate-access-path (access-path model object-id)
  "Evaluate prepared SQL query of the `ACCESS-PATH' with top-level
object bound to an object of `MODEL' identified by OBJECT-ID."
  (check-type access-path access-path)
  (check-type model model)
  (assert (eql model (access-path-model access-path)))
  (let ((query (format nil (access-path-prepared-sql access-path) object-id)))
    ;; access path adresses single attribute, so take first value from
    ;; each row
    (mapcar #'first (run-query query))))



;; not used
(defun db-setup-connection (&key (use-oracle nil))
  (setq *dbcon*
	(if use-oracle
            (clsql:connect '("vcalc" "science" "tttpwd") :database-type :odbc)
            (clsql:connect '("test/istina.sqlite") :database-type :sqlite3))))

;; not used
(defun db-close-connection ()
  (clsql:disconnect)
  (setq *dbcon* nil))


;;; Examples of SQL query processing


(defun test-db (&key (use-oracle nil))
  (when (null *dbcon*) (db-setup-connection :use-oracle use-oracle))
  (clsql:with-default-database (*dbcon*)
    (clsql:with-transaction ()
      (loop for k :from 1 :to 10
	 :do (clsql:query "SELECT count(*) FROM article"))
      (clsql:query "SELECT f_article_id, f_article_name FROM article WHERE f_article_id>216748"))))


;;; Using another framework
#+(or)
(defun test-dbi ()
  (dbi:with-connection (conn :sqlite3 :database-name "test/istina.sqlite")
    (let* ((query (dbi:prepare conn "SELECT count(*) FROM article"))
           (result (dbi:execute query)))
      (loop for row = (dbi:fetch result)
         while row
         do (format t "~A~%" row)))))
