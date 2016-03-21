(in-package :secsrv-test)

(defvar *testing-db* nil)

(defun create-testing-database ()
  (clsql:with-database (db '(":memory:") :database-type :sqlite3)
    (clsql:with-default-database (db)
      (clsql:query "create table x (
                      f_x_id integer
                    )")
      (clsql:query "create table y (
                      f_y_id integer,
                      f_x_id integer references x(id)
                    )")
      (clsql:query "insert into x (f_x_id) values (123)")
      ;;(print (clsql:query "select * from x"))
    )))


(deftestsuite parse-access-path (root)
  ((the-policy (secsrv::load-policy "test/policies/test.acl")))
  (:run-setup :once-per-suite)
  (:documentation
   "Unit tests for access path expressions parsing."))

(addtest (parse-access-path)
  parse-simple-access-paths
  (let ((article-model (secsrv::find-model "Article")))
    (ensure-different article-model nil)

    (mapc #'(lambda (query-result)
              "Parse access path expression (CAR) and compare with CDR."
              (let* ((path-expression (car query-result))
                     (expected-result (cdr query-result))
                     (ap (secsrv::make-access-path)))
                (secsrv::parse ap path-expression :top-level-model article-model)
                (mapc #'(lambda (obtained expected)
                          (ensure-same obtained expected :test #'string-equal))
                      (mapcar #'(lambda (att)
                                  (format nil "~A.~A"
                                          (secsrv::model-name (secsrv::attribute-model att))
                                          (secsrv::attribute-name att)))
                              (secsrv::access-path-parsed ap))
                      expected-result)))
          '(("article.owner" . ("article.owner"))
            ("article.owner.name" . ("article.owner" "user.name"))
            ("object.owner.name" . ("article.owner" "user.name"))
            ("object.owner.articles.owner" . ("article.owner" "user.articles" "article.owner"))))))



(deftestsuite generate-sql (root)
  ((the-policy (secsrv::load-policy "test/policies/test.acl"))
   (database-info '(:type :sqlite3
                    :connect-string ("test/istina.sqlite")))
   (testing-database-info '(:type :sqlite3
                            :connect-string (":memory:"))))
  (:run-setup :once-per-suite)
  (:setup
   (create-testing-database)
   (format t ""))
  (:teardown
   (format t ""))
  (:documentation
   "Unit tests for generated sql code."))




(addtest (generate-sql)
  simple-access-paths
  (let ((article-model (secsrv::find-model "Article"))
        (object-id 0))
    (ensure-different article-model nil)
    (mapc #'(lambda (path-expression)
              (secsrv::access-path->sql
               (secsrv::make-access-path path-expression article-model)
               article-model
               object-id))
          '("article.owner.name"))))

#|
(ensure-same (secsrv::get-object-attribute article-model
                                           211444
                                               "owner.name")
             '("safonin")
             :test #'equal)))
|#
