(in-package :secsrv-test)

(deftestsuite policy (root)
  ((database-info '(:type :sqlite3
                    :connect-string ":memory:"))
   (policy (secsrv.parser:load-policy-from-string *test-policy*))
   (conn))
  (:setup
   (progn ;;(secsrv::setup-logging)
          (setf conn
                (dbi:connect (getf database-info :type)
                             :database-name (getf database-info :connect-string)))
          (evaluate-queries conn *database-schema*)
          (evaluate-queries conn *database-values*)))
  (:teardown
   (progn (dbi:disconnect conn)))
  (:documentation
   "Tests for validation access check corectness."))


(addtest (policy)
  simple-request
  (secsrv::with-checker (the-checker database-info :policy policy)
    (ensure-same (has-access the-checker "user" "ent-b" 2 "delete") t)
    (ensure-same (has-access the-checker "root" "ent-b" 2 "delete") nil)))

(addtest (policy)
  simple-constrained-request
  (secsrv::with-checker (the-checker database-info :policy policy :reuse-database-connection conn)
    (ensure-same (has-access the-checker "user" "ent-a" 1 "delete") nil)
    (ensure-same (has-access the-checker "root" "ent-a" 1 "edit") nil)
    ;;(ensure-same (has-access the-checker "root" "ent-a" 2 "edit") nil)
    (ensure-same (has-access the-checker "root" "ent-a" 3 "edit") t)))
