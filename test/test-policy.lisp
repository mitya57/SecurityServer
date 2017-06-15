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
  (checker::with-checker (the-checker #'(lambda () nil) :policy policy :reuse-database-connection conn)
    (ensure-same (checker:has-access the-checker "user" "ent-b" 2 "delete") nil :report "Non-root user delete operation on B entity with pk=2 FAILED.")
    (ensure-same (checker:has-access the-checker "root" "ent-b" 2 "delete") nil :report "Delete operation on B entity with pk=2 issued by ROOT has FAILED.")))

(addtest (policy)
  simple-constrained-request
  (checker:with-checker (the-checker #'(lambda () nil) :policy policy :reuse-database-connection conn)
    (ensure-same (checker:has-access the-checker "user" "ent-a" 1 "delete") nil :report "USER delete on A:1 FAILED.")
    (ensure-same (checker:has-access the-checker "root" "ent-a" 1 "edit") nil :report "ROOT delete on A:1 FAILED.")
    (ensure-same (checker:has-access the-checker "root" "ent-a" 3 "edit") nil :report "ROOT delete on A:3 FAILED.")))
