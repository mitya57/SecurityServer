(in-package :secsrv-test)


(defun evaluate-queries (conn statements)
  (dolist (sql-statement statements)
    (let* ((query (dbi:prepare conn sql-statement))
           (result (dbi:execute query)))
      ;;(dbi:fetch result)
      (declare (ignore result)))))

(defparameter *database-schema*
  '("CREATE TABLE a (a_id INTEGER PRIMARY KEY)"
    "CREATE TABLE b (
        b_id INTEGER PRIMARY KEY,
        a_id INTEGER REFERENCES a(a_id),
        str  VARCHAR2(200))"
    "CREATE TABLE bitem (
        bitem_id INTEGER PRIMARY KEY,
        b_id INTEGER REFERENCES b(b_id),
        bitem_value INTEGER)"
    ;; tables for user-to-role mebership
    "CREATE TABLE auth_user (
        id INTEGER PRIMARY KEY,
        username VARCHAR2(200))"
    "CREATE TABLE v_granted_roles (
        user_id INTEGER,
        f_permissionstypes_name VARCHAR2(500),
        f_department_id INTEGER)"))

(defparameter *database-values*
  '("INSERT INTO a VALUES (1)" ; no bitems
    "INSERT INTO a VALUES (2)" ; small bitems
    "INSERT INTO a VALUES (3)" ; large bitems
    ;;
    "INSERT INTO b VALUES (1, 1, 'b with no items')"
    ;;
    "INSERT INTO b VALUES (2, 2, 'with small items')"
    "INSERT INTO bitem VALUES (6, 2, 10)"
    "INSERT INTO bitem VALUES (7, 2, 20)"
    "INSERT INTO bitem VALUES (8, 2, 30)"
    ;;
    "INSERT INTO b VALUES (3, 3, 'with large items')"
    "INSERT INTO bitem VALUES (4, 3, 100)"
    "INSERT INTO bitem VALUES (5, 3, 500)"))


(defparameter *test-policy* "
(defmodel ent-a
  (table a)
  (attributes
    ((id a_id primary key))))
(defmodel ent-b
  (table b)
  (attributes
    ((id b_id primary key)
     (a a_id references ent-a)
     (str str))))
(defmodel ent-bitem
  (table bitem)
  (attributes
    ((id bitem_id primary key)
     (b b_id references ent-b)
     (bitem_value bitem_value))))
(defconcept AWithLargeBItem
  (inherits ent-a)
  (constraint some(ent-bs.ent-bitems.bitem_value) > 100))
;;
(defrole registered-user)
;;
(rule everyone-can-delete-b
  (access allow)  ; allow/deny
  (concept ent-b)
  (grantees (role registered-user))
  (operations delete edit))
;;
(rule root-can-NOT-delete-b
  (access deny)
  (concept ent-b)
  (grantees (user root))
  (operations delete edit))
;;
(rule can-delete-a
  \"root can delete A, if all b.bitem are greater than 100\"
  (access allow)
  (concept AWithLargeBItem)
  (grantees (user root))
  (operations delete edit)
  (constraint 100 <= every(ent-bs.ent-bitems.bitem_value)))
")
