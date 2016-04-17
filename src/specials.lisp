;;;; Special variables

(in-package :secsrv.sys)

(defvar *dbcon* nil
  "Default database connection.")

(defvar *sql-trace* t
  "If not NIL, all SQL queries will be logged.")

(defvar *sql-query-count* 0
  "Used for counting processed SQL queries per access request.")

(defvar *database-info* nil
  "Current database info structure.")

(defvar *current-policy* nil
  "An instance of `<policy>' that used by default by all checking functions.")

(defvar *current-checker* nil
  "An instance of `<checker>' that used by default by all access control functions.")

(defvar *current-request* nil
  "An instance of `<access-request>' representing current access request parameters.")
