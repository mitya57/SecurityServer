;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :secsrv)

(defmethod register ((the-role role) (the-policy policy))
  "Add an instance of `role' THE-ROLE to THE-POLICY."
  (with-accessors ((roles policy-roles)) the-policy
    (push the-role roles))
  t)

(defmethod register ((the-model model) (the-policy policy))
  "Add an instance of `model' THE-MODEL to THE-POLICY."
  (push the-model (policy-models the-policy))
  t)

(defmethod register ((the-rule rule) (the-policy policy))
  "Add an instance of `rule' THE-RULE to THE-POLICY."
  (push the-rule (policy-rules the-policy))
  t)


(defun load-policy (filename &key (validate-syntax-only nil))
  "Process all statements in a file specified by FILENAME and
validates correctness of the policy. When VALIDATE-SYNTAX-ONLY is set
to NIL and no errors were found in the file, then new `policy' is
assigned to *CURRENT-POLICY* special variable. Otherwise, no changes
are made.

Returns (values policy nil) if no errors found and (values nil
list-of-errors) otherwise.
"
  (let ((last-statement-name "BEGIN OF FILE")
        (new-policy (make-policy))
        (validation-errors '()))
    (handler-case
        (with-open-file (s filename)
          (loop
             :with *current-policy* = new-policy
             :for statement = (read s nil nil) :while statement
             :for parsed-object = (parse-statment statement)
             :do
             (multiple-value-bind (result error)
                 (validate parsed-object :policy new-policy)
               (unless result (push validation-errors error)))
             (setf last-statement-name (entity-name parsed-object))
             (register parsed-object new-policy))
          (when (and (not validate-syntax-only) (not validation-errors))
            (setf *current-policy* new-policy))
          (values (if (endp validation-errors) new-policy nil)
                  validation-errors))
      (end-of-file ()
        (error (format nil "Syntax error (unbalanced parentheses?) after '~A'."
                       last-statement-name)))
      (parsing-error (e)
        (format t "~&~%ERROR: ~A~%~%" (text e))))))
