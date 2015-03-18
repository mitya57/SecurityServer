;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :secsrv)


(defun relation-argument-value (argument user model object-id)
  "Returns value of relation's ARGUMENT. Argument could be a constant,
or an aceess-path. The returned value is always a list, regardless of
the result's real cardinality."
  (typecase argument
    ((or number string) (list argument))
    (access-path
     (evaluate-access-path argument model object-id))))

(defun match-constraint (constraint user model object-id)
  "Check that `CONSTRAINT' is satisfied by USER and an object of `MODEL'
  identified by OBJECT-ID."
  (log-message :info "Checking constraint ~A~%" constraint)
  (typecase constraint
    (constraint       ; and/or expression
     (funcall
      (ecase (constraint-logical-operator constraint)
        ((:and) #'(lambda (p list) (every p list)))
        ((:or) #'(lambda (p list) (some p list))))
      #'(lambda (c) (match-constraint c user model object-id))
      (constraint-items constraint)))
    (relation
     (let ((arguments-values
            (map 'list #'(lambda (a) (relation-argument-value a user model object-id))
                 (relation-arguments constraint))))
       (format t "~&values=~A~%MATCH-CONSTRAINT result=" arguments-values)
       (alexandria:eswitch ((relation-operator constraint) :test #'string-equal)
         ("=" (equal (first arguments-values) (second arguments-values))))))
    (t nil)))

(defun match-rule (user model object-id operation rule)
  "Check that the `RULE' matches the access request specified by USER,
MODEL, OBJECT-ID, and OPERATION. Returns T or NIL."
  (let ()
    (with-accessors ((rule-users rule-users)
                     (rule-roles rule-roles)
                     (rule-model rule-model)
                     (rule-operations rule-operations)
                     (rule-constraint rule-constraint))
        rule
      (and
       ;; This rule is defined for the right model and operation
       (eql model rule-model)
       (member operation rule-operations :test #'string-equal)
       ;; User in the list of rule's users, if such a list specified by the rule
       (or (not rule-users) (member user rule-users :test #'string=))
       ;; User is a member of one of parameterized roles, if any
       (or (not rule-roles)
           (some #'(lambda (role)
                     (user-has-role user (first role) :parameters (rest role)))
                 rule-roles))
       ;; Rule's additional constraint is satisfied
       (or (not rule-constraint)
           (match-constraint rule-constraint user model object-id))))))

(defmethod has-access ((the-checker checker) (user string) (model string) (object-id integer) (operation string))
  "Check that USER has permission to evaluate OPERATION on object of
`model' MODEL with id OBJECT-ID.

Return: T or NIL."
  (let* ((policy (checker-policy the-checker))
         (model (find-model model :policy policy))
         (operation (string-downcase operation))
         (start-time (get-internal-real-time))
         (*current-request* (make-access-request user model object-id operation)))
    (prog1
        (and (some #'(lambda (r)
                       (and (eql (rule-mode r) :allow)
                            (match-rule user model object-id operation r)))
                   (policy-rules policy))
             (notany #'(lambda (r)
                         (and (eql (rule-mode r) :deny)
                              (match-rule user model object-id operation r)))
                     (policy-rules policy)))
      (log-message :info "Access check took ~F seconds.~%"
                   (/ (- (get-internal-real-time) start-time) internal-time-units-per-second)))))
