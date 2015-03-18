;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :secsrv)

(defmethod validate ((the-model model) &key (policy *current-policy*))
  "Validate `model' object. If there exist attributes referencing to
another model, then model's name is resolved and target model stored
into attribute's target-model slot and backward referencing attribute
added to target-model."
  (let ((errors '()))
    (dolist (attribute (model-attributes the-model))
      (with-accessors ((target attribute-target-model))
          attribute
        (when target
          (let ((target-model (find-model target :policy policy)))
            (if target-model
                ;; Resolve target reference for foreign key attribute, and
                ;; add 'reverse' foreign key attribute to target model.
                (let* ((reverse-fk-name (format nil "~As" (model-name the-model)))
                       (reverse-fk-column-name
                        (attribute-column-name (model-primary-key target-model)))
                       (back-attribute (make-attribute reverse-fk-name
                                                       reverse-fk-column-name
                                                       :target-model the-model
                                                       :reverse-attribute attribute)))
                  (setf target target-model)
                  (add-attribute back-attribute target-model))
                (push (format nil "~A.~A references to an unknown model ~A."
                              (model-name the-model)
                              (attribute-name attribute)
                              target)
                      errors))))))
    ;;--- FIXME(serg@msu.ru): find a better way and place for missing PK initialization
    (unless (model-primary-key the-model)
      (setf (model-primary-key the-model)
            (format nil "F_~A_ID" (model-table the-model))))
    (values (endp errors) errors)))


(defmethod validate ((the-role role) &key (policy *current-policy*))
  "Validates correctness of `role' instance ROLE under `POLICY'."
  (let ((errors '()))
    (when (find-role (role-name the-role))
      (push (format nil "Duplicated role ~A found." (role-name the-role))
            errors))
    (values (endp errors) errors)))


(defmethod validate ((the-rule rule) &key (policy *current-policy*))
  (declare (ignore policy))
  (let ((errors '()))
    (when (and (null (rule-roles the-rule)) (null (rule-users the-rule)))
      (push (format nil "Rule '~A' has neither associated rules, no users."
                    (entity-name the-rule))
            errors))
    (values (endp errors) errors)))
