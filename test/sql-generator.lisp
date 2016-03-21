
(defun can-edit-article (article-id user-id)
  (format nil
          "SELECT 1
FROM article
JOIN authora ON (authora.f_article_id = article.f_article_id)
JOIN workers_profile ON (workers_profile.worker_id = authora.f_man_id)
WHERE article.f_article_id = (SELECT min(f_article_id) FROM article WHERE f_article_id >= ~D)
  AND workers_profile.user_id = ~D"
          article-id user-id))

(defun can-edit-article-admin (article-id user-id)
  (format nil
          "SELECT 1
FROM article
JOIN authora ON (authora.f_article_id = article.f_article_id)
JOIN manspost ON (manspost.f_man_id = authora.f_man_id)
WHERE article.f_article_id = (SELECT min(f_article_id) FROM article WHERE f_article_id >= ~D)
  AND manspost.f_department_id IN (
        SELECT gp.f_department_id
        FROM v_all_granted_roles gp
        WHERE gp.user_id = ~D
      )"
          article-id user-id))

(defun can-not-edit-article (article-id user-id)
  (format nil
          "SELECT 1
FROM  statistics_rankedarticleAUF89F
WHERE article_id = (SELECT min(f_article_id) FROM article where f_article_id >= ~D)
AND NOT EXISTS (~A)"
          article-id
          (can-edit-article-admin article-id user-id)))

(defun can-use-formula (formula-id user-id)
  (format nil
          "SELECT 1
FROM pointsformula
WHERE pointsformula.f_pointsformula_id IN (
      SELECT f_pointsformula_id
      FROM pfdepartmentlink
      WHERE f_pointsformula_id = (SELECT max(f_pointsformula_id) FROM pointsformula WHERE f_pointsformula_id <= ~D))
OR pointsformula.f_pointsformula_user IN (
      SELECT user_id
      FROM v_my_users_via_role
      WHERE f_permissionstypes_name = 'is_representative_for'
        AND role_user_id = ~D
      )"
          formula-id user-id))

(defun can-browse-rating (worker-id user-id)
  (format nil
          "SELECT 1
FROM v_my_users_via_role
WHERE f_permissionstypes_name IN ('can_view_rating', 'is_representative_for')
  AND f_man_id = (SELECT max(f_man_id) FROM man WHERE f_man_id <= ~D)
  AND role_user_id = ~D"
          worker-id user-id))


(defun generate-test ()
  (loop
     :with generators = `((,#'can-edit-article ,#'can-not-edit-article)
                          (,#'can-edit-article ,#'can-edit-article-admin ,#'can-not-edit-article)
                          (,#'can-use-formula ,#'can-browse-rating))
     :for k :below 10
     :for requester = (if (random 5)
                          (+ 100 (random 20000))
                          (elt '(54290 18889 63 354 36176 16271 16238) (random 7)))
     :for object-id = (+ 201113 (random 5000000))
     :for fs = (elt generators (random (length generators)))
     :do
     (format t "~{~A~%/~%~}~%"
             (mapcar #'(lambda (f)
                         (funcall f object-id requester))
                     fs))))
