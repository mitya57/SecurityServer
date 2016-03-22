(in-package :secsrv)

(defun main ()
  (let (;;(model-filename "test/policies/istina.model.acl")
        (acl-filename "test/policies/test.acl")
        ;; for oracle use :odbc and connect string like ("host" "username" "password")
        (*database-info* '(:type :sqlite3
                         :connect-string "test/databases/istina.sqlite")))
    (setup-logging)
    (log-message :info "Starting")

    (setf *sql-trace* t)
    (setf *current-policy* (secsrv.parser::parse-file acl-filename))

    (start-server *database-info* :port 8135)

    (with-checker (the-checker *database-info* :policy *current-policy*)
      (print (time (has-access the-checker "safonin" "article" 211444 "delete")))
      (print (time (has-access the-checker "safonin" "book" 211916 "delete"))))

    #+or(with-checker (the-checker database-info :policy *current-policy*)
      (print (time (has-access the-checker "safonin" "article" 217019 "delete"))))


    ;; some quick tests
    #+or(with-checker (the-checker database-info :policy *current-policy*)
      (let ((access-granted? (has-access the-checker "safonin" "article" 211444 "delete"))
            (article-model (find-model "Article"))
            (user-model (find-model "User"))
            (*sql-trace* t))
        (format t "~&~%Check for user.articles attribute existance: ~A=~A"
                (attribute-column-name (find-attribute "articles" user-model))
                (find-attribute "articles" user-model))
        (format t "~&Object attribute: ~A~%"
                (get-object-attribute (find-model "Article") 211444 "owner.name"))
        (format t "Access ~:[DENIED~;GRANTED~]~%" access-granted?)

        (let ((access-path (make-access-path)))
          (parse access-path "owner.name" :top-level-model article-model)
          (print (access-path-prepared-sql access-path)))

        (let ((access-path (make-access-path)))
          (parse access-path "user.articles.id"
                 :top-level-model (list (cons "user" user-model)))
          (print (access-path->prepared-sql access-path))
          (print (evaluate-access-path access-path user-model 63)))
        access-granted?))))
