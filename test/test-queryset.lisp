(in-package :secsrv-test)

(deftestsuite queryset (root)
  () ; no suite-specific slots
  (:documentation
   "Unit tests for compiling querysets to SQL."))

(defun concatenate-newline (&rest strings-list)
  (format nil "~{~A~^~%~}" strings-list)
)

(addtest (queryset)
  create-queryset
  (let ((queryset (make-instance 'secsrv.queryset::<queryset>
                   :main-table "article"))
        (join (secsrv.queryset::make-join
                 secsrv.queryset::+left-join+
                 (secsrv.queryset::make-default-queryset "journal")
                 "journal" ;; alias
                 (make-instance 'secsrv.queryset::<join-relation>
                   :left (make-instance 'secsrv.queryset::<join-operand> :expression "article.f_journal_id")
                   :operator "="
                   :right (make-instance 'secsrv.queryset::<join-operand> :expression  "journal.f_journal_id"))))
        (expected-query (concatenate-newline
                          "SELECT f_article_id AS id"
                          "FROM article AS T0"
                          "LEFT JOIN (SELECT *"
                          "FROM journal AS T0) journal ON (article.f_journal_id = journal.f_journal_id)")))
    (secsrv.queryset::add-select-expression queryset "f_article_id" "id")
    (setf (secsrv.queryset::queryset-joins queryset) (append (secsrv.queryset::queryset-joins queryset) `(,join)))
    (ensure-same expected-query (secsrv.queryset::sql<-queryset queryset))))
