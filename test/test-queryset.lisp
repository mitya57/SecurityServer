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
        (journal-join (secsrv.queryset::make-join
                        secsrv.queryset::+left-join+
                        "journal" ;; table
                        "T1" ;; alias
                        (make-instance 'secsrv.queryset::<join-relation>
                          :left (make-instance 'secsrv.queryset::<join-operand> :expression "T0.f_journal_id")
                          :operator "="
                          :right (make-instance 'secsrv.queryset::<join-operand> :expression "T1.f_journal_id"))))
        (collection-join (secsrv.queryset::make-join
                           secsrv.queryset::+join+
                           (secsrv.queryset::make-default-queryset "collection")
                           "T2" ;; alias
                           (make-instance 'secsrv.queryset::<join-relation>
                             :left (make-instance 'secsrv.queryset::<join-operand> :expression "T0.f_collection_id")
                             :operator "="
                             :right (make-instance 'secsrv.queryset::<join-operand> :expression "T2.f_collection_id"))))
        (expected-query (concatenate-newline
                          "SELECT f_article_id AS id"
                          "FROM article AS T0"
                          "LEFT JOIN journal T1 ON (T0.f_journal_id = T1.f_journal_id)"
                          "JOIN (SELECT *"
                          "FROM collection AS T0) T2 ON (T0.f_collection_id = T2.f_collection_id)")))
    (secsrv.queryset::add-select-expression queryset "f_article_id" "id")
    (setf (secsrv.queryset::queryset-joins queryset)
          (append (secsrv.queryset::queryset-joins queryset) `(,journal-join ,collection-join)))
    (ensure-same expected-query (secsrv.queryset::sql<-queryset queryset))))
