(in-package :secsrv-test)

(defvar *testing-db* nil)

(deftestsuite parser (root)
  () ; no suite-specific slots
  (:documentation
   "Unit tests for access policy parsing."))

(addtest (parser)
  parse-simple-access-paths
  (dolist (expr '("a.b.c.d"
                  "test.long.names"))
    (esrap:parse 'secsrv.parser::access-path expr)))

(addtest (parser)
  parse-constants
  (dolist (expr '("a.b.c.d"
                  "test.long.names"))
    (esrap:parse 'secsrv.parser::access-path expr)))

(addtest (parser)
  parse-access-paths-with-simple-filters
  (dolist (expr '("a.b[x > 0].c.d"
                  "a.b[x > 0].c[y<1].d"))
    (esrap:parse 'secsrv.parser::access-path expr)))

(addtest (parser)
  parse-access-paths-with-boolean-filters
  (dolist (expr '("a.b[x > 0 and y < 1].c.d"
                  "a.b[x > 0 and (y < 1)].c.d"
                  "a.b[(x > 0 and y < 1)].c.d"
                  "a.b[(x > 0 or y < 1) and (x > y)].c.d"))
    (esrap:parse 'secsrv.parser::access-path expr)))

(addtest (parser)
  parse-nested-access-paths
  (dolist (expr '("a.b[x.y.z > 0].c.d"
                  "a.b[0 > x.y.z].c.d"
                  "a.b[x.y.z > x.y.z].c.d"
                  "a[x.y[a>0 and b.x[123 < att] > 2].z > x.y.z].b[0 > x.y.z].c[x.y.z > 0].d[x.y.z <= 0]"))
    (esrap:parse 'secsrv.parser::access-path expr)))

;;; Entity parser

(addtest (parser)
  parse-model-definition
  (secsrv.parser:parse-string "(defmodel post
      (table \"manspost\")
      (attributes
        ((id f-manspost-id)
         (man f_man_id references man)
         (department f_department_id references department))))"))


;;; Concept parser

(addtest (parser)
  parse-concept-definition
  (secsrv.parser:parse-string
   "(defconcept test-1
     \"It's just a comment with an escaped double-quote inside.\"
     (inherits base-model-1 base-model-2)
     (constraint a < b)
     (attributes
       (leaderName participants[role=\"leader\"].employee.lastname))
     (exclude startDate))"))


;;; Rule parser

(addtest (parser)
  parse-cbac-rule
  (secsrv.parser:parse-string
   "(rule owner-delete-article
  \"Owner can delete article.\"
  (access allow)
  (concept article)
  (grantees (role \"stuff\" object.authoras.man.posts.department)
            (role \"registred-user\")
            (role \"admin\" object.authoras.man.posts.department))
  (operations delete edit)
  (constraint object.owner = 63))"))

(addtest (parser)
  parse-cbac-rule-polish-constraint
  (secsrv.parser:parse-string
   "(rule owner-delete-article
  \"Owner can delete article.\"
  (access allow)
  (concept article)
  (grantees (role \"stuff\" object.authoras.man.posts.department)
            (role \"registred-user\")
            (role \"admin\" object.authoras.man.posts.department))
  (operations delete edit)
  (constraint (and (= object.owner 63)
                   (in 100 object.authoras.man[id<100].posts.department))))"))
