;;;;
;;;; Access control policy parser.
;;;;

(in-package :cl-user)

(defpackage secsrv.parser
  (:use :cl :alexandria :esrap :secsrv.policy :secsrv.sys)
  (:import-from :cl-log
                #:log-message)
  (:export
   #:parse-file
   #:parse-string
   #:load-policy-from-string))

(in-package :secsrv.parser)

;;; Data structures for parsed representation of ACL policy entities

(defclass <parsed-role> ()
  ((name :type string
         :initarg :name
         :reader role-name)
   (parameters
    :type list ; of strings
    :initarg :parameters
    :reader role-parameters
    :documentation "The list of concepts names."))
  (:documentation "Information about role."))


(defclass <parsed-access-path-item> ()
  ((attribute-name :type string
                   :initarg :attribute-name)
   (constraint
    :type (or <parsed-constraint> null)
    :initarg :constraint
    :reader pap-item-constraint)))

(defclass <parsed-constraint> ()
  ((constraint
    :type list
    :initarg :constraint
    :documentation "Tree representation of a boolean expression. At
    the top level, it is a list of the form (operation arg1 [arg2]),
    where operation is a string, where each arg is either an atomic
    expression (a constant, or `<parsed-access-path>'), or a
    sublist. Operations are: and, or, not, <, >, = etc.")))

(defclass <parsed-access-path> ()
  ((path :type list ; of `<parsed-access-path-item>'
         :initarg :path
         :documentation "List of <parsed-access-path-item>."))
  (:documentation ""))

(defclass <parsed-entity-attribute> ()
  ((name :type string
         :initarg :name
         :reader attribute-name)
   (column-name :type string
                :initarg :column-name
                :reader attribute-column-name)
   (primary-p
    :type '(or t nil)
    :initform nil
    :initarg :primary-p
    :reader attribute-primary-p)
   (referenced-table
    :type (or string null)
    :initarg :referenced-table
    :initform nil
    :reader attribute-referenced-table
    :documentation "The name of referenced table for foreing key and
    m2m attributes.")
   (m2m-table
    :type (or string null)
    :initform nil
    :initarg :m2m-table
    :documentation "The name of database table representing m2m
    relation between the table of cutrent attribute and
    referenced-table.")
   (m2m-forward-key
    :type (or string null)
    :initform nil
    :initarg :m2m-forward-key
    :documentation "Name of the m2m database column representing key to referenced-table.")
   (m2m-reverse-key
    :type (or string null)
    :initform nil
    :initarg :m2m-reverse-key
    :documentation "Name of the m2m database column representing key
    to the table this attribute belongs to."))
   (:documentation ""))


(defclass <parsed-entity> ()
  ((name :type string
         :initarg :name
         :reader entity-name)
   (table-name :type string
               :initarg :table-name
               :reader entity-table-name)
   (docstring :type (or null string)
              :initform nil
              :initarg :docstring
              :reader entity-docstring)
   (attributes
    :type list ; of <parsed-entity-attribute>
    :initarg :attributes
    :initform nil
    :reader concept-attributes
    :documentation "List of `<parsed-entity-attribute>'."))
  (:documentation ""))


(defclass <parsed-concept> ()
  ((name :type string
         :initarg :name
         :reader concept-name)
   (docstring :type (or null string)
              :initform nil
              :initarg :docstring
              :reader concept-docstring)
   (super :type list ; of strings
          :initarg :super
          :initform nil
          :reader concept-super)
   (constraint ;;:type (or null <parsed-constraint>)
               :initarg :constraint
               :initform nil
               :reader concept-constraint)
   (excluded-attributes
    :type list ; of strings
    :initarg :excluded-attributes
    :initform nil
    :reader concept-excluded-attributes
    :documentation "List of excluded attributes names.")
   (added-attributes
    :type list ; of (attribute-name . `<parsed-access-path>') conses
    :initarg :added-attributes
    :initform nil
    :reader concept-added-attributes
    :documentation "List of attributes added to the concept. Each item
    is a (attribute-name . access-path) pair, where attribute-name is
    a string and access-path is an instance of `<parsed-access-path>'."))
  (:documentation ""))

(defclass <parsed-cbac-rule> ()
  ((name :type string
         :initarg :name
         :reader rule-name)
   (docstring :type (or null string)
              :initform nil
              :initarg :docstring)
   (concept
    :type string
    :reader rule-concept-name
    :initarg :concept)
   (access-type
    :type '(member :allow :deny)
    :reader rule-access-type
    :initarg :access-type)
   (operations
    :type list ; of strings
    :initarg :operations
    :reader rule-operations
    :documentation "List of operations names addressed by the rule.")
   (grantees
    :type list
    :initarg :grantees
    :reader rule-grantees
    :documentation "List of grantee exressions: (type name
    access-paths). Type (user or role) and name are strings,
    access-paths are `<parsed-access-path>'.")
   (constraint
    ;;:type (or <parsed-constraint> null)
    :initform nil
    :reader rule-constraint
    :initarg :constraint))
  (:documentation "Parsed represetation of CBAC rule."))


(defun make-pap-constriant (constraint)
  (make-instance '<parsed-constraint>
                 :constraint constraint))

(defun make-pap-item (attribute-name constraint)
  (make-instance '<parsed-access-path-item>
                 :attribute-name attribute-name
                 :constraint constraint))

(defun make-pap (items)
  (make-instance
   '<parsed-access-path>
   :path (mapcar #'(lambda (name-constr) (apply #'make-pap-item name-constr)) items)))

;;;
;;; Parsing rules
;;;


;;;A semantic predicate for filtering out double quotes.
(defun not-doublequote (char)
  (not (eql #\" char)))

(defun not-integer (string)
  (when (find-if-not #'digit-char-p string)
    t))

;;;
;;; Auxilary parser rules
;;;
(defrule symbol (and alphanumeric (* (or alphanumeric #\- #\_)))
  (:lambda (list)
    (text list)))

(defrule alphanumeric (alphanumericp character))

(defrule comment (and ";" (* (not #\newline)))
  (:constant nil))

(defrule whitespace (and (+ (or #\space #\tab #\newline)) (? (and comment (? whitespace))))
  (:constant nil))

(defrule integer (and (? (or "+" "-")) (+ (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9")))
  (:lambda (list)
    (parse-integer (text list) :radix 10)))

(defrule string (or simple-string triple-quoted-string))

(defrule simple-string (and #\" (* string-char) #\")
  (:destructure (q1 string q2)
    (declare (ignore q1 q2))
    (text string)))

(defrule triple-quoted-string (and (and #\" #\" #\") (* string-char) (and #\" #\" #\"))
  (:destructure (q1 string q2)
    (declare (ignore q1 q2))
    (text string)))

(defrule string-char (or (not-doublequote character) (and #\\ #\")))

;;;
;;; Access path and filtering constraints
;;;
(defrule access-path
    (and ap-item (* (and "." ap-item)))
  (:destructure (head rest-items)
    (cons :access-path (cons head (mapcar #'second rest-items)))))

;; Used in boolean expressions to produce unified output for the operands
(defrule access-path-prefixed-output
    (and access-path)
  (:destructure (expr)
    expr))

(defrule ap-item
    (and symbol (? filtering-constraint))
  (:destructure (attribute-name filter)
     (cons attribute-name filter)))

(defrule filtering-constraint
    (and "[" (? whitespace) constraint-expression (? whitespace) "]")
  (:destructure (br1 ws1 expr ws2 br2)
    (declare (ignore ws1 ws2 br1 br2))
    expr))

(defrule constraint-expression
    (or boolean-expression)
  (:lambda (expr)
    (make-instance '<parsed-constraint>
                   :constraint expr)
    expr))

;; Boolean expression in the LISP coding style
(defrule polish-constraint-expression
    (or polish-boolean-expression)
  (:lambda (expr)
    (make-instance '<parsed-constraint>
                   :constraint expr)
    expr))

(defrule boolean-expression
    (or boolean-expression-logcon ; logical connection of two expressions
        boolean-expression-atom   ; atomic expression, e.g. x > 0
        boolean-expression-group  ; ( expr )
        boolean-expression-negation) ; not expr
  (:identity t))

(defrule boolean-expression-group
    (and "(" (? whitespace) boolean-expression (? whitespace) ")" (? whitespace))
  (:destructure (br1 ws1 expr ws2 br2 ws3)
    (declare (ignore br1 ws1 ws2 br2 ws3))
    expr))

(defrule boolean-expression-negation
    (and "not" whitespace boolean-expression)
  (:destructure (not ws expr)
    (declare (ignore ws))
    (list :operator not expr)))


(defrule boolean-expression-atom
    (and boolean-expression-operand
         (? whitespace) relational-operator (? whitespace)
         boolean-expression-operand)
  (:destructure (op1 ws1 operator ws2 op2)
      (declare (ignore ws1 ws2))
      (list :operator operator op1 op2)))

(defrule boolean-expression-operand
    (or constant
        quantified-access-path
        access-path-prefixed-output))

(defrule quantified-access-path
    (and set-function (? whitespace) "(" (? whitespace) access-path (? whitespace) ")")
  (:destructure (quantifier ws1 br1 ws2 access-path ws3 br2)
    (declare (ignore ws1 br1 ws2 ws3 br2))
    (list :operator quantifier access-path)))

(defrule boolean-expression-logcon
    (and boolean-expression (? whitespace)
         (or "and" "or")
         (? whitespace) boolean-expression)
  (:destructure (op1 ws1 connector ws2 op2)
    (declare (ignore ws1 ws2))
    (list :operator connector op1 op2)))

;; Boolean expression in the LISP coding style
(defrule polish-boolean-expression
    (or polish-boolean-expression-node ; function call
        polish-boolean-expression-atom)   ; atomic expression, e.g. access path or constant
  (:identity t))

(defrule polish-boolean-expression-node
    (and (and "(" (? whitespace))
         (or logcon relational-operator)
         (+ (and whitespace polish-boolean-expression))
         (and (? whitespace) ")"))
  (:destructure (open operator arguments close)
    (declare (ignore open close))
    (cons :operator
          (cons operator
                (mapcar #'second arguments)))))

(defrule polish-boolean-expression-atom
    (or constant access-path-prefixed-output))

(defrule relational-operator (or "=" ">=" "<=" "!=" ">" "<" "in" "not in"))

(defrule logcon (or "and" "or"))

(defrule set-function (or "some" "every" "exists" "the" "count"))

(defrule constant (or numeric-constant literal-constant)
  (:lambda (value)
    (cons :const value)))

(defrule numeric-constant (or integer))

(defrule literal-constant (or string))


;;;
;;; Entity
;;;

(defrule entity
    (and "(defmodel" whitespace symbol
         (? (and whitespace (? string))) ; docstring
         (* (and (? whitespace) entity-option))
         (? whitespace) ")")
  (:destructure (kw1 ws1 entity-name (ws2 docstring) options ws3 br2)
    (declare (ignore kw1 ws1 ws2 ws3 br2))
    (let ((options (loop :for (nil option) :in options :collect option)))
      (flet ((get-option (key) (cdr (assoc key options))))
        (make-instance '<parsed-entity>
                       :name entity-name
                       :docstring docstring
                       :table-name (get-option :table-name)
                       :attributes (get-option :attributes))))))


(defrule entity-option
    (or entity-table
        entity-attributes-list))

(defrule entity-table
    (and "(table" whitespace (or symbol string) (? whitespace) ")")
  (:destructure (kw1 ws1 table-name ws2 br2)
    (declare (ignore kw1 ws1 ws2 br2))
    (cons :table-name (text table-name))))

(defrule entity-attributes-list
    (and (and "(attributes" (? whitespace) "(")
         (+ (and (? whitespace) entity-attribute))
         (and (? whitespace) ")" (? whitespace) ")"))
  (:destructure (init-kws attributes end-kws)
    (declare (ignore init-kws end-kws))
    (cons :attributes
          (loop :for (nil attribute) :in attributes :collect attribute))))

(defrule entity-attribute
    (and (and "(" (? whitespace))
         (and symbol whitespace symbol) ; attribute name, column name
         (* (or attribute-foreign-key
                attribute-primary-key))
         (and (? whitespace) ")"))
  (:destructure (br1 (attribute-name ws1 column-name) attribute-options br2)
    (declare (ignore br1 ws1 br2))
    (flet ((get-option (key) (cdr (assoc key attribute-options))))
      (make-instance '<parsed-entity-attribute>
                     :name attribute-name
                     :column-name column-name
                     :primary-p (get-option :primary-key)
                     :referenced-table (get-option :referenced-table)))))

(defrule attribute-foreign-key
    (and (and whitespace "references" whitespace)
         (or symbol string))
  (:destructure (kw1 referenced-table-name)
    (declare (ignore kw1))
    (cons :referenced-table
          (text referenced-table-name))))

(defrule attribute-primary-key
    (and (and whitespace "primary" whitespace "key"))
  (:constant (cons :primary-key t)))


;;;
;;; Role
;;;

(defrule role
    (and "(defrole" whitespace symbol
         (* (and whitespace (or symbol string)))
         (? whitespace) ")")
  (:destructure (kw1 ws1 role-name parameters ws2 br2)
    (declare (ignore kw1 ws1 ws2 br2))
    (make-instance '<parsed-role>
                   :name role-name
                   :parameters (loop :for (nil parameter) :in parameters :collect parameter))))

;;;
;;; Concepts
;;;

(defrule concept
    (and "(defconcept" whitespace symbol
         (? whitespace) (? string) ; docstring
         (* (and (? whitespace) concept-option))
         (? whitespace) ")")
  (:destructure (kw1 ws1 concept-name ws2 docstring options ws3 br2)
    (declare (ignore kw1 ws1 ws2 ws3 br2))
    (let ((options (loop :for (nil option) :in options :collect option)))
      (flet ((get-option (key) (cdr (assoc key options))))
        (make-instance '<parsed-concept>
                       :name concept-name
                       :docstring docstring
                       :super (get-option :inheritance)
                       :constraint (get-option :constraint)
                       :excluded-attributes (get-option :excluded-attributes)
                       :added-attributes (get-option :added-attributes))))))

(defrule concept-option
    (or concept-inheritance
        concept-constraint
        concept-added-attributes
        concept-removed-attributes))

(defrule concept-inheritance
    (and "(" "inherits" (+ (and whitespace symbol)) (? whitespace) ")")
  (:destructure (br1 kw1 super-names ws2 br2)
    (declare (ignore br1 kw1 ws2 br2))
    (cons :inheritance (loop :for (nil concept-name) :in super-names :collect concept-name))))

(defrule concept-constraint
    (and "(" "constraint" whitespace boolean-expression (? whitespace) ")")
  (:destructure (br1 kw1 ws1 expr ws2 br2)
    (declare (ignore br1 kw1 ws1 ws2 br2))
    (cons :constraint expr)))

(defrule concept-added-attributes
    (and "(" "attributes"
         (+ (and (? whitespace) "(" (? whitespace) symbol whitespace access-path (? whitespace) ")"))
         (? whitespace) ")")
  (:destructure (br1 kw1 attributes-defs ws2 br2)
    (declare (ignore br1 kw1 ws2 br2))
    (cons :added-attributes
          (loop
             :for (nil nil nil attribute-name nil access-path nil nil) :in attributes-defs
             :collect (cons attribute-name access-path)))))

(defrule concept-removed-attributes
    (and "(" "exclude" (+ (and whitespace symbol)) (? whitespace) ")")
  (:destructure (br1 kw1 attributes-names ws2 br2)
    (declare (ignore br1 kw1 ws2 br2))
    (cons :removed-attribute
          (loop :for (nil attribute-name) :in attributes-names :collect attribute-name))))

;;;
;;; CBAC rule
;;;

(defrule cbac-rule
    (and (or "(rule" "(defrule") whitespace (or symbol string)
         (? (and whitespace (? string))) ; docstring
         (* (and (? whitespace) cbac-rule-option))
         (? whitespace) ")")
  (:destructure (kw1 ws1 rule-name (ws2 docstring) options ws3 br2)
    (declare (ignore kw1 ws1 ws2 ws3 br2))
    (let ((options (loop :for (nil option) :in options :collect option)))
      (flet ((get-option (key) (cdr (assoc key options))))
        (make-instance '<parsed-cbac-rule>
                       :name (text rule-name)
                       :docstring docstring
                       :access-type (get-option :access-type)
                       :concept (get-option :concept)
                       :operations (get-option :operations)
                       :grantees (get-option :grantees)
                       :constraint (get-option :constraint))))))

(defrule cbac-rule-option
    (or cbac-rule-access
        cbac-rule-concept
        cbac-rule-grantees-list
        cbac-rule-operations
        cbac-rule-constraint))

(defrule cbac-rule-access
    (and "(access" whitespace (or "allow" "deny") (? whitespace) ")")
  (:destructure (kw1 ws1 access-type ws2 br2)
    (declare (ignore kw1 ws1 ws2 br2))
    (cons :access-type (if (string= (text access-type) "allow") :allow :deny))))

(defrule cbac-rule-concept
    (and "(concept" whitespace (or symbol string) (? whitespace) ")")
  (:lambda (list)
    (cons :concept (text (elt list 2)))))

(defrule cbac-rule-operations
    (and "(operations" (+ (and whitespace (or symbol string))) (and (? whitespace) ")"))
  (:destructure (kw1 operations kw2)
    (declare (ignore kw1 kw2))
    (cons :operations
          (mapcar #'second operations))))

(defrule cbac-rule-constraint
    (and "(constraint" whitespace (or constraint-expression polish-constraint-expression) (? whitespace) ")")
  (:lambda (list)
    (cons :constraint (elt list 2))))

(defrule cbac-rule-grantees-list
    (and "(grantees" (* (and (? whitespace) cbac-rule-grantee)) (and (? whitespace) ")"))
  (:destructure (kw1 grantees kw2)
    (declare (ignore kw1 kw2))
    (cons :grantees
          (mapcar #'second grantees))))

(defrule cbac-rule-grantee (or cbac-rule-grantee-user cbac-rule-grantee-role))

(defrule cbac-rule-grantee-user
    (and "(" (? whitespace) "user" whitespace (or symbol string) (? whitespace) ")")
  (:destructure (br1 ws1 kw1 ws2 user-name ws3 br2)
    (declare (ignore br1 ws1 kw1 ws2 ws3 br2))
    (list :user (text user-name))))

(defrule cbac-rule-grantee-role
    (and (and "(" (? whitespace) "role" whitespace) (or symbol string)
         (* (and whitespace access-path-prefixed-output)) (? whitespace) ")")
  (:destructure (kw1 role-name access-paths ws3 br2)
    (declare (ignore kw1 ws3 br2))
    (cons :role (cons role-name
                      (loop :for (nil ap) :in access-paths :collect ap)))))

;;;
;;; Top level expressions
;;;
(defrule acl-statement (or entity role concept cbac-rule))

(defrule policy
    (* (and (? whitespace) (or acl-statement comment) (? whitespace)))
  (:lambda (statements)
    (loop :for expr :in statements
       :when (second expr) :collect (second expr))))


;;;
;;; File loading
;;;

(defun read-file (filename)
  "Reads file's content and return it as a string."
  (with-open-file (s filename)
    (text
     (loop :for line = (read-line s nil nil) :while line :collect line :collect (string #\newline)))))


(defun iterate-parsed-instances (seq test-or-class function)
  "Evaluate FUNCTION on every item of SEQ if it passes filtering
condition specified by TEST-OR-CLASS."
  (dolist (item seq)
    (when (or (and (symbolp test-or-class)
                   (subtypep (type-of item) test-or-class))
              (and (listp test-or-class)
                   (member (type-of item) test-or-class))
              (and (functionp test-or-class)
                   (funcall test-or-class item)))
      (funcall function item))))


(defun add-concept-no-fk-attributes (parsed-entity policy)
  "Add all concepts entities and their attributes to the policy. Attributes are
added without foreign key references."
  (check-type parsed-entity (or <parsed-entity> <parsed-concept>))
  (let* ((is-entity (subtypep (type-of parsed-entity) '<parsed-entity>))
         (super (if is-entity
                    nil
                    (mapcar #'(lambda (name)
                                (let ((parent (find-concept name :policy policy)))
                                  (unless parent
                                    (inconsistent-policy-error "Concapt name `~A' not found." name))
                                  parent))
                            (concept-super parsed-entity))))
         (name (if is-entity (entity-name parsed-entity) (concept-name parsed-entity)))
         (entity (if is-entity
                     (make-entity name (entity-table-name parsed-entity))
                     (make-concept name :super super))))
    ;; add all attributes as non foreign key
    (when (subtypep (type-of parsed-entity) '<parsed-entity>)
      (dolist (att (concept-attributes parsed-entity))
        (add-attribute entity
                       (attribute-name att)
                       (make-entity-attribute (attribute-name att)
                                              entity
                                              (attribute-column-name att)
                                              :primary-key-p (attribute-primary-p att)))))
    (add-concept policy name entity)))


(defun link-all-foreign-keys (parsed-entity policy)
  "Set up relations between entities properly."
  (check-type parsed-entity <parsed-entity>)
  (loop
     :with entity = (find-concept (entity-name parsed-entity) :policy policy)
     :for parsed-att :in (concept-attributes parsed-entity)
     :for referenced-table = (attribute-referenced-table parsed-att)
     :when referenced-table :do
     (register-fk-relation (find-attribute entity (attribute-name parsed-att))
                           (find-concept referenced-table :policy policy))))

(defun process-path-attributes (parsed-concept policy)
  (let ((concept (find-concept (concept-name parsed-concept) :policy policy)))
    (loop :for (name . ap) :in (concept-added-attributes parsed-concept)
       :do ;;(verify-access-path-expression concept ap)
       (add-attribute concept name (make-relational-attribute name concept ap))))
  t)

(defun process-concept-constraint (parsed-concept policy)
  (let ((concept (find-concept (concept-name parsed-concept) :policy policy))
        (constraint (concept-constraint parsed-concept)))
    (when (and constraint (verify-filtering-constraint concept constraint))
      (set-concept-constraint concept constraint)))
  t)

(defun process-role (parsed-role policy)
  (let ((concepts (mapcar #'(lambda (cname)
                              (find-concept cname :policy policy))
                          (role-parameters parsed-role))))
    (apply #'policy-add-role policy (role-name parsed-role) concepts))
  t)

(defun process-access-rule (parsed-rule policy)
  (with-accessors ((name rule-name)
                   (cname rule-concept-name)
                   (access-mode rule-access-type)
                   (operations rule-operations)
                   (grantees rule-grantees)
                   (constraint rule-constraint))
      parsed-rule
    (let ((granted-roles (mapcar #'rest (remove-if #'(lambda (g) (eq (car g) :user)) grantees)))
          (granted-users (mapcar #'second (remove-if #'(lambda (g) (eq (car g) :role)) grantees))))
      (policy-add-create-rule
       policy name access-mode (find-concept cname :policy policy) operations
       :granted-users granted-users
       :granted-roles granted-roles
       :constraint constraint)))
  t)



(defun parse-string (string)
  "Parse CBAC policy expression from STRING."
  (let ((statements (parse 'policy string)))
    statements))

(defun load-policy-from-string (string)
  (let ((statements (parse 'policy string))
        (policy (make-policy)))
    ;; First pass on concepts: add concepts hierarchy and entities'
    ;; direct attributes. Do not process fk relations because foreign
    ;; keys could define circular relations, so related entities could
    ;; not be define so far.
    (iterate-parsed-instances statements
                              '(<parsed-entity> <parsed-concept>)
                              #'(lambda (entity) (add-concept-no-fk-attributes entity policy)))
    ;; Add all FK attributes
    (iterate-parsed-instances statements
                              '<parsed-entity>
                              #'(lambda (entity) (link-all-foreign-keys entity policy)))
    ;; Add all path-defined attributes defined for concepts
    (iterate-parsed-instances statements
                              '<parsed-concept>
                              #'(lambda (entity) (process-path-attributes entity policy)))
    ;; Add all concept-level constraints
    (iterate-parsed-instances statements
                              '<parsed-concept>
                              #'(lambda (concept) (process-concept-constraint concept policy)))
    ;; Add all roles
    (iterate-parsed-instances statements
                              '<parsed-role>
                              #'(lambda (role) (process-role role policy)))
    ;; Add all access rules
    (iterate-parsed-instances statements
                              '<parsed-cbac-rule>
                              #'(lambda (rule) (process-access-rule rule policy)))
    policy))

(defun parse-file (filename)
  "Parse the entire content of the CBAC policy file specified by the
FILENAME and returns `<policy>' object. Signal a condition in case of
error."
  (load-policy-from-string (read-file filename)))
