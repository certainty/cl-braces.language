(in-package :tests.frontend.parser)

(defun scan-input (input)
  (let* ((token-ids (support:make-id-generator))
         (span-map (span:make-span-map))
         (scanner (scanner:make-scanner input token-ids span-map)))
    (multiple-value-bind (tokens had-errors) (scanner:all-tokens scanner)
      (assert-false had-errors)
      tokens)))

(defun parse-as-expression (input)
  (parse input #'parser::<expression))

(defun parse-as-statements (input)
  (parse input #'parser::<statement-list))

(defun parse (input production)
  (let* ((node-ids (support:make-id-generator))
         (node-spans (span:make-span-map))
         (tokens (scan-input input))
         (parser (parser:make-parser tokens node-ids node-spans)))
    (parser:parse parser :production production)))

(define-test parse-integer-literal ()
  "Parse an integer literal"
  (let ((node (parse-as-expression "3")))
    (assert-equal 'ast:literal (type-of node))
    (assert-equal 3 (ast:literal-value node))))

(define-test parse-unary-minus ()
  "Parse simple unary minus"
  (let ((node (parse-as-expression "-3")))
    (assert-eql 'ast:unary-expression (type-of node))
    (assert-eql token:@MINUS (token:class (ast:unary-expression-operator node)))
    (assert-eql 'ast:literal (type-of (ast:unary-expression-operand node)))))

(define-test parse-unary-plus ()
  "Parse simple unary plus"
  (let ((node (parse-as-expression "+3")))
    (assert-eql 'ast:unary-expression (type-of node))
    (assert-eql token:@PLUS (token:class (ast:unary-expression-operator node)))
    (assert-eql 'ast:literal (type-of (ast:unary-expression-operand node)))))

(define-test parse-grouping ()
  "Parse grouping expression with two operands"
  (let ((node (parse-as-expression "(3 - 4)")))
    (assert-eql 'ast:grouping-expression (type-of node))
    (assert-eql 'ast:binary-expression (type-of (ast:grouping-expression-expression node)))))

(define-test parse-binary-plus ()
  "Parse binary plus with two operands"
  (let ((node (parse-as-expression "3 + 4")))
    (assert-eql 'ast:binary-expression (type-of node))
    (assert-eql 'ast:literal (type-of (ast:binary-expression-lhs node)))
    (assert-eql token:@PLUS (token:class (ast:binary-expression-operator node)))
    (assert-eql 'ast:literal (type-of (ast:binary-expression-rhs node)))))

(define-test parse-binary-minus ()
  "Parse binary minus with two operands"
  (let ((node (parse-as-expression "3 - 4")))
    (assert-eql 'ast:binary-expression (type-of node))
    (assert-eql 'ast:literal (type-of (ast:binary-expression-lhs node)))
    (assert-eql token:@MINUS (token:class (ast:binary-expression-operator node)))
    (assert-eql 'ast:literal (type-of (ast:binary-expression-rhs node)))))

(define-test parse-binary-with-mixed-precedence-operators ()
  "Parse binary expressions with operators with mixed precedence"
  (let ((node (parse-as-expression "3 - 4 * 5")))
    (assert-eql 'ast:binary-expression (type-of node))
    (assert-eql token:@MINUS (token:class (ast:binary-expression-operator node)))

    (let ((lhs (ast:binary-expression-lhs node))
          (rhs (ast:binary-expression-rhs node)))
      (assert-eql 'ast:literal (type-of lhs))
      (assert-eql 'ast:binary-expression (type-of rhs))

      (let ((rhs-lhs (ast:binary-expression-lhs rhs))
            (rhs-rhs (ast:binary-expression-rhs rhs)))
        (assert-eql 'ast:literal (type-of rhs-lhs))
        (assert-eql token:@STAR (token:class (ast:binary-expression-operator rhs)))
        (assert-eql 'ast:literal (type-of rhs-rhs))))))

(define-test parse-binary-with-explicit-grouping ()
  "Parse a binary expression that has explicit grouping"
  (let ((node (parse-as-expression "(3 - 4) * 5")))
    (assert-eql 'ast:binary-expression (type-of node))
    (assert-eql token:@STAR (token:class (ast:binary-expression-operator node)))
    (let ((lhs (ast:binary-expression-lhs node))
          (rhs (ast:binary-expression-rhs node)))
      (assert-eql 'ast:grouping-expression (type-of lhs))
      (assert-eql 'ast:literal (type-of rhs))

      (let ((inner (ast:grouping-expression-expression lhs)))
        (assert-eql 'ast:binary-expression (type-of inner))
        (assert-eql token:@MINUS (token:class (ast:binary-expression-operator inner)))
        (assert-eql 'ast:literal (type-of (ast:binary-expression-lhs inner)))
        (assert-eql 'ast:literal (type-of (ast:binary-expression-rhs inner)))))))

(define-test parse-reprodcue-bug ()
  (multiple-value-bind (ast had-errors) (parse-as-statements "(3 + 3) * 3")
    (declare (ignore ast))
    (assert-false had-errors)))

(define-test parse-short-assignment ()
  (multiple-value-bind (ast had-errors) (parse-as-statements "a := 3")
    (assert-false had-errors)
    (assert-eql 'ast:statement-list (type-of ast))
    (let ((statements (ast:statement-list-statements ast)))
      (assert-eql 1 (length statements))
      (let ((decl (first statements)))
        (assert-eql 'ast:short-variable-declaration (type-of decl))))))

(define-test parse-if-conditional ()
  "Parse simple if conditional without else"
  (multiple-value-bind (statements had-errors) (parse-as-statements "if 3 { 1 } ")
    (assert-false had-errors)
    (let ((decl (first (ast:statement-list-statements statements))))
      (assert-eql 'ast:if-statement (type-of decl)))))

(define-test parse-if-else-conditional ()
  "Parse simple if conditional with else"
  (multiple-value-bind (statements had-errors) (parse-as-statements "if x { 1 } else { 2 }")
    (assert-false had-errors)
    (let ((decl (first (ast:statement-list-statements statements))))
      (assert-eql 'ast:if-statement (type-of decl))
      ;; make sure we have an else statement
      (assert (not (null (ast:if-statement-alternative decl)))))))

(define-test parse-if-with-short-statement ()
  "Parse if conditional with short statement"
  (multiple-value-bind (statements had-errors) (parse-as-statements " if x := 10; x < 20 { x }")
    (assert-false had-errors)
    (let ((decl (first (ast:statement-list-statements statements))))
      (assert-eql 'ast:if-statement (type-of decl))
      ;; check th init and condition part
      (assert-eql 'ast:short-variable-declaration (type-of (ast:if-statement-init decl)))
      (assert-eql 'ast:binary-expression (type-of (ast:if-statement-condition decl))))))

(define-test parse-variable-declaration ()
  "Parse variable declaration for single variable"
  (multiple-value-bind (statements had-errors) (parse-as-statements "var x int")
    (assert-false had-errors)
    (let ((decl (first (ast:statement-list-statements statements))))
      (assert-eql 'ast:variable-declaration (type-of decl))
      (assert-eql 1 (length (ast:variable-declaration-specifications decl)))
      (let ((var (first (ast:variable-declaration-specifications decl))))
        (assert-eql 'ast:variable-specification (type-of var))))))

(define-test parse-variable-declaration-with-assignment ()
  "Parse variable declaration with assignment"
  (multiple-value-bind (statements had-errors) (parse-as-statements "var x int = 10")
    (assert-false had-errors)
    (let ((decl (first (ast:statement-list-statements statements))))
      (assert-eql 'ast:variable-declaration (type-of decl))
      (assert-eql 1 (length (ast:variable-declaration-specifications decl)))
      (let ((var (first (ast:variable-declaration-specifications decl))))
        (assert-eql 'ast:variable-specification (type-of var))))))

(define-test parse-variable-declaration-with-assignment-and-type-inference ()
  "Parse variable declaration with assignment and type inference"
  (multiple-value-bind (statements had-errors) (parse-as-statements "var x = 10")
    (assert-false had-errors)
    (let ((decl (first (ast:statement-list-statements statements))))
      (assert-eql 'ast:variable-declaration (type-of decl))))
  )

(define-test parse-variable-declaration-with-multiple-variables ()
  "Parse variable declaration with multiple variables"
  (multiple-value-bind (statements had-errors) (parse-as-statements "var x, y int")
    (assert-false had-errors)
    (let ((decl (first (ast:statement-list-statements statements))))
      (assert-eql 'ast:variable-declaration (type-of decl)))))

(define-test parse-variable-declaration-with-multiple-variables-and-assignment ()
  "Parse variable declaration with multiple variables and assignment"
  (multiple-value-bind (statements had-errors) (parse-as-statements "var x, y int = 10, 20")
    (assert-false had-errors)
    (let ((decl (first (ast:statement-list-statements statements))))
      (assert-eql 'ast:variable-declaration (type-of decl))
      (assert-eql 1 (length (ast:variable-declaration-specifications decl)))
      (let ((spec (first (ast:variable-declaration-specifications decl))))
        (assert-eql 'ast:variable-specification (type-of spec))
        (assert-eql 2 (length (ast:identifier-list-identifiers (ast:variable-specification-identifiers spec))))))))


(define-test parse-variable-declaration-with-group-of-variables ()
  "Parse variable declaration with group of variables"
  (multiple-value-bind (statements had-errors) (parse-as-statements "var (x int; y int = 10)")
    (assert-false had-errors)
    (let ((decl (first (ast:statement-list-statements statements))))
      (assert-eql 'ast:variable-declaration (type-of decl))
      (assert-eql 2 (length (ast:variable-declaration-specifications decl))))))

(define-test parse-variable-declaration-with-placeholder ()
  "Parse variable declaration with placeholder"
  (multiple-value-bind (statements had-errors) (parse-as-statements "var _, x = 1,2")
    (assert-false had-errors)
    (let ((decl (first (ast:statement-list-statements statements))))
      (assert-eql 'ast:variable-declaration (type-of decl)))))

(define-test parse-assignment ()
  "Parse simple assignment"
  (multiple-value-bind (statements had-errors) (parse-as-statements "x = 10")
    (assert-false had-errors)
    (let ((decl (first (ast:statement-list-statements statements))))
      (assert-eql 'ast:assignment-statement (type-of decl)))))

(define-test parse-assignment-with-addition ()
  "Parse assignment with addition"
  (multiple-value-bind (statements had-errors) (parse-as-statements "x += 10")
    (assert-false had-errors)
    (let ((decl (first (ast:statement-list-statements statements))))
      (assert-eql 'ast:assignment-statement (type-of decl))
      (assert-eql token:@PLUS_EQUAL (token:class (ast:assignment-statement-operator decl))))))

(define-test parse-assignment-with-multiplication ()
  "Parse assignment with multiplication"
  (multiple-value-bind (statements had-errors) (parse-as-statements "x *= 10")
    (assert-false had-errors)
    (let ((decl (first (ast:statement-list-statements statements))))
      (assert-eql 'ast:assignment-statement (type-of decl))
      (assert-eql token:@MUL_EQUAL (token:class (ast:assignment-statement-operator decl))))))

(define-test parse-assignment-with-multiple-values ()
  "Parse assignment with multiple values"
  (multiple-value-bind (statements had-errors) (parse-as-statements "x, y = 10, 20")
    (assert-false had-errors)
    (let ((decl (first (ast:statement-list-statements statements))))
      (assert-eql 'ast:assignment-statement (type-of decl))
      (assert-eql 2 (length (ast:expression-list-expressions (ast:assignment-statement-lhs decl)))))))

(define-test parse-assigment-with-multiple-values-and-placeholders ()
  "Parse assignment with multiple values and placeholders"
  (multiple-value-bind (statements had-errors) (parse-as-statements "x, _ = 10, 20")
    (assert-false had-errors)
    (let* ((decl (first (ast:statement-list-statements statements)))
           (lhs (ast:assignment-statement-lhs decl)))
      (assert-eql 'ast:expression-list (type-of lhs))
      (assert-eql 2 (length (ast:expression-list-expressions lhs))))))


(define-test parse-simple-function-declaration ()
  (multiple-value-bind (decl had-errors) (parse "func f() {}"  #'parser::<function-declaration)
    (assert-false had-errors)
    (assert-eql 'ast:function-declaration (type-of decl))
    (assert-eql 'ast:identifier (type-of (ast:function-declaration-name decl)))
    (assert-eql 'ast:function-signature (type-of (ast:function-declaration-signature decl)))
    (assert-eql 'ast:block (type-of (ast:function-declaration-body decl)))))

(define-test parse-function-with-various-types ()
  (multiple-value-bind (decl had-errors) (parse "func f(a int, b string) (int, string) { 10 }"  #'parser::<function-declaration)
    (assert-false had-errors)
    (assert-eql 'ast:function-declaration (type-of decl))
    (assert-eql 'ast:identifier (type-of (ast:function-declaration-name decl)))
    (assert-eql 'ast:function-signature (type-of (ast:function-declaration-signature decl)))
    (assert-eql 'ast:block (type-of (ast:function-declaration-body decl)))))

(define-test parse-function-with-variadic-arguments ()
  (multiple-value-bind (decl had-errors) (parse "func f(a int, b ...string) (int, string) { 10 }"  #'parser::<function-declaration)
    (assert-false had-errors)
    (assert-eql 'ast:function-declaration (type-of decl))
    (assert-eql 'ast:identifier (type-of (ast:function-declaration-name decl)))
    (assert-eql 'ast:function-signature (type-of (ast:function-declaration-signature decl)))
    (assert-eql 'ast:block (type-of (ast:function-declaration-body decl)))))
