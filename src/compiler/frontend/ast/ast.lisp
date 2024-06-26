(in-package :cl-braces.compiler.frontend.ast)

;;;; Representation of a faithful AST for our programming language.
;;;; Faithful means that the AST represents the source code in a way that is as close as possible to the original source code.

(defclass node (support:has-id-mixin) ()
  (:documentation "The base class for all AST nodes in the highlevel AST."))

;;; ===========================================================================
;;; AST traversal via the visitor pattern
;;; ===========================================================================

(defgeneric walk (visitor node)
  (:documentation "Walks the AST rooted at `NODE', calling the appropriate `VISIT' and `LEAVE' methods on `VISITOR'. The tree is traversed inorder."))

(defgeneric children (node)
  (:documentation "Returns a list of the children of `NODE'"))

(defgeneric enter (visitor node)
  (:documentation "Dispatches to the appropriate visit method for the node and visitor"))

(defgeneric leave (visitor node)
  (:documentation "Dispatches to the appropriate leave method for the node and visitor"))

(defun stop-walking-p (visitor-result)
  (and visitor-result (eq visitor-result :stop)))

(defun continue-walking-p (visitor-result)
  (not (stop-walking-p visitor-result)))

(defmethod walk (visitor (node node))
  (when (continue-walking-p (enter visitor node))
    (dolist (child (children node))
      (walk visitor child))
    (leave visitor node)))

(defmethod walk (visitor (tok frontend.token:token))
  (enter visitor tok)
  (leave visitor tok))

;;; ===========================================================================
;;; Source-File
;;; ===========================================================================

(defclass source-file (node)
  ((package
    :reader source-file-package
    :initarg :package
    :initform (error "must provide package")
    :documentation "The package declaration for the file.")
   (imports
    :reader source-file-imports
    :initarg :imports
    :initform nil
    :type list)
   (declarations
    :reader source-file-declarations
    :initarg :declarations
    :initform (error "must provide declarations")
    ;; list of top-level declarations
    :type list))
  (:documentation "The root node of the highlevel AST."))

(defun make-source-file (id package-decl imports decls)
  (make-instance 'source-file
                 :id id
                 :package package-decl
                 :declarations decls))

(defmethod children ((node source-file))
  (cons (source-file-package node)
        (source-file-declarations node)))

(defun merge-source-files (source-files node-ids)
  "Merges source files, which all must have the same package, into a single source file."
  (let* ((package-decls (mapcar #'source-file-package source-files))
         (unique-package-names (remove-duplicates
                                (mapcar
                                 (lambda (p) (ast:identifier-name (ast:package-declaration-name p)))
                                 package-decls)
                                :test #'equal)))
    (when (cdr unique-package-names)
      (error "Multiple different packages in source files: ~A" unique-package-names))

    (let ((declarations (mapcan #'source-file-declarations source-files)))
      (make-source-file
       (support:next-id node-ids)
       (first package-decls) nil declarations))))

;;; ===========================================================================
;;; Statements
;;; ===========================================================================

(defclass statement (node) ())

(defclass bad-statement (statement)
  ((message :reader bad-statement-message
            :initarg :message
            :initform (error "must provide message")
            :type string))
  (:documentation "A statement that could not be parsed correctly."))

(defmethod children ((node bad-statement))
  nil)

(defclass empty-statement (statement) ())

(defmethod children ((node empty-statement))
  nil)

(defclass statement-list (statement)
  ((statements
    :reader statement-list-statements
    :initarg :statements
    :initform (error "must provide statements")
    :type list))
  (:documentation "A statement that is a list of statements"))

(defmethod children ((node statement-list))
  (statement-list-statements node))

(defclass expression-statement (statement)
  ((expression
    :reader expression-statement-expression
    :initarg :expression
    :initform (error "must provide expression")
    :type expression))
  (:documentation "A statement that is an expression."))

(defmethod children ((node expression-statement))
  (list (expression-statement-expression node)))

(defclass if-statement (statement)
  ((init
    :reader if-statement-init
    :initarg :init
    :initform nil
    :type (or empty-statement simple-statement))

   (condition
    :reader if-statement-condition
    :initarg :condition
    :initform (error "must provide condition")
    :type expression)

   (consequence
    :reader if-statement-consequence
    :initarg :consequence
    :initform (error "must provide then-block")
    :type block)

   (alternative
    :reader if-statement-alternative
    :initarg :alternative
    :initform nil
    :type (or null statement)))
  (:documentation "A statement that is an expression."))

(defmethod children ((node if-statement))
  (let ((base (list
               (if-statement-init node)
               (if-statement-condition node)
               (if-statement-consequence node))))
    (when (if-statement-alternative node)
      (setf base (append base (list (if-statement-alternative node)))))
    base))

(defclass return-statement (statement)
  ((expressions
    :reader return-statement-expressions
    :initarg :expressions
    ;; TODO: is null really the right type here?
    :type (or null expression-list)))
  (:documentation "A statement that is a return."))

(defmethod children ((node return-statement))
  (when (return-statement-expressions node)
    (list (return-statement-expressions node))))

(defclass assignment-statement (statement)
  ((lhs
    :reader assignment-statement-lhs
    :initarg :lhs
    :initform (error "must provide lhs")
    :type expression-list)
   (operator
    :reader assignment-statement-operator
    :initarg :operator
    :initform (error "must provide op")
    :type frontend.token:token)
   (rhs
    :reader assignment-statement-rhs
    :initarg :rhs
    :initform (error "must provide rhs")
    :type expression-list))
  (:documentation "A statement that is an assignment."))

(defmethod children ((node assignment-statement))
  (list (assignment-statement-lhs node)
        (assignment-statement-operator node)
        (assignment-statement-rhs node)))

(defclass block (node)
  ((statements
    :reader block-statements
    :initarg :statements
    :initform (error "must provide statements")
    :type statement-list))
  (:documentation "A block of statements"))

(defmethod children ((node block))
  (list (block-statements node)))

;;; ===========================================================================
;;; Declarations
;;; ===========================================================================

(defclass declaration (statement) ())

(defclass bad-declaration (declaration)
  ((message :reader bad-declaration-message
            :initarg :message
            :initform (error "must provide message")
            :type string))
  (:documentation "A declaration that could not be parsed correctly."))

(defmethod children ((node bad-declaration))
  nil)

(defclass import-declaration (declaration)
  ((import-specs
    :reader import-declaration-import-specs
    :initarg :import-specs
    :initform (error "must provide import specs")
    :type list))
  (:documentation "An import declaration"))

(defmethod children ((node import-declaration))
  (import-declaration-import-specs node))

(defclass import-specification ()
  ((package-name
    :reader import-specification-package-name
    :initarg :package-name
    :initform (error "must provide package name")
    :type (or null identifier))
   (path
    :reader import-specification-path
    :initarg :path
    :initform (error "must provide path")
    :type string))
  (:documentation "An import specification"))

(defmethod children ((node import-declaration))
  (list (import-declaration-package-name node)
        (import-declaration-path node)))

(defclass variable-declaration (declaration)
  ((specifications
    :reader variable-declaration-specifications
    :initarg :specifications
    :initform (error "must provide specifications")
    :type (support:list-of variable-specification)))
  (:documentation "A declaration that declares variables."))

(defmethod children ((node variable-declaration))
  (variable-declaration-specifications node))

(defclass variable-specification (declaration)
  ((identifiers
    :reader variable-specification-identifiers
    :initarg :identifiers
    :initform (error "must provide variables")
    :type identifier-list
    :documentation "The list of variables to declar")
   (type
    :reader variable-specification-type
    :initarg :type
    :initform nil
    :type (or null type-specifier)
    :documentation "The type of the variables. If this is nil it means the type is inferred from the initializer.")
   (initializer
    :reader variable-specification-initializer
    :initarg :initializer
    :initform nil
    :type (or null expression-list)
    :documentation "The initializer for the variables")))

(defmethod children ((node variable-specification))
  (let ((base (list (variable-specification-identifiers node))))
    (when (variable-specification-type node)
      (push (variable-specification-type node) base))
    (when (variable-specification-initializer node)
      (push (variable-specification-initializer node) base))
    (reverse base)))

(defclass type-specifier (node)
  ((name
    :reader type-specifier-name
    :initarg :name
    :initform (error "must provide name")
    :type token:token))
  (:documentation "The base class for all type specifiers in the highlevel AST."))

(defmethod children ((node type-specifier))
  (list (type-specifier-name node)))

(defclass short-variable-declaration (declaration)
  ((identifiers
    :reader short-variable-declaration-identifiers
    :initarg :identifiers
    :initform (error "must provide variables")
    :type identifier-list
    :documentation "The list of variables")
   (expressions
    :reader short-variable-declaration-expressions
    :initarg :expressions
    :initform (error "must provide initializer")
    :type expression-list))
  (:documentation "The list of expressions for the variables"))

(defmethod children ((node short-variable-declaration))
  (list
   (short-variable-declaration-identifiers node)
   (short-variable-declaration-expressions node)))

(defclass function-declaration (declaration)
  ((name
    :reader function-declaration-name
    :initarg :name
    :initform (error "must provide name")
    :type identifier)
   (signature
    :reader function-declaration-signature
    :initarg :signature
    :initform (error "must provide signature")
    :type function-signature)
   (body
    :reader function-declaration-body
    :initarg :body
    :initform (error "must provide body")
    :type block))
  (:documentation "A function declaration"))

(defmethod children ((node function-declaration))
  (list (function-declaration-name node)
        (function-declaration-signature node)
        (function-declaration-body node)))

(defclass function-signature (declaration)
  ((parameters
    :reader function-signature-parameters
    :initarg :parameters
    :initform (error "must provide parameters")
    :type parameter-list)
   (return-type
    :reader function-signature-return-type
    :initarg :return-type
    :initform nil
    :type (or null type-specifier)
    :documentation "The return type of the function if it doesn't declare named return parameters.")
   (return-parameters
    :reader function-signature-return-parameters
    :initarg :return-parameters
    :initform nil
    :type (or null parameter-list)
    :documentation "This is set iff the function has named return parameters. It's a list of `parameter-declaration'.")))

(defmethod children ((node function-signature))
  (let ((base (list (function-signature-parameters node))))
    (when (function-signature-return-type node)
      (push (function-signature-return-type node) base))
    (when (function-signature-return-parameters node)
      (push (function-signature-return-parameters node) base))
    (reverse base)))


(defclass parameter-list (declaration)
  ((parameters
    :reader parameter-list-parameters
    :initarg :parameters
    :initform (error "must provide parameters")
    :type list))
  (:documentation "A list of parameters"))

(defmethod children ((node parameter-list))
  (parameter-list-parameters node))

(defun parameter-names (params)
  (mapcan #'identifier-list-identifiers (mapcan #'parameter-declaration-identifiers params)))

(defclass parameter-splat (node)
  ((token
    :reader parameter-splat-token
    :initarg :token
    :initform (error "must provide token")
    :type token:token))
  (:documentation "A parameter splat"))

(defmethod children ((node parameter-splat)) nil)

(defclass parameter-declaration (declaration)
  ((identifiers
    :reader parameter-declaration-identifiers
    :initarg :identifiers
    :initform nil
    :type (or null identifier-list))
   (splat
    :reader parameter-declaration-splat
    :initarg :splat
    :initform nil
    :type (or null parameter-splat))
   (type
    :reader parameter-declaration-type
    :initarg :type
    :initform (error "must provide type")
    :type type-specifier))
  (:documentation "A parameter declaration"))

(defmethod children ((node parameter-declaration))
  (let ((base nil))
    (a:when-let ((identifiers (parameter-declaration-identifiers node)))
      (push identifiers base))
    (a:when-let ((splat  (parameter-declaration-splat node)))
      (push splat base))
    (a:when-let ((type (parameter-declaration-type node)))
      (push type base))
    (reverse base)))

(defclass package-declaration (declaration)
  ((name
    :reader package-declaration-name
    :initarg :name
    :initform (error "must provide name")
    :type identifier))
  (:documentation "A package declaration"))

(defmethod children ((node package-declaration))
  (list (package-declaration-name node)))

;;; ===========================================================================
;;; Expressions
;;; ===========================================================================

(defclass expression (node) ()
  (:documentation "The base class for all expressions in the highlevel AST."))

(defclass literal (expression)
  ((token
    :reader literal-token
    :initarg :token
    :initform (error "must provide token")
    :type frontend.token:token))
  (:documentation "The base class for all literals in the highlevel AST."))

(defmethod children ((node literal))
  (list (literal-token node)))

(-> literal-value (literal) t)
(defun literal-value (expression)
  (token:value (literal-token expression)))

(defclass grouping-expression (expression)
  ((expression
    :reader grouping-expression-expression
    :initarg :expression
    :initform (error "must provide expression")
    :type expression))
  (:documentation "An expression that is surrounded by parentheses."))

(defmethod children ((node grouping-expression))
  (list (grouping-expression-expression node)))

(defclass unary-expression (expression)
  ((operator
    :reader unary-expression-operator
    :initarg :operator
    :initform (error "must provide op")
    :type frontend.token:token)
   (operand
    :reader unary-expression-operand
    :initarg :operand
    :initform (error "must provide operand")
    :type expression))
  (:documentation "An expression for binary relations"))

(defmethod children ((node unary-expression))
  (list
   (unary-expression-operator node)
   (unary-expression-operand node)))

(defclass binary-expression (expression)
  ((lhs
    :reader binary-expression-lhs
    :initarg :lhs
    :initform (error "must provide lhs")
    :type expression)
   (operator
    :reader binary-expression-operator
    :initarg :operator
    :initform (error "must provide operator")
    :type frontend.token:token)
   (rhs
    :reader binary-expression-rhs
    :initarg :rhs
    :initform (error "must provide rhs")
    :type expression))
  (:documentation "An expression for binary relations"))

(defmethod children ((node binary-expression))
  (list
   (binary-expression-operator node)
   (binary-expression-lhs node)
   (binary-expression-rhs node)))

(defclass expression-list (expression)
  ((expressions
    :reader expression-list-expressions
    :initarg :expressions
    :initform (error "must provide expressions")
    :type list))
  (:documentation "An expression that is a list of expressions"))

(defmethod children ((node expression-list))
  (expression-list-expressions node))

(defclass variable (node)
  ((identifier
    :reader variable-identifier
    :initarg :identifier
    :initform (error "must provide identifier")
    :type frontend.token:token))
  (:documentation "The base class for all variables in the highlevel AST."))

(defmethod children ((node variable))
  (list (variable-identifier node)))

(defclass identifier (node)
  ((token
    :reader identifier-token
    :initarg :token
    :initform (error "must provide token")
    :type frontend.token:token))
  (:documentation "The base class for all identifiers in the highlevel AST."))

(defmethod children ((node identifier))
  (list (identifier-token node)))

(defun identifier-name (identifier)
  (token:lexeme (identifier-token identifier)))

(defclass qualified-identifier (node)
  ((package-name
    :reader qualified-identifier-package-name
    :initarg :package-name
    :initform (error "must provide package name")
    :type identifier)
   (identifier
    :reader qualified-identifier-identifier
    :initarg :identifier
    :initform (error "must provide identifier")
    :type identifier))
  (:documentation "A qualified identifier"))

(defclass identifier-list (node)
  ((identifiers
    :reader identifier-list-identifiers
    :initarg :identifiers
    :initform (error "must provide variables")
    :type list))
  (:documentation "A list of identifiers"))

(defmethod children ((node identifier-list))
  (identifier-list-identifiers node))

;; preserving commas is useful for the parser and for pretty printing
(defclass comma (node)
  ((token
    :reader comma-token
    :initarg :token
    :initform (error "must provide token")
    :type token:token))
  (:documentation "A comma"))

(defclass function-call (expression)
  ((function
    :reader function-call-function
    :initarg :function
    :initform (error "must provide functio ")
    :type expression)
   (arguments
    :reader function-call-arguments
    :initarg :arguments
    :initform (error "must provide arguments")
    :type (or null expression-list)))
  (:documentation "A function call"))

(defmethod children ((node function-call))
  (let ((base (list (function-call-function node))))
    (when (function-call-arguments node)
      (push (function-call-arguments node) base))
    (reverse base)))
