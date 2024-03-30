(in-package :cl-user)

(defpackage :cl-braces.compiler.frontend.ast
  (:nicknames :compiler.frontend.ast :frontend.ast :ast)
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:shadow :declaration :variable :block)
  (:export
   #:merge-source-files
   #:node
   #:location
   #:expression
   #:bad-expression
   #:literal
   #:literal-value

   #:grouping-expression
   #:grouping-expression-expression

   #:unary-expression
   #:unary-expression-operator
   #:unary-expression-operand

   #:binary-expression
   #:binary-expression-lhs
   #:binary-expression-operator
   #:binary-expression-rhs

   #:expression-list
   #:expression-list-expressions

   #:declaration
   #:bad-declaration
   #:short-variable-declaration
   #:short-variable-declaration-expressions
   #:short-variable-declaration-identifiers
   #:variable-declaration
   #:variable-declaration-specifications
   #:variable-specification
   #:variable-specification-identifiers
   #:variable-specification-type
   #:variable-specification-initializer

   #:statement
   #:bad-statement
   #:empty-statement
   #:if-statement
   #:if-statement-init
   #:if-statement-condition
   #:if-statement-consequence
   #:if-statement-alternative
   #:return-statement
   #:return-statement-expressions
   #:expression-statement
   #:expression-statement-expression
   #:statement-list
   #:statement-list-statements

   #:assignment-statement
   #:assignment-statement-lhs
   #:assignment-statement-operator
   #:assignment-statement-rhs

   #:variable
   #:variable-identifier

   #:type-specifier
   #:type-specifier-name

   #:identifier
   #:identifier-token
   #:identifier-name
   #:identifier-list
   #:identifier-list-identifiers

   #:qualified-identifier
   #:qualified-identifier-package-name
   #:qualified-identifier-identifier

   #:function-declaration
   #:function-declaration-name
   #:function-declaration-signature
   #:function-declaration-body

   #:function-signature
   #:function-signature-parameters
   #:function-signature-return-type
   #:function-signature-return-parameters

   #:function-call
   #:function-call-function
   #:function-call-arguments

   #:parameter-declaration
   #:parameter-declaration-identifiers
   #:parameter-declaration-splat
   #:parameter-declaration-type

   #:parameter-splat
   #:parameter-splat-token

   #:parameter-list
   #:parameter-list-parameters

   #:package-declaration
   #:package-declaration-name

   #:comma

   #:block
   #:block-statements

   #:source-file
   #:source-file-declarations
   #:source-file-package
   #:make-source-file

   #:walk
   #:enter
   #:leave
   #:print-ast))
