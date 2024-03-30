(in-package :cl-user)

(defpackage :cl-braces.compiler.frontend.token
  (:nicknames :frontend.token :token)
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:shadow :class)
  (:import-from :serapeum :->)
  (:export
   #:token
   #:class
   #:lexeme
   #:value
   #:location
   #:token-class
   #:class=
   #:literal-p
   #:identifier-p
   #:punctuation-p
   #:class-any-p
   #:synthetic-eof

   #:@EOF
   #:@ILLEGAL
   #:@LPAREN
   #:@RPAREN
   #:@LBRACKET
   #:@RBRACKET
   #:@LBRACE
   #:@RBRACE
   #:@INTEGER
   #:@PLUS
   #:@PLUS_PLUS
   #:@MINUS
   #:@MINUS_MINUS
   #:@STAR
   #:@SLASH
   #:@LT
   #:@LE
   #:@GT
   #:@GE
   #:@DOT
   #:@SEMICOLON
   #:@COMMA
   #:@COLON_EQUAL
   #:@PLUS_EQUAL
   #:@MUL_EQUAL
   #:@EQUAL
   #:@EQUAL_EQUAL
   #:@AMPERSAND_AMPERSAND
   #:@AMPERSAND
   #:@AMPERSAND_EQUAL
   #:@PIPE_PIPE
   #:@PIPE
   #:@PIPE_EQUAL
   #:@BANG
   #:@BANG_EQUAL
   #:@TILDE
   #:@TILDE_EQUAL
   #:@CARET
   #:@CARET_EQUAL

   #:@IDENTIFIER
   #:@TRUE
   #:@FALSE
   #:@NIL
   #:@FUNC
   #:@IF
   #:@ELSE
   #:@BREAK
   #:@CONTINUE
   #:@FALLTHROUGH
   #:@RETURN
   #:@VAR
   #:@PACKAGE
   #:@IMPORT
   #:@ELLIPSIS))

(defpackage :cl-braces.compiler.frontend.scanner
  (:nicknames :frontend.scanner :scanner :frontend.lexer :lexer)
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export
   #:all-tokens
   #:next-token
   #:make-scanner))
