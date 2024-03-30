(in-package :asdf-user)

(defsystem  "cl-braces.language"
  :description "A compiler and virtual machine for a minimal go-like programming language, called `gone'"
  :author "David Krentzlin <david.krentzlin@gmail.com>"
  :maintainer "David Krentzlin <david.krentzlin@gmail.com>"
  :source-control (:git "https://github.com/certainty/cl-braces.language.git")
  :license "BSD"
  :version "1.0"
  :depends-on (:alexandria :serapeum :random-uuid :cl-ppcre :tmpdir)
  :in-order-to ((test-op (test-op "cl-braces.language/tests")))
  :serial t
  :pathname "src"
  :components
  ((:module "support"
    :components
    ((:file "package")
     (:file "utils")
     (:file "id")))
   (:module "runtime"
    :components
    ((:file "package")
     (:file "value")))
   (:module "bytecode"
    :components
    ((:file "package")
     (:file "chunk")
     (:file "isa")
     (:file "isa-1.0")
     (:file "disassembler")))
   (:module "sourcecode"
    :components
    ((:file "package")
     (:file "input")
     (:file "location")
     (:file "span")))
   (:module "compiler"
    :components
    ((:file "package")
     (:file "symbols")
     (:module "frontend"
      :components
      ((:module "lexer"
        :components
                ((:file "package")
                 (:file "token")
                 (:file "scanner")))

       (:module "ast"
        :components
                ((:file "package")
                 (:file "ast")))
       (:module "types"
        :components
                ((:file "package")
                 (:file "type")
                 (:file "map")
                 (:file "check")))

       (:file "ast/printer")

       (:module "parser"
        :components
                ((:file "package")
                 (:file "buffer")
                 (:file "parser")))))

     (:module "middleend"
      :components
      ((:module "semantic"
        :components
                ((:file "package")
                 (:file "symbol-resolver")))))

     (:module "backend"
      :components
      ((:file "package")
       (:file "constants-builder")
       (:file "codegen")))
     (:file "workspace")
     (:file "pipeline")))
   (:module "vm"
    :components
    ((:file "package")
     (:file "debug")
     (:file "callstack")
     (:file "machine")))


   ))

(defsystem "cl-braces.language/tests"
  :depends-on (:lisp-unit2 :alexandria :cl-braces.language)
  :serial t
  :pathname "test"
  :components
  ((:file "packages")
   (:file "snapshot_tests")
   (:file "runner")
   (:module "compiler"
    :components
    ((:module "frontend"
      :components
      ((:file "scanner_suite")
       (:file "parser_suite")))
     (:module "middleend"
      :components
      ((:file "symbol_resolver_suite")))
     (:module "backend"
      :components
      ((:file "codegen_suite")))
     (:file "symbols_suite")))
   (:module "vm"
    :components
    ((:file "bytecode_suite")
     (:file "system_suite"))))
  :perform (test-op (o c)
                    (declare (ignore o c))
                    (uiop:symbol-call :cl-braces.tests.runner :run-suites)))
