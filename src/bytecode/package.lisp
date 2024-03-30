(in-package :cl-user)

(defpackage :cl-braces.bytecode
  (:nicknames :bytecode)
  (:use :cl :cl-braces.support)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export
   #:chunk
   #:instruction
   #:chunk-code
   #:chunk-constants
   #:chunk-registers-used
   #:chunk-entrypoint
   #:constant-table


   #:make-constants-builder
   #:constants-add
   #:make-chunk-builder
   #:instruction-opcode
   #:instruction-operands

   #:add-constant
   #:add-instructions

   #:go-package
   #:package-builder
   #:make-package-builder
   #:add-closure

   #:print-isa
   #:*isa-1.0*
   #:*current-isa*
   #:with-opcodes-from-current-isa
   #:operand-value
   #:address-value
   #:register-value
   #:disass
   #:disass-instruction
   #:format-value

   #:address-t
   #:register-t
   #:opcode-t
   #:immediate-t

   #:instr
   #:address
   #:addr
   #:register
   #:reg
   #:label
   #:label-address
   #:immediate
   #:imm
   #:label-name-for-label

   #:const
   #:call
   #:mov
   #:test
   #:jz
   #:jnz
   #:jmp
   #:ret
   #:noop
   #:halt
   #:add
   #:sub
   #:div
   #:mul
   #:neg
   #:eq
   #:lor
   #:land
   #:lnot))
