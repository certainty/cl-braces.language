(in-package :tests.vm.bytecode)

;; Disabled since the result of chunks isn't deterministic

(defun compile-chunk (input)
  (let ((ws (compiler:make-workspace "test_chunk" #p"")))
    (compiler:compile ws input :fail-fast t)))

(define-test disassembler-works ()
  (snapshots:assert-snapshot-equals
   "disassembler.snapshot"
   (with-output-to-string (s) (bytecode:disass (compile-chunk "3 + 3") :stream s))))
