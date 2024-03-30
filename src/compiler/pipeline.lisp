(in-package :cl-braces.compiler)

(define-condition compile-error (error)
  ((message :initarg :message :reader compile-error-message)
   (input :initarg :input :reader compile-error-input)
   (details :initarg :details :reader compile-error-details)
   (workspace :initarg :workspace :reader compile-error-workspace))
  (:report (lambda (condition stream)
             (format stream "Compile Error at ~A:  ~A ~%Details: ~A"
                     (compile-error-input condition)
                     (compile-error-message condition)
                     (compile-error-details condition)))))

(defun signal-compile-error (workspace input message &rest details)
  (error (make-condition 'compile-error :input input :message message :details details :workspace workspace)))

(defun compile* (module-name module-root &key fail-fast)
  "Compiles the provided module and returns the resulting bytecode chunk.
   `module-name` is the name of the module to compile and all imports in packages of that module will be resolved relative to `module-root`,
   and must contain the `module-name` as a prefix."
  (compile (make-workspace module-name module-root :fail-fast-p fail-fast)))

(defun compile (workspace)
  "Compiles all source files in the provided workspace"
  (with-slots (package-asts code-generator) workspace
    (let ((packages (parse-packages workspace)))
      (setf package-asts packages)
      (loop :for package :being :the :hash-keys :of packages
            :for package-ast = (gethash package packages)
            :do (compile-package workspace package-ast)
            :finally (return (codegen:finalize-chunk code-generator))))))

(defun parse-packages (workspace)
  "Parses all packages in the workspace and returns a hash table mapping package names to their ASTs."
  (let* ((packages (make-hash-table :test #'equal)))
    (let* ((file-asts (mapcar (lambda (file) (parse-file workspace file)) (source-files workspace))))
      (loop :for ast :in file-asts
            :do (let ((package-name (ast:identifier-name (ast:package-declaration-name (ast:source-file-package ast)))))
                  (a:if-let ((existing-ast (gethash package-name packages)))
                    (setf (gethash package-name packages) (cons ast existing-ast))
                    (setf (gethash package-name packages) (list ast)))))

      (loop :for package :being :the :hash-keys :of packages
            :for package-source-files = (gethash package packages)
            :for package-ast = (ast:merge-source-files package-source-files (ws-node-ids workspace))
            :do (setf (gethash package packages) package-ast))
      packages)))

(-> parse-file (workspace pathname) ast:source-file)
(defun parse-file (workspace path)
  "Parse the file at `path' and return a `ast:source-file' node."
  (sourcecode:with-input (input path)
    (let* ((source-code (sourcecode::input-string input))
           (scanner (scanner-for workspace source-code))
           (tokens (scanner:all-tokens scanner))
           (parser (parser-for workspace tokens)))
      (multiple-value-bind (ast had-errors state) (parser:parse parser :production #'parser::<source-file)
        (prog1 ast
          (when had-errors
            (signal-compile-error workspace input "Syntactic analysis failed" (parser:parse-errors state))))))))

(defun compile-package (workspace package)
  "Compiles the provided package and returns the resulting bytecode chunk."
  (pass-type-checking workspace package)
  (pass-semantic-analysis workspace package)
  (pass-code-generation workspace package))

(defun pass-type-checking (workspace ast)
  "Types the AST and returns any errors that occured.
   This is not only checking types, but also inferring them if possible.
   Once this pass has finished, the typed nodes in the AST will have a type assigned.
  "
  (let ((checker (type-checker-for workspace)))
    (multiple-value-bind (ok errors) (frontend.types:check checker ast)
      (unless ok
        (signal-compile-error workspace ast "Type checking failed" errors)))))

(defun pass-semantic-analysis (workspace ast)
  "Resolves all symbols in the AST and returns any errors that occured."
  (let ((symbol-resolver (symbol-resolver-for workspace)))
    (multiple-value-bind (ok errors) (symbol-resolver:resolve-symbols symbol-resolver ast)
      (unless ok
        (signal-compile-error workspace  ast "Semantic analysis failed" errors)))))

(defun pass-code-generation (workspace ast)
  "Generates the bytecode chunk for the AST"
  (with-slots (code-generator) workspace
    (codegen:generate code-generator ast)))
