(in-package :cl-braces.compiler)

(s:defconst +workspace-source-file-glob-pattern+ #p"**/*.go")

(defclass workspace ()
  ((module-name
    :reader ws-module-name
    :initarg :module-name
    :initform (error "No module name provided")
    :type string
    :documentation "The name of the module that is being compiled")
   (module-root
    :reader ws-module-root
    :initarg :module-root
    :initform (error "No module root provided")
    :type pathname
    :documentation "The root directory of the module that is being compiled")
   (fail-fast-p
    :reader ws-fail-fast-p
    :initarg :fail-fast-p
    :initform nil
    :type boolean
    :documentation "If true, the compilation process will stop at the first error")
   (token-ids
    :reader ws-token-ids
    :initarg :token-ids
    :initform (support:make-id-generator :prefix "t-")
    :type support:id-generator
    :documentation "A map that maps tokens to their ids")
   (node-ids
    :reader ws-node-ids
    :initarg :node-ids
    :initform (support:make-id-generator :prefix "n-")
    :type support:id-generator
    :documentation "A map that maps nodes to their ids")
   (symbol-ids
    :reader ws-symbol-ids
    :initarg :symbol-ids
    :initform (error "No symbol ids provided")
    :type support:id-generator
    :documentation "A map that maps symbols to their ids")
   (token-spans
    :reader ws-token-spans
    :initarg :token-spans
    :initform (span:make-span-map)
    :type span:span-map
    :documentation "A map that maps tokens to their spans in the input")
   (node-spans
    :reader ws-node-spans
    :initarg :node-spans
    :initform (span:make-span-map)
    :type span:span-map
    :documentation "A map that maps nodes to their spans in the input")
   (package-asts
    :reader ws-package-asts
    :initarg :packages
    :initform (make-hash-table :test 'equal)
    :type (hash-table string ast:source-file)
    :documentation "A map that maps package names to their ast nodes")
   (typemap
    :reader ws-typemap
    :initarg :type-map
    :type types:type-map
    :initform (types:make-type-map)
    :documentation "A map that maps types to their ids")
   (symbol-table
    :reader ws-symbol-table
    :initarg :symbol-table
    :initform (error "No symbol table provided")
    :type symbols:symbol-table
    :documentation "A map that maps symbols to their ids")
   (code-generator
    :reader ws-code-generator
    :initarg :code-generator
    :type codegen:bytecode-generator
    :initform (error "No code generator provided")))
  (:documentation
   "The workspace holds the state for the compilation process of an entire module.
    Each file or package may be compile in an individual context, with some parts shared with the workspace.
   "))

(defun make-workspace (module-name module-root &key (fail-fast-p nil))
  "Create a workspace for a project. The `module-name' is the name of the golang module
   and the `module-root' is the root directory of the module.
   This is similar to the declaration on the go.mod file."
  (let* ((symbol-ids (support:make-id-generator :prefix "s-"))
         (symbol-table (symbols:make-symbol-table symbol-ids))
         (absolute-root (truename (uiop:native-namestring module-root))))
    (make-instance 'workspace
                   :module-name module-name
                   :module-root absolute-root
                   :fail-fast-p fail-fast-p
                   :symbol-ids symbol-ids
                   :symbol-table symbol-table
                   :code-generator (codegen:make-generator symbol-table))))

(defmethod print-object ((workspace workspace) stream)
  (print-unreadable-object (workspace stream :type t :identity t)
    (format stream "fail-fast: ~a module: ~a root: ~a" (if (ws-fail-fast-p workspace) "true" "false") (ws-module-name workspace) (ws-module-root workspace) )))

(defun source-files (workspace)
  "Returns a list of all source files in the workspace. Directories are traversed recursively."
  (let ((glob-pattern (merge-pathnames #p"**/*.go" (ws-module-root workspace))))
    (uiop:directory* glob-pattern)))

(-> scanner-for (workspace string) scanner::state)
(defun scanner-for (workspace source-code)
  "Create a scanner for the given context."
  (with-slots (token-ids token-spans fail-fast-p) workspace
    (scanner:make-scanner source-code token-ids token-spans :fail-fast fail-fast-p)))

(-> parser-for (workspace (vector token:token *)) parser::state)
(defun parser-for (workspace tokens)
  (with-slots (node-ids node-spans fail-fast-p) workspace
    (parser:make-parser tokens node-ids node-spans :fail-fast fail-fast-p)))

(-> type-checker-for (workspace) types::type-checker)
(defun type-checker-for (workspace)
  (with-slots (typemap fail-fast-p) workspace
    (types:make-type-checker typemap :fail-fast fail-fast-p)))

(-> symbol-resolver-for (workspace) symbol-resolver::resolver)
(defun symbol-resolver-for (workspace)
  (with-slots (symbol-table fail-fast-p) workspace
    (symbol-resolver:make-resolver symbol-table :fail-fast fail-fast-p)))

