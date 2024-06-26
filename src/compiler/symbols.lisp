(in-package :cl-braces.compiler.symbols)
(deftype scope-t () '(integer 0 *))

(deftype denotation-t () '(member :function :variable :type :constant))

;; we use angle brackets to avoid name clashes with common lisp symbols
(defclass <symbol> ()
  ((id
    :reader id
    :initarg :id
    :initform (error "id not initialized")
    :type symbol)
   (name
    :reader name
    :initarg :name
    :initform (error "name not initialized")
    :type string)
   (package-name
    :reader package-name*
    :initarg :package-name
    :initform (error "package not initialized")
    :type string)
   (denotation
    :reader denotation
    :initarg :denotation
    :initform nil
    :type (or null denotation-t ))
   (scope ;; 0 is global
    :reader scope
    :initarg :scope
    :initform 0
    :type scope-t)
   (exportedp
    :reader exportedp
    :initarg :exportedp
    :initform nil
    :type boolean)))

(defun qualified-name (symbol)
  (qualify (package-name* symbol) (name symbol)))

(defun qualify (package name)
  (concatenate 'string package "#" name))

(defmethod print-object ((symbol <symbol>) stream)
  (print-unreadable-object (symbol stream :type t :identity t)
    (format stream "denotation: ~a name: ~a" (denotation symbol) (name symbol))
    (when (scope symbol)
      (format stream " scope: ~a" (scope symbol)))))

(defclass symbol-table ()
  ((id-generator
    :initform (error "id-generator not initialized")
    :initarg :id-generator
    :type support:id-generator)
   (symbols-by-name
    :initform (make-hash-table :test 'equal)
    :initarg :by-name
    :type hash-table)
   (symbols-by-id
    :initform (make-hash-table :test 'equal)
    :initarg :by-id
    :type hash-table)))

(defun make-symbol-table (id-generator)
  "Create a new empty symbol table."
  (make-instance 'symbol-table :id-generator id-generator))

(-> globale-scope-p (scope-t) boolean)
(defun global-scope-p (scope)
  "Return true if the given `SCOPE' is the global scope."
  (and scope (zerop scope)))

(-> denotes-function-p (<symbol>) boolean)
(declaim (inline denotes-function-p))
(defun denotes-function-p (symbol)
  "Return true if the given `SYMBOL' denotes a function."
  (eql :function (denotation symbol)))

(-> denotes-variable-p (<symbol>) boolean)
(declaim (inline denotes-variable-p))
(defun denotes-variable-p (symbol)
  "Return true if the given `SYMBOL' denotes a variable."
  (eql :variable (denotation symbol)))

(-> denotes-type-p (<symbol>) boolean)
(declaim (inline denotes-type-p))
(defun denotes-type-p (symbol)
  "Return true if the given `SYMBOL' denotes a type."
  (eql :type (denotation symbol)))

(-> denotes-constant-p (<symbol>) boolean)
(declaim (inline denotes-constant-p))
(defun denotes-constant-p (symbol)
  "Return true if the given `SYMBOL' denotes a constant."
  (eql :constant (denotation symbol)))

(-> place-holder-p (<symbol>) boolean)
(defun place-holder-p (sym)
  (and (denotes-variable-p sym)
       (string= (name sym) "_")))

(-> add-symbol (symbol-table string string denotation-t &key (:scope scope-t)) symbol)
(defun add-symbol (table package name denotation &key (scope 0))
  "Add the symbol with the given `NAME' in `PACKAGE' and `DENOTATION' to the `TABLE' and return its `id'"

  (assert (plusp (length package)) (package package) "package must not be empty")
  (assert (plusp (length name)) (name name) "name must not be empty")

  (with-slots (symbols-by-name symbols-by-id id-generator) table
    (let* ((exportedp (char= (char-upcase (aref name 0)) (aref name 0)))
           (sym-id (support:next-id id-generator))
           (sym (make-instance '<symbol> :id sym-id :package-name package :name name :denotation denotation :exportedp exportedp :scope scope)))
      (with-slots (id) sym
        (prog1 id
          (setf (gethash id symbols-by-id) sym)
          (let ((updated (gethash (qualified-name sym) symbols-by-name)))
            (pushnew sym updated)
            ;; symbols with a higher scope are first
            (setf updated (sort updated #'> :key #'scope))
            (setf (gethash (qualified-name sym) symbols-by-name) updated)))))))

(-> find-by-id (symbol-table symbol) (or null <symbol>))
(defun find-by-id (table id)
  "Find the symbol with the given `ID' in the `TABLE' and return it or NIL if it does not exist."
  (with-slots (symbols-by-id) table
    (gethash id symbols-by-id)))

(-> find-by-name (symbol-table string string &key (:denotation (or null (function (<symbol>) boolean))) (:scope<= (or null scope-t)) (:only-exported (member nil t))) list)
(defun find-by-name (table package name &key (denotation nil) (scope<= nil) (only-exported nil))
  "Find all symbols with the given `NAME' in the `TABLE' and return them or NIL if it does not exist.
If `DENOTATION' is given if must be a function that takes a symbol and returns a boolean. See also `denots-any', `denotes-function-p', `denotes-variable-p' and `denotes-type-p'.
If `SCOPE<=' is given all symboles that have scope <= the given scope are returned.
If `ONLY-EXPORTED' is true only exported symbols are returned.
"
  (with-slots (symbols-by-name) table
    (let ((candidates (gethash (qualify package name) symbols-by-name)))
      (when denotation
        (setf candidates (filter-denotation denotation candidates)))
      (when scope<=
        (setf candidates (remove-if (lambda (sym) (> (scope sym) scope<=)) candidates)))
      (when only-exported
        (setf candidates (remove-if-not #'exportedp candidates)))
      candidates)))

(-> denotes-any (denotation-t &rest denotation-t) (function (<symbol>) boolean))
(defun denotes-any (denotation &rest more-denotations)
  "Return a function that takes a symbol and returns true if the symbol has any of the given `DENOTATION's."
  (let ((all-canddates (cons denotation more-denotations)))
    (lambda (symbol)
      (member (denotation symbol) all-canddates))))

(-> filter-denoation ((function (<symbol>) boolean) list) list)
(defun filter-denotation (denotation candidates)
  "Return a list of all symbols in `CANDIDATES' that satisfy the given `DENOTATION'."
  (remove-if (lambda (sym) (not (funcall denotation sym))) candidates))

(-> closest-scope (scope-t list) (or null <symbol>))
(defun closest-scope (current-scope candidates)
  "Return the symbol with the closest scope to `CURRENT-SCOPE' in `CANDIDATES' or NIL if no such symbol exists.
It searchs from high to low scopes, so it finds the hightest scope that is <= `CURRENT-SCOPE'.
"
  (find-if (lambda (sym) (<= (scope sym) current-scope)) candidates))

(defmethod support:debug-print ((obj symbol-table))
  (with-slots (symbols-by-name) obj
    (let ((all-symbols (a:hash-table-alist symbols-by-name)))
      (setf all-symbols (sort all-symbols #'string< :key #'car))
      (format *debug-io* "~20,a ~20,a ~15,a ~7,a ~10,a ~20,a~%" "Package" "Name" "Denotation" "Scope" "Exported" "ID")
      (dolist (entry all-symbols)
        (destructuring-bind (name . symbols) entry
          (declare (ignore name))
          (dolist (sym symbols)
            (format *debug-io* "~20,a ~20,a ~15,a ~7,a ~10,a ~20,a~%" (package-name* sym) (name sym) (denotation sym) (scope sym) (if (exportedp sym) "yes" "no") (id sym))))))))
