(in-package :cl-braces.compiler.middleend.semantic.symbol-resolver)

;;;; this pass deals with symbol resolution and ultimately creates symbol-table(s)
;;;; to use for downstream passes
;;;;

;;;; however for now we start with a simple scope analysis to find references to undefined symbols
;;;;
;;;;

(define-condition semantic-error (error) ())

(define-condition variable-already-defined (semantic-error)
  ((name :initarg :symbol :reader name)))

(define-condition undefined-symbol (semantic-error)
  ((name :initarg :symbol :reader name)))

(defclass resolver ()
  ((symbol-table
    :initarg :symbol-table
    :initform (error "No symbol table provided"))
   (current-scope
    :initarg :current-scope
    :initform 0
    :type (integer 0 *))
   (current-package
    :initarg :current-package
    :initform nil
    :type (or null string))
   (context
    :initarg :context
    :initform nil
    :type (or null ast:node))
   (fail-fast
    :initarg :fail-fast
    :initform nil
    :type (or null boolean))
   (errors
    :reader errors-of
    :initform nil
    :type (support:list-of semantic-error))))

(-> resolver (symbol-table &key (:fail-fast boolean)) resolver)
(defun make-resolver (symbol-table &key (fail-fast nil))
  (prog1 (make-instance 'resolver :symbol-table symbol-table)))

(defun resolve-symbols (resolver ast)
  (with-slots (errors) resolver
    (handler-bind ((semantic-error (lambda (c)
                                     (if (slot-value resolver 'fail-fast)
                                         (invoke-debugger c)
                                         (when (find-restart 'continue)
                                           (push errors c)
                                           (invoke-restart 'continue))))))
      (ast:walk resolver ast)
      (values (null errors) (when errors (nreverse errors))))))

(defmethod ast:enter ((resolver resolver) (node ast:node))
  (declare (ignore resolver node))
  :continue)

(defmethod ast:leave ((resolver resolver) (node ast:node))
  (declare (ignore resolver node))
  :continue)

(defmethod ast:enter ((resolver resolver) (tok token:token))
  (declare (ignore resolver tok))
  :continue)

(defmethod ast:leave ((resolver resolver) (tok token:token))
  (declare (ignore resolver tok))
  :continue)


(defmethod ast:enter ((resolver resolver) (node ast:block))
  (enter-scope resolver))

(defmethod ast:leave ((resolver resolver) (node ast:block))
  (leave-scope resolver))

(defun enter-scope (resolver)
  (with-slots (current-scope) resolver
    (incf current-scope)))

(defun leave-scope (resolver)
  (with-slots (current-scope) resolver
    (decf current-scope)))

(defmethod ast:enter ((resolver resolver) (node ast:package-declaration))
  (with-slots (current-package) resolver
    (setf current-package (ast:identifier-name (ast:package-declaration-name node)))))

(defmethod ast:enter ((resolver resolver) (node ast:short-variable-declaration))
  (with-slots (symbol-table current-scope current-package errors) resolver
    (let* ((variables (ast:short-variable-declaration-identifiers node)))
      (dolist (variable (ast:identifier-list-identifiers variables))
        (let ((identifier (ast:identifier-name variable)))
          (a:if-let ((existing (symbols:find-by-name symbol-table current-package identifier :denotation #'symbols:denotes-variable-p :scope<= current-scope)))
            (dolist (existing existing)
              (unless (symbols:place-holder-p existing)
                (cerror "Variable already defined" (make-condition 'variable-already-defined :symbol identifier))))
            (symbols:add-symbol symbol-table current-package identifier :variable :scope current-scope)))))))

(defmethod ast:enter ((resolver resolver) (node ast:identifier))
  (with-slots (current-scope current-package errors symbol-table) resolver
    (let ((variable (ast:identifier-name node)))
      (unless
          (or (symbols:find-by-name symbol-table current-package variable :denotation #'symbols:denotes-variable-p :scope<= current-scope)
              (symbols:find-by-name symbol-table current-package variable :denotation #'symbols:denotes-function-p :scope<= 0))
        (cerror "Undefined symbol" (make-condition 'undefined-symbol :symbol variable))))))

(defmethod ast:enter ((resolver resolver) (node ast:variable-specification))
  (with-slots (symbol-table current-scope current-package errors) resolver
    (let* ((variables (ast:variable-specification-identifiers node)))
      (dolist (identifier (ast:identifier-list-identifiers variables))
        (let ((name (ast:identifier-name identifier)))
          (a:if-let ((existing (symbols:find-by-name symbol-table current-package name :denotation #'symbols:denotes-variable-p :scope<= current-scope)))
            (dolist (existing existing)
              (unless (symbols:place-holder-p existing)
                (cerror "Variable already defined" (make-condition 'variable-already-defined :symbol name))))
            (symbols:add-symbol symbol-table current-package name :variable :scope current-scope)))))))

(defmethod ast:enter ((resolver resolver) (node ast:function-declaration))
  (with-slots (symbol-table current-scope current-package errors) resolver
    (let ((name (ast:identifier-name (ast:function-declaration-name node))))
      (when (symbols:find-by-name symbol-table current-package name :denotation #'symbols:denotes-function-p)
        (cerror "Function already defined" (make-condition 'variable-already-defined :symbol name)))
      (symbols:add-symbol symbol-table current-package name :function :scope 0))))

(defmethod ast:enter ((resolver resolver) (node ast:function-signature))
  (enter-scope resolver))

(defmethod ast:leave ((resolver resolver) (node ast:function-signature))
  (leave-scope resolver))

(defmethod ast:enter ((resolver resolver) (node ast:block))
  (enter-scope resolver))

(defmethod ast:leave ((resolver resolver) (node ast:block))
  (leave-scope resolver))

(defmethod ast:enter ((resolver resolver) (node ast:parameter-declaration))
  (with-slots (symbol-table current-scope current-package errors) resolver
    (dolist (parameter (ast:identifier-list-identifiers (ast:parameter-declaration-identifiers node)))
      (let ((name (ast:identifier-name parameter)))
        (a:if-let ((exising (symbols:find-by-name symbol-table current-package name :denotation #'symbols:denotes-variable-p :scope<= current-scope)))
          (cerror "Variable already defined" (make-condition 'variable-already-defined :symbol name))
          (symbols:add-symbol symbol-table current-package name :variable :scope current-scope))))))
