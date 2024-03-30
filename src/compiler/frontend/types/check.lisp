(in-package :cl-braces.compiler.frontend.types)

(define-condition typing-error (error)
  ((message :initarg :message :reader type-error-message))
  (:report (lambda (condition stream)
             (format stream "TypeError: ~A" (type-error-message condition)))))

(defclass type-checker ()
  ((errors
    :reader type-checker-errors
    :initarg :errors
    :initform nil)
   (fail-fast-p
    :reader fail-fast-p
    :initarg :fail-fast-p
    :initform t)
   (type-map
    :reader type-map
    :initarg :type-map
    :initform (error "No type map provided")))
  (:documentation "A type checker for the AST"))

(-> make-type-checker (type-map &key (:fail-fast boolean)) type-checker)
(defun make-type-checker (typemap &key (fail-fast nil))
  (prog1 (make-instance 'type-checker :type-map typemap :fail-fast-p fail-fast)))

(defun check (checker ast)
  (ast:walk checker ast)
  (let ((errors (type-checker-errors checker)))
    (values (null errors) errors)))

(defun add-error (checker message)
  (with-slots (errors) checker
    (let ((err (make-instance 'typing-error :message message)))
      (push err errors)
      (when (fail-fast-p checker)
        (cerror "Continue type checking" err)))))

(defmethod ast:enter ((checker type-checker) (node ast:node))
  (declare (ignore checker node))
  :continue)

(defmethod ast:leave ((checker type-checker) (node ast:node))
  (declare (ignore checker node))
  :continue)

(defmethod ast:enter ((checker type-checker) (node ast:identifier))
  (declare (ignore checker node))
  :continue)

(defmethod ast:leave ((checker type-checker) (node ast:identifier))
  (declare (ignore checker node))
  :continue)

(defmethod ast:enter ((checker type-checker) (tok token:token))
  (declare (ignore checker tok))
  :continue)


(defmethod ast:leave ((checker type-checker) (tok token:token))
  (declare (ignore checker tok))
  :continue)

(defmethod ast:enter ((checker type-checker) (node ast:literal))
  (with-slots (type-map) checker
    (let* ((token-class (token:class (ast::literal-token node)))
           (tpe (token-class-to-type token-class)))
      (assert tpe)
      (setf (fetch-type type-map node) tpe))))

(defun token-class-to-type (token-class)
  (s:select token-class
    (token:@INTEGER (integer-type))
    (token:@TRUE (boolean-type))
    (token:@FALSE (boolean-type))
    (token:@NIL (nothing-type))))

(defmethod ast:leave ((checker type-checker) (node ast:binary-expression))
  (with-slots (type-map) checker
    (let ((right-type (fetch-type type-map (ast:binary-expression-rhs node)))
          (left-type  (fetch-type type-map (ast:binary-expression-lhs node))))
      (assert right-type)
      (assert left-type)
      (unless (type-equal left-type right-type)
        (add-error checker "Binary expression operands are not of the same type"))
      (let* ((op-token (token:class (ast::binary-expression-operator node)))
             (op-type (binary-op-to-type op-token)))
        (assert op-type)
        (unless (type-equal left-type op-type)
          (add-error checker "Binary expression operands are not of type integer"))
        (setf (fetch-type type-map node) left-type)))))

(defun binary-op-to-type (op-token)
  (s:select op-token
    (token:@PLUS (integer-type))
    (token:@MINUS (integer-type))
    (token:@STAR (integer-type))
    (token:@SLASH (integer-type))))

(defmethod ast:leave ((checker type-checker) (node ast:return-statement))
  (with-slots (type-map) checker
    (let ((expr-list (ast:return-statement-expressions node)))
      (setf (fetch-type type-map node)
            (if (null expr-list)
                (nothing-type)
                (fetch-type type-map expr-list))))))

(defmethod ast:leave ((checker type-checker) (node ast:expression-list))
  (with-slots (type-map) checker
    (loop :for expr :in (ast:expression-list-expressions node)
          :collect (fetch-type type-map expr) into types
          :finally (setf (fetch-type type-map node)
                         (cond
                           ((null types) (nothing-type))
                           ((= 1 (length types)) (car types))
                           (t (tuple-type types)))))))

(defmethod ast:leave ((checker type-checker) (stmt ast:assignment-statement))
  (with-slots (type-map) checker
    (let* ((lhs-type (fetch-type type-map (ast:assignment-statement-lhs stmt)))
           (rhs-type (fetch-type type-map (ast:assignment-statement-rhs stmt))))
      (assert lhs-type)
      (assert rhs-type)
      (unless (type-equal lhs-type rhs-type)
        (add-error checker "Assignment types do not match"))
      (setf (fetch-type type-map stmt) lhs-type))))

(defun check-function-type (func-type arg-types)
  "Check that the function type can be called with the given argument types"
  t)
