(in-package :cl-braces.compiler.frontend.types)

(defclass <type> ()
  ((name
    :reader type-name
    :initarg :name
    :initform (error "Name is required")
    :type string
    :documentation "The name of the type.")
   (id
    :reader type-id
    :initarg :id
    :initform (error "Id is required")
    :type symbol
    :documentation "The unique identifier of the type.")
   (is-referenct-type-p
    :reader is-reference-type-p
    :initarg :is-reference-type-p
    :initform nil
    :type boolean
    :documentation "True if the type is a reference type, false otherwise.")))

(defclass <tuple-type> (<type>)
  ((element-types
    :reader element-types
    :initarg :element-types
    :initform nil
    :type list)))

(defclass <pointer-type> (<type>)
  ((inner-type
    :reader inner-type
    :initarg :inner-type
    :initform (error "Inner type is required")
    :type <type>)))

(defvar *integer-type* (make-instance '<type> :name "Integer" :id 'int))
(defvar *boolean-type* (make-instance '<type>  :name "Bool" :id 'bool))
(defvar *nothing-type* (make-instance '<type> :name "Nothing" :id 'nil :is-reference-type-p t))

(defun integer-type () *integer-type*)
(defun boolean-type () *boolean-type*)
(defun nothing-type () *nothing-type*)

(defun tuple-type (element-types)
  (let ((name (format nil "(~{~a~^, ~})" (mapcar #'type-name element-types)))
        (id (intern (format nil "tuple-~{~a~^-~}" (mapcar #'type-id element-types)))))
    (make-instance '<tuple-type> :name name :id id :element-types element-types)))

(defun pointer-type (inner)
  (let ((name (format nil "*~a" (type-name inner)))
        (id (intern (format nil "*~a" (type-id inner)))))
    (make-instance '<pointer-type> :name name :id id :inner-type inner)))

(defgeneric type-equal (type1 type2)
  (:documentation "Returns true if the two types are equal, false otherwise."))

(defmethod type-equal ((type1 <type>) (type2 <type>))
  (eq (slot-value type1 'id) (slot-value type2 'id)))

(defmethod type-equal ((type1 <tuple-type>) (type2 <tuple-type>))
  (let ((types1 (slot-value type1 'element-types))
        (types2 (slot-value type2 'element-types)))
    (and (= (length types1) (length types2))
         (every #'type-equal types1 types2))))

(defmethod type-equal ((type1 <pointer-type>) (type2 <pointer-type>))
  (type-equal (slot-value type1 'inner-type)
              (slot-value type2 'inner-type)))

(defun reference-type-p (type)
  (slot-value type 'is-reference-typep))

(defun value-type-p (type)
  (not (reference-type-p type)))
