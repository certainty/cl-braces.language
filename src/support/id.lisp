(in-package :cl-braces.support)

(defclass id-generator () ()
  (:documentation "Abstract class for id generators"))

(defclass sequential-id-generator (id-generator)
  ((prefix :initarg :prefix :initform (error "prefix must be provided"))
   (counter :initarg :counter :initform 0))
  (:documentation "Default id generator. Simple and effective."))

(defun make-id-generator (&key (prefix "id-") (counter 0))
  (make-instance 'sequential-id-generator :prefix prefix :counter counter))

(defgeneric next-id (id-generator)
  (:documentation "Generates a new id using the `id-generator'"))

(defmethod next-id ((id-generator sequential-id-generator))
  (with-slots (prefix counter) id-generator
    (incf counter)
    (make-symbol (concatenate 'string prefix (write-to-string counter)))))

(defclass has-id-mixin ()
  ((id
    :initarg :id
    :initform (error "id must be provided for descendants of has-id-mixin")))
  (:documentation "Mixin for objects that have an id"))

(defgeneric id-of (has-id)
  (:documentation "Returns the id of the object"))

(defmethod id-of ((has-id has-id-mixin))
  (slot-value has-id 'id))
