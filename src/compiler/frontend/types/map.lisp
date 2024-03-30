(in-package :cl-braces.compiler.frontend.types)

(defclass type-map ()
  ((id-to-type
    :initform (make-hash-table)))
  (:documentation "Map things that have an id to their types"))

(defun make-type-map ()
  (make-instance 'type-map))

(defgeneric fetch-type (type-map id)
  (:documentation "Lookup a type by id"))

(defmethod fetch-type ((type-map type-map) (obj support:has-id-mixin))
  (fetch-type type-map (support:id-of obj)))

(defmethod fetch-type ((type-map type-map) id)
  (with-slots (id-to-type) type-map
    (gethash id id-to-type)))

(defmethod (setf fetch-type) (new-value (type-map type-map) (obj support:has-id-mixin))
  (with-slots (id-to-type) type-map
    (setf (gethash (support:id-of obj) id-to-type) new-value)
    new-value))

(defmethod support:debug-print ((type-map type-map))
  (with-slots (id-to-type) type-map
    (maphash (lambda (id type)
               (format t "  ~A -> ~A~%" id (types:type-name type)))
             id-to-type)))
