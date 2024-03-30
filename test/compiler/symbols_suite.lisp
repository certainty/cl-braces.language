(in-package :cl-braces.tests.compiler.symbols)

(define-test adding-symbols ()
  (let ((st (symbols:make-symbol-table (support:make-id-generator))))
    (assert-false (symbols:find-by-name st "main" "foo"))
    (symbols:add-symbol st "main" "foo" :variable)
    (assert (symbols:find-by-name st "main" "foo"))))

(define-test finding-symbols-by-id ()
  (let ((st (symbols:make-symbol-table (support:make-id-generator))))
    (symbols:add-symbol st "main" "bar" :variable)
    (symbols:add-symbol st "main" "foobar" :function)

    (let* ((id (symbols:add-symbol st "main" "foo" :variable))
           (found (symbols:find-by-id st id)))
      (assert-equal (symbols:id found) id)
      (assert-equal (symbols:denotation found) :variable)
      (assert-equal (symbols:name found) "foo"))))

(define-test finding-symbol-by-name  ()
  (let ((st (symbols:make-symbol-table (support:make-id-generator))))
    (symbols:add-symbol st "main" "bar" :variable)
    (symbols:add-symbol st "main" "foobar" :function)
    (symbols:add-symbol st "main" "foobar" :variable)
    (symbols:add-symbol st "main" "foo" :variable)
    (let* ((found (symbols:find-by-name st "main" "foo")))
      (assert-equal (symbols:denotation (first found)) :variable)
      (assert-equal (symbols:name (first found)) "foo"))

    (let* ((found (symbols:find-by-name st "main" "foobar")))
      (assert-true (consp found))
      (assert-equal (length found) 2)
      (assert-true (member :function (mapcar #'symbols:denotation found)))
      (assert-true (member :variable (mapcar #'symbols:denotation found))))))

(define-test find-by-name-and-denotation ()
  (let ((st (symbols:make-symbol-table (support:make-id-generator))))
    (symbols:add-symbol st "main" "bar" :variable)
    (symbols:add-symbol st "main" "bar" :variable :scope 3)
    (symbols:add-symbol st "main" "bar" :variable :scope 4)
    (symbols:add-symbol st "main" "bar" :function)
    (symbols:add-symbol st "main" "foo" :function)

    (let ((found (symbols:find-by-name st "main" "bar" :denotation (symbols:denotes-any :function :variable))))
      (assert-true (consp found))
      (assert-equal (length found) 4))

    (let ((found (symbols:find-by-name st "main" "bar" :denotation #'symbols:denotes-function-p)))
      (assert-true (consp found))
      (assert-equal (length found) 1)
      (assert-equal (symbols:denotation (first found)) :function))))

(define-test find-by-name-denotation-and-scope ()
  (let ((st (symbols:make-symbol-table (support:make-id-generator))))
    (symbols:add-symbol st "main" "bar" :variable)
    (symbols:add-symbol st "main" "bar" :variable :scope 3)
    (symbols:add-symbol st "main" "bar" :variable :scope 4)
    (symbols:add-symbol st "main" "bar" :function)
    (symbols:add-symbol st "main" "foo" :function)

    (let ((found (symbols:find-by-name st "main" "bar" :denotation (symbols:denotes-any :variable))))
      (assert-true (consp found))
      (assert-equal (length found) 3)
      (let ((closest (symbols:closest-scope 5 found)))
        (assert closest)
        (assert-equal 4 (symbols:scope closest))))

    (let ((found (symbols:find-by-name st "main" "bar" :denotation #'symbols:denotes-variable-p :scope<= 2)))
      (assert-true (consp found))
      (assert-equal (length found) 1)
      (assert-equal (symbols:scope (first found)) 0))))
