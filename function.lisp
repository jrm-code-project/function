;;; -*- Lisp -*-

(in-package "FUNCTION")

(defgeneric binary-compose-left (b f)
  (:method ((b function) (f function))
    (lambda (l r)
      (funcall b (funcall f l) r)))
  (:method ((b function) (f (eql #'identity)))
    b)
  (:method ((b function) (f (eql 'identity)))
    b))

(defgeneric binary-compose-right (b f)
  (:method ((b function) (f function))
    (lambda (l r)
      (funcall b l (funcall f r))))
  (:method ((b function) (f (eql #'identity)))
    b)
  (:method ((b function) (f (eql 'identity)))
    b))

(declaim (ftype (FUNCTION ((FUNCTION (T T) T) T) (VALUES FUNCTION &OPTIONAL)) binary->folder))
(defun binary->folder (binary-function base-case)
  (lambda (arglist)
    (fold-left binary-function base-case arglist)))

(declaim (ftype (FUNCTION ((FUNCTION (T T) T)) (VALUES FUNCTION &OPTIONAL)) binary->folder-1))
(defun binary->folder-1 (binary-function)
  (lambda (base-case arglist)
    (fold-left binary-function base-case arglist)))

(declaim (ftype (FUNCTION ((FUNCTION (T T) T) T) (VALUES FUNCTION &OPTIONAL)) binary->n-ary))
(defun binary->n-ary (binary-function base-case)
  (lambda (&rest args)
    (fold-left binary-function base-case args)))

(declaim (ftype (FUNCTION ((FUNCTION (T T) T)) (VALUES FUNCTION &OPTIONAL)) binary->n-ary1))
(defun binary->n-ary1 (binary-function)
  (lambda (base-case &rest args)
    (fold-left binary-function base-case args)))

(defgeneric compose2 (outer inner)
  (:method ((outer function) (inner function))
    (lambda (&rest args)
      (multiple-value-call outer (apply inner args))))
  (:method ((outer (eql #'identity)) (inner function))
    inner)
  (:method ((outer (eql 'identity)) (inner function))
    inner)
  (:method ((outer function) (inner (eql #'identity)))
    outer)
  (:method ((outer function) (inner (eql 'identity)))
    outer))

(defun compose (&rest functions)
  (fold-left #'compose2 #'identity functions))

(declaim (ftype (function ((function (t t) t)) (function (t) (function (t) t))) curry-left))
(defun curry-left (binary-function)
  (lambda (left)
    (lambda (right)
      (funcall binary-function left right))))

(declaim (ftype (function ((function (t t) t)) (function (t) (function (t) t))) curry-right))
(defun curry-right (binary-function)
  (lambda (right)
    (lambda (left)
      (funcall binary-function left right))))

(defmacro deff (name val &optional (docstring ""))
  `(eval-when (:load-toplevel :execute)
     (progn
       (setf (symbol-function ',name) ,val
             ,@(when docstring
                 `((documentation ',name 'cl:function) ,docstring)))
       ',name)))

(defgeneric inverse (function))

(declaim (ftype (function ((function (t t) t) t) (function (t) t)) partial-apply-left))
(defun partial-apply-left (binary-function left)
  (lambda (right)
    (funcall binary-function left right)))

(declaim (ftype (function ((function (t t) t) t) (function (t) t)) partial-apply-right))
(defun partial-apply-right (binary-function right)
  (lambda (left)
    (funcall binary-function left right)))

(declaim (ftype (function ((function (t t) t)) (function (t t) t)) swap-arguments))
(defun swap-arguments (binary-function)
  (lambda (left right)
    (funcall binary-function right left)))

