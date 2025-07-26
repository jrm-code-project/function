;;; -*- Lisp -*-

(in-package "FUNCTION")

(defmacro deff (name val &optional (docstring ""))
  "Define a function NAME with the value VAL. If DOCSTRING is provided, it will be used as the documentation for the function."
  `(PROGN
     (EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
       (DECLAIM (FTYPE FUNCTION ,name)))

     (EVAL-WHEN (:LOAD-TOPLEVEL :EXECUTE)
       (PROGN
         (SETF (SYMBOL-FUNCTION ',name) ,val
               ,@(when (and (stringp docstring)
                            (not (equal docstring "")))
                   `((DOCUMENTATION ',name 'CL:FUNCTION) ,docstring)))
         ',name))))

(defgeneric binary-compose-left (b f)
  (:documentation "Compose a binary function with another function on the left side.")
  (:method ((b function) (f function))
    (lambda (l r)
      (funcall b (funcall f l) r)))
  (:method ((b function) (f (eql #'identity)))
    b)
  (:method ((b function) (f (eql 'identity)))
    b))

(defgeneric binary-compose-right (b f)
  (:documentation "Compose a binary function with another function on the right side.")
  (:method ((b function) (f function))
    (lambda (l r)
      (funcall b l (funcall f r))))
  (:method ((b function) (f (eql #'identity)))
    b)
  (:method ((b function) (f (eql 'identity)))
    b))

(declaim (ftype (FUNCTION ((FUNCTION (T T) T) T) (VALUES FUNCTION &OPTIONAL)) binary->folder))
(defun binary->folder (binary-function base-case)
  "Convert a binary function into a folder function that can be used to reduce a list of arguments."
  (lambda (arglist)
    (fold-left binary-function base-case arglist)))

(declaim (ftype (FUNCTION ((FUNCTION (T T) T)) (VALUES FUNCTION &OPTIONAL)) binary->folder-1))
(defun binary->folder-1 (binary-function)
  "Convert a binary function into a folder function that can be used to reduce a list of arguments.  The first argument is the base case."
  (lambda (base-case arglist)
    (fold-left binary-function base-case arglist)))

(declaim (ftype (FUNCTION ((FUNCTION (T T) T) T) (VALUES FUNCTION &OPTIONAL)) binary->n-ary))
(defun binary->n-ary (binary-function base-case)
  "Convert a binary function into an n-ary function that can be used to reduce multiple arguments."
  (lambda (&rest args)
    (fold-left binary-function base-case args)))

(declaim (ftype (FUNCTION ((FUNCTION (T T) T)) (VALUES FUNCTION &OPTIONAL)) binary->n-ary1))
(defun binary->n-ary1 (binary-function)
  "Convert a binary function into an n-ary function that can be used to reduce multiple arguments. The first argument is the base case."
  (lambda (base-case &rest args)
    (fold-left binary-function base-case args)))

(defgeneric compose2 (outer inner)
  (:documentation "Compose two functions, where the result of the inner function is passed to the outer function.")
  (:method ((outer function) (inner function))
    (lambda (&rest args)
      (funcall outer (apply inner args))))
  (:method ((outer (eql #'identity)) (inner function))
    inner)
  (:method ((outer (eql 'identity)) (inner function))
    inner)
  (:method ((outer function) (inner (eql #'identity)))
    outer)
  (:method ((outer function) (inner (eql 'identity)))
    outer))

(deff compose (binary->n-ary #'compose2 #'identity) "Compose a list of functions.")

(declaim (ftype (function ((function (t t) t)) (function (t) (function (t) t))) curry-left))
(defun curry-left (binary-function)
  "Curry a binary function to take the left argument first."
  (lambda (left)
    (lambda (right)
      (funcall binary-function left right))))

(declaim (ftype (function ((function (t t) t)) (function (t) (function (t) t))) curry-right))
(defun curry-right (binary-function)
  "Curry a binary function to take the right argument first."
  (lambda (right)
    (lambda (left)
      (funcall binary-function left right))))

(defgeneric inverse (function)
  (:documentation "Return the inverse of FUNCTION.")
  (:method ((function (eql #'1-))) #'1+)
  (:method ((function (eql '1-)))   '1+)
  (:method ((function (eql #'1+))) #'1-)
  (:method ((function (eql '1+)))   '1-)
  (:method ((function (eql #'acos))) #'cos)
  (:method ((function (eql 'acos)))   'cos)
  (:method ((function (eql #'acosh))) #'cosh)
  (:method ((function (eql 'acosh)))   'cosh)
  (:method ((function (eql #'asin))) #'sin)
  (:method ((function (eql 'asin)))   'sin)
  (:method ((function (eql #'asinh))) #'sinh)
  (:method ((function (eql 'asinh)))   'sinh)
  (:method ((function (eql #'atan))) #'tan)
  (:method ((function (eql 'atan)))   'tan)
  (:method ((function (eql #'atanh))) #'tanh)
  (:method ((function (eql 'atanh)))   'tanh)
  (:method ((function (eql #'conjugate))) #'conjugate)
  (:method ((function (eql 'conjugate)))   'conjugate)
  (:method ((function (eql #'cos))) #'acos)
  (:method ((function (eql 'cos)))   'acos)
  (:method ((function (eql #'cosh))) #'acosh)
  (:method ((function (eql 'cosh)))   'acosh)
  (:method ((function (eql #'exp))) #'log)
  (:method ((function (eql 'exp)))   'log)
  (:method ((function (eql #'log))) #'exp)
  (:method ((function (eql 'log)))   'exp)
  (:method ((function (eql #'identity))) #'identity)
  (:method ((function (eql 'identity)))   'identity)
  (:method ((function (eql #'sin))) #'asin)
  (:method ((function (eql 'sin)))   'asin)
  (:method ((function (eql #'sinh))) #'asinh)
  (:method ((function (eql 'sinh)))   'asinh)
  (:method ((function (eql #'tan))) #'atan)
  (:method ((function (eql 'tan)))   'atan)
  (:method ((function (eql #'tanh))) #'atanh)
  (:method ((function (eql 'tanh)))   'atanh)
  )

(declaim (ftype (function ((function (t t) t) t) (function (t) t)) partial-apply-left))
(defun partial-apply-left (binary-function left)
  "Partial-apply a binary function to the left argument, returning a unary function that takes the right argument."
  (lambda (right)
    (funcall binary-function left right)))

(declaim (ftype (function ((function (t t) t) t) (function (t) t)) partial-apply-right))
(defun partial-apply-right (binary-function right)
  "Partial-apply a binary function to the right argument, returning a unary function that takes the left argument."
  (lambda (left)
    (funcall binary-function left right)))

(declaim (ftype (function ((function (t t) t)) (function (t t) t)) swap-arguments))
(defun swap-arguments (binary-function)
  "Swap the arguments of a binary function, returning a new function that takes the arguments in reverse order."
  (lambda (left right)
    (funcall binary-function right left)))
