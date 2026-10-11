;;; -*- Lisp -*-

(defpackage "FUNCTION-TEST"
  (:use "COMMON-LISP" "FIVEAM")
  (:export "RUN-TESTS"))

(in-package "FUNCTION-TEST")

(def-suite function-suite :description "Tests for function.lisp")
(in-suite function-suite)

(defun run-tests ()
  (run! 'function-suite))

(test deff
  (function::deff test-deff-fn (lambda (x) (* 2 x)) "Doubles.")
  (is (= 10 (funcall 'test-deff-fn 5)))
  (is (equal "Doubles." (documentation 'test-deff-fn 'cl:function)))
  (function::deff test-deff-fn2 #'1+)
  (is (= 6 (funcall 'test-deff-fn2 5))))

(function::defmemo test-memo-fib (n)
  (if (< n 2) n (+ (test-memo-fib (- n 1)) (test-memo-fib (- n 2)))))

(defvar *memo-calls* 0)
(function::defmemo test-memo-counted (a b)
  (incf *memo-calls*)
  (+ a b))

(test defmemo
  (is (= 55 (test-memo-fib 10)))
  (is (= 12586269025 (test-memo-fib 50)))
  (setf *memo-calls* 0)
  (is (= 5 (test-memo-counted 2 3)))
  (is (= 5 (test-memo-counted 2 3)))
  (is (= 1 *memo-calls*))
  (is (= 7 (test-memo-counted 3 4)))
  (is (= 2 *memo-calls*))
  (is (= 5 (function-test::%test-memo-counted 2 3)))
  (is (= 3 *memo-calls*)))

(test binary-compose-left
  (is (= 16 (funcall (function::binary-compose-left #'* #'1+) 3 4)))
  (is (eq #'+ (function::binary-compose-left #'+ #'identity)))
  (is (eq #'+ (function::binary-compose-left #'+ 'identity))))

(test binary-compose-right
  (is (= 15 (funcall (function::binary-compose-right #'* #'1+) 3 4)))
  (is (eq #'+ (function::binary-compose-right #'+ #'identity)))
  (is (eq #'+ (function::binary-compose-right #'+ 'identity))))

(test binary-function->folder
  (is (= 10 (funcall (function::binary-function->folder #'+ 0) '(1 2 3 4))))
  (is (= 0 (funcall (function::binary-function->folder #'+ 0) '())))
  (is (equal '(3 2 1) (funcall (function::binary-function->folder (lambda (acc x) (cons x acc)) nil)
                               '(1 2 3)))))

(test binary-function->folder-1
  (is (= 20 (funcall (function::binary-function->folder-1 #'+) 10 '(1 2 3 4))))
  (is (= 10 (funcall (function::binary-function->folder-1 #'+) 10 '()))))

(test binary->n-ary
  (is (= 10 (funcall (function::binary->n-ary #'+ 0) 1 2 3 4)))
  (is (= 0 (funcall (function::binary->n-ary #'+ 0))))
  (is (= 24 (funcall (function::binary->n-ary #'* 1) 1 2 3 4))))

(test binary->n-ary1
  (is (= 20 (funcall (function::binary->n-ary1 #'+) 10 1 2 3 4)))
  (is (= 10 (funcall (function::binary->n-ary1 #'+) 10))))

(test compose2
  (is (= 7 (funcall (function::compose2 #'1+ #'*) 2 3)))
  (is (= 8 (funcall (function::compose2 #'1+ #'1+) 6)))
  (let ((f (lambda (x) x)))
    (is (eq f (function::compose2 #'identity f)))
    (is (eq f (function::compose2 'identity f)))
    (is (eq f (function::compose2 f #'identity)))
    (is (eq f (function::compose2 f 'identity)))))

(test compose
  (is (= 5 (funcall (function::compose) 5)))
  (is (= 6 (funcall (function::compose #'1+) 5)))
  ;; rightmost function is applied first
  (is (= 11 (funcall (function::compose #'1+ (lambda (x) (* 2 x))) 5)))
  (is (= 12 (funcall (function::compose (lambda (x) (* 2 x)) #'1+) 5)))
  (is (= 3 (funcall (function::compose #'1+ #'1+ #'1+) 0)))
  (is (= 7 (funcall (function::compose #'1+ #'+) 2 4))))

(test curry
  (is (= 7 (funcall (funcall (function::curry-left #'-) 10) 3)))
  (is (= -7 (funcall (funcall (function::curry-left #'-) 3) 10)))
  (is (= 7 (funcall (funcall (function::curry-right #'-) 3) 10)))
  (is (equal '(a b) (funcall (funcall (function::curry-left #'list) 'a) 'b)))
  (is (equal '(b a) (funcall (funcall (function::curry-right #'list) 'a) 'b))))

(test inverse
  (dolist (pair '((1- 1+) (acos cos) (acosh cosh) (asin sin) (asinh sinh)
                  (atan tan) (atanh tanh) (exp log)))
    (destructuring-bind (a b) pair
      (is (eq b (function::inverse a)))
      (is (eq a (function::inverse b)))
      (is (eq (symbol-function b) (function::inverse (symbol-function a))))
      (is (eq (symbol-function a) (function::inverse (symbol-function b))))))
  (is (eq 'identity (function::inverse 'identity)))
  (is (eq #'identity (function::inverse #'identity)))
  (is (eq 'conjugate (function::inverse 'conjugate)))
  (is (eq #'conjugate (function::inverse #'conjugate)))
  (is (= 5 (funcall (function::inverse #'1+) (funcall #'1+ 5))))
  (signals error (function::inverse #'car)))

(test opposite
  (is (eq t (funcall (function::opposite #'evenp) 3)))
  (is (eq nil (funcall (function::opposite #'evenp) 4)))
  (is (eq t (funcall (function::opposite #'<) 3 3)))
  (is (eq nil (funcall (function::opposite #'<) 1 2 3)))
  (is (eq t (funcall (function::opposite #'=) 1 2))))

(test partial-apply
  (is (= 7 (funcall (function::partial-apply-left #'- 10) 3)))
  (is (= 7 (funcall (function::partial-apply-right #'- 3) 10)))
  (is (equal '(a b) (funcall (function::partial-apply-left #'list 'a) 'b)))
  (is (equal '(b a) (funcall (function::partial-apply-right #'list 'a) 'b))))

(test sum-sequence
  (is (= 0 (function::sum-sequence #'identity '())))
  (is (= 14 (function::sum-sequence (lambda (x) (* x x)) '(1 2 3))))
  (is (= 14 (function::sum-sequence (lambda (x) (* x x)) #(1 2 3))))
  (is (= 6 (function::sum-sequence #'identity '(1 2 3)))))

(test product-sequence
  (is (= 1 (function::product-sequence #'identity '())))
  (is (= 36 (function::product-sequence (lambda (x) (* x x)) '(1 2 3))))
  (is (= 36 (function::product-sequence (lambda (x) (* x x)) #(1 2 3))))
  (is (= 6 (function::product-sequence #'identity '(1 2 3)))))

(test swap-arguments
  (is (= 7 (funcall (function::swap-arguments #'-) 3 10)))
  (is (equal '(b a) (funcall (function::swap-arguments #'list) 'a 'b)))
  (is (equal '(a . b) (funcall (function::swap-arguments (function::swap-arguments #'cons)) 'a 'b))))
