;;; -*- Lisp -*-

(defsystem "function"
  :depends-on ("alexandria" "fold")
  :components ((:file "function" :depends-on ("package"))
               (:file "package")))

(defsystem "function/test"
  :depends-on ("function" "fiveam")
  :components ((:file "function-test"))
  :perform (test-op (op c)
             (symbol-call "FUNCTION-TEST" "RUN-TESTS")))
