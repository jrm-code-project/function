;;; -*- Lisp -*-

(defsystem "function"
  :depends-on ("fold")
  :components ((:file "function" :depends-on ("package"))
               (:file "package")))
