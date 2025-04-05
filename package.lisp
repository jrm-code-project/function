;;; -*- Lisp -*-

(defpackage "FUNCTION"
  (:export "BINARY-COMPOSE-LEFT"
           "BINARY-COMPOSE-RIGHT"
           "COMPOSE"
           "COMPOSE2"
           "CURRY-LEFT"
           "CURRY-RIGHT"
           "INVERSE"
           "PARTIAL-APPLY-LEFT"
           "PARTIAL-APPLY-RIGHT"
           "SWAP-ARGUMENTS")
  (:use "COMMON-LISP" "FOLD"))
