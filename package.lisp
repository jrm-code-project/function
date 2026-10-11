;;; -*- Lisp -*-

(defpackage "FUNCTION"
  (:shadow "COMPOSE")
  (:export "BINARY-COMPOSE-LEFT"
           "BINARY-COMPOSE-RIGHT"
           "BINARY->FOLDER"
           "BINARY->FOLDER-1"
           "BINARY->N-ARY"
           "BINARY->N-ARY1"
           "COMPOSE"
           "COMPOSE2"
           "CURRY-LEFT"
           "CURRY-RIGHT"
           "DEFF"
           "DEFMEMO"
           "INVERSE"
           "OPPOSITE"
           "PARTIAL-APPLY-LEFT"
           "PARTIAL-APPLY-RIGHT"
           "PRODUCT-SEQUENCE"
           "SUM-SEQUENCE"
           "SWAP-ARGUMENTS")
  (:use "ALEXANDRIA" "COMMON-LISP" "FOLD"))
