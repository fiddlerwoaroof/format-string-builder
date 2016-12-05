;;;; package.lisp

(defpackage #:format-string-builder
  (:use #:cl #:alexandria)
  (:import #:serapeum #:intersperse #:op)
  (:export #:make-format-string
           #:define-message))

