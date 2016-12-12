;;;; package.lisp

(defpackage #:format-string-builder
  (:use #:cl #:alexandria)
  (:import-from #:serapeum #:intersperse #:op)
  (:export #:make-format-string
           #:define-message
	   #:format*))

