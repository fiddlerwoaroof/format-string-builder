A simple package implementing a DSL for generating format strings.

```lisp

(make-format-string '(:str)) #| ==> "~a" |#
(make-format-string '((:map () :str))) #| ==> "~{~a~}" |#

(define-message hello (name)
  "Hello " :str)
(define-message print-comma-separated (values)
  (:map () :str))

```
