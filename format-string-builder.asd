;;;; format-string-builder.asd

(asdf:defsystem #:format-string-builder
  :description "A DSL wrapping cl:format's syntax with something more lispy."
  :author "fiddlerwoaroof"
  :license "MIT"
  :depends-on (:alexandria
	       :serapeum)
  :serial t
  :components ((:file "package")
               (:file "format-string-builder")))

