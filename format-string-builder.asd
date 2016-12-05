;;;; format-string-builder.asd

(asdf:defsystem #:format-string-builder
  :description "Describe format-string-builder here"
  :author "fiddlerwoaroof"
  :license "MIT"
  :depends-on (:alexandria
	       :serapeum)
  :serial t
  :components ((:file "package")
               (:file "format-string-builder")))

