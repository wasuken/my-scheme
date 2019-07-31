;; (require \'asdf)

(in-package :cl-user)
(defpackage my-scheme-asd
  (:use :cl :asdf :ppcre))
(in-package :my-scheme-asd)

(defsystem :my-scheme
	:version "1.0.0"
	:author "wasu"
	:license "MIT"
	:components ((:file "package")
				 (:module "src" :components ((:file "my-scheme")))))
