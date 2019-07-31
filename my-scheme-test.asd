;; (require \'asdf)
 
 (in-package :cl-user)
 (defpackage my-scheme-test-asd
 (:use :cl :asdf))
 (in-package :my-scheme-test-asd)
 
 (defsystem my-scheme-test
 :depends-on (:my-scheme)
 :version "1.0.0"
 :author "wasu"
 :license "MIT"
 :components ((:module "t" :components ((:file "my-scheme-test"))))
 :perform (test-op :after (op c)
 (funcall (intern #.(string :run) :prove) c)))

