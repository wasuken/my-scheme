(in-package :cl-user)
(defpackage my-scheme-test
  (:use :cl :prove :my-scheme))
(in-package #:my-scheme-test)

(plan 2)

(subtest "Testing parser"
		 (let ((result (my-scheme:parser "(+ 1 2)")))
		   (diag "first function")
		   (ok (functionp (car result)))
		   (diag "second arg-lst")
		   (ok (equal (cadr result)
		   			  (list 1 2)))
		   )
		 (let ((result (my-scheme:parser "(+ 1 (* 2 3)")))
		   (diag "first function")
		   (ok (functionp (car result)))
		   (diag "second arg-lst in function")
		   (ok (functionp (caadr (cadr result))))
		   )
		 )

(subtest "Testing lexer"
		 (is (my-scheme:lexer "(+ 1 2)")
			 3)
		 (is (my-scheme:lexer "(+ 1 (* 2 3)")
			 7)
		 (is (my-scheme:lexer "(+ (* 50 2) (* 25 4))")
			 200))
