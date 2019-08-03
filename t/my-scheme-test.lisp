(in-package :cl-user)
(defpackage my-scheme-test
  (:use :cl :prove :my-scheme))
(in-package #:my-scheme-test)

;;; やりたいことをテストとして書いていきたい。

(plan 2)

(subtest "Testing parser"
  (let ((result (my-scheme:parser "(+ 1 2)")))
	(diag "first function")
	(ok result "+ 1 2")
	)
  (let ((result (my-scheme:parser "(+ 1 (* 2 3)")))
	(diag "first function")
	(is result '("+ 1 " ("* 2 3")))
	)
  )

(subtest "Testing lexer"
  (is (my-scheme:lexer "(+ 1 2)")
	  "3")
  (is (my-scheme:lexer "(+ 1 (* 2 3)")
	  "7")
  (is (my-scheme:lexer "(+ (* 50 2) (* 25 4))")
	  "200"))

(subtest "Variable Test"
  (lexer "(def c 10)")
  (is (lexer "(+ 1 c)") "11")
  (lexer "(def d 11)")
  (is (lexer "(+ c d)") "21"))
