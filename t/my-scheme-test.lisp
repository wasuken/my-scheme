(in-package :cl-user)
(defpackage my-scheme-test
  (:use :cl :prove :my-scheme))
(in-package #:my-scheme-test)

;;; やりたいことをテストとして書いていきたい。

(plan 2)

(subtest "Testing parser"
  (let ((result (my-scheme:lexer "(+ 1 2)")))
	(diag "first function")
	(is result
		'(:|value| "+" :|type| :|method| :|args|
		  ((:|value| 1 :|type| :|integer| :|args| NIL)
		   (:|value| 2 :|type| :|integer| :|args| NIL))))
	)
  (let ((result (my-scheme:lexer "(+ 1 (* 2 3)")))
	(diag "second function")
	(is result
		'(:|value| "+" :|type| :|method| :|args|
		  ((:|value| 1 :|type| :|integer| :|args| NIL)
		   (:|value| "*" :|type| :|method| :|args|
			((:|value| 2 :|type| :|integer| :|args| NIL)
			 (:|value| 3 :|type| :|integer| :|args| NIL))))))
	)
  (let ((result (my-scheme:lexer "(quote ((10) 20 (30 40)))"))))
  )

(subtest "Testing eval"
  (is (my-scheme:my-eval "(+ 1 2)")
	  3)
  (is (my-scheme:my-eval "(+ 1 (* 2 3))")
	  7)
  (is (my-scheme:my-eval "(+ (* 50 2) (* 25 4))")
	  200))

(subtest "Variable Test"
  (my-scheme:my-eval "(def c 10)")
  (is (my-scheme:my-eval "(+ 1 c)") 11)
  (my-scheme:my-eval "(def d 11)")
  (is (my-scheme:my-eval "(+ c d)") 21))
