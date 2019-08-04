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
		'((:|value| "+" :|type| :|method|)
		  (:|value| 1 :|type| :|integer|)
		  (:|value| 2 :|type| :|integer|)))
	)
  (let ((result (my-scheme:lexer "(+ 1 (* 2 3)")))
	(diag "second function")
	(is result
		'((:|value| "+" :|type| :|method|) (:|value| 1 :|type| :|integer|)
		  ((:|value| "*" :|type| :|method|) (:|value| 2 :|type| :|integer|)
		   (:|value| 3 :|type| :|integer|))))
	)
  )

;; (subtest "Testing eval"
;;   (is (my-scheme:eval "(+ 1 2)")
;; 	  "3")
;;   (is (my-scheme:eval "(+ 1 (* 2 3)")
;; 	  "7")
;;   (is (my-scheme:eval "(+ (* 50 2) (* 25 4))")
;; 	  "200"))

;; (subtest "Variable Test"
;;   (eval "(def c 10)")
;;   (is (eval "(+ 1 c)") "11")
;;   (eval "(def d 11)")
;;   (is (eval "(+ c d)") "21"))
