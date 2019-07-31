(in-package #:my-scheme)

(defparameter *methods-alist*
  '((+ . #'(lambda (x y) (+ x y)))))

(defun getmethod (method)
  (find-if #'(lambda (x) (string= (car x) var)) *methods-alist*))

(defun setmethod (method lambda)
  (push '(method . lambda) *methods-alist*))

(defparameter *variable-number-alist*
  '())

(defun getvar (var)
  (find-if #'(lambda (x) (string= (car x) var)) *variable-number-alist*))

(defun setvar (var contents)
  (push '(var . contents) *variable-number-alist*))

(defun str-to-parens (str)
  (if (string= (subseq str 0 1) "(")
	  (let* ((inside-list-str (subseq str 1 (1- (length str))))
			 (inside-list (ppcre:split " " inside-list-str)))
		(list "(" (mapcar #'(lambda (x)
							  (if (string= (subseq x 0 1) "(")
								  (str-to-parens x)
								  x))
						  inside-list)
			  ")"))
	  str))

(defmacro case-str (x &body body)
  (let ((cond-list `(cond)))
	(dolist (b body)
	  (if (eql t (car b))
		  (push `(t ,(cadr b))
				cond-list)
		  (push `((string= ,x ,(car b))
				  ,(cadr b))
				cond-list)))
	(reverse cond-list)))

(defun intepretor ())

(defun parser (str)
  (let ((parens (str-to-parens str)))
	))

(defun lexer-method (parens)
  )

(defun lexer (str)
  (let ((parens (str-to-parens str)))
	(case-str (subseq (car parens) 0 1)
	  ("(" (lexer-method (cdr parens)))
	  (t (getvar (subseq (car parens) 0 1))))))
