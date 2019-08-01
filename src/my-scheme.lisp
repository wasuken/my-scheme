(in-package #:my-scheme)

(defmacro when-bind* (binds &body body)
  (if (null binds)
	  `(progn ,@body)
	  `(let (,(car binds))
		 (if ,(caar binds)
			 (when-bind* ,(cdr binds) ,@body)))))

(defun funcall-args-cons (func args)
  (let ((run-lst `(,func funcall)))
	(dolist (arg args)
	  (push arg run-lst))
	(eval (reverse run-lst))))

(defparameter *methods-alist*
  `(("+" ,(function (lambda (x y) (+ x y))))
	("-" ,(function (lambda (x y) (- x y))))
	("*" ,(function (lambda (x y) (* x y))))
	("/" ,(function (lambda (x y) (/ x y))))))

(defun getmethod (method)
  (cadr (find-if #'(lambda (x) (string= (car x) method)) *methods-alist*)))

(defun setmethod (method lambda)
  (push '(method . lambda) *methods-alist*))

(defparameter *variable-number-alist*
  '())

(defun getvar (var)
  (cadr (find-if #'(lambda (x) (string= (car x) var)) *variable-number-alist*)))

(defun setvar (var contents)
  (push '(var . contents) *variable-number-alist*))

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

(defmacro case-reg-str (x &body body)
  (let ((cond-list `(cond)))
	(dolist (b body)
	  (if (eql t (car b))
		  (push `(t ,(cadr b))
				cond-list)
		  (push `((ppcre:scan-to-strings ,(car b) ,x)
				  ,(cadr b))
				cond-list)))
	(reverse cond-list)))

(defun tree-str-remove-if (tree tar)
  (cond ((null tree) nil)
		((not (listp (car tree)))
		 (if (string= (car tree) tar)
			 (tree-str-remove-if (cdr tree) tar)
			 (append (list (car tree))
					 (tree-str-remove-if (cdr tree) tar))))
		((null (car tree)) (tree-str-remove-if (cdr tree) tar))
		((null (cdr tree)) (list (tree-str-remove-if (car tree) tar)))
		(t (append (list (tree-str-remove-if (car tree) tar))
				   (tree-str-remove-if (cdr tree) tar)))))

(defun str-to-parens (str)
  (cond ((string= str "") nil)
		((null str) nil)
		(t (case-reg-str str
			 ("^\\(" (list "(" (str-to-parens (subseq str 1))))
			 ("^\\)" (cons ")" (str-to-parens (subseq str 1))))
			 ("^\\w[\\w|0-9]+" (cons (ppcre:scan-to-strings "^\\w[\\w|0-9]+" str)
									 (str-to-parens (subseq str (length (ppcre:scan-to-strings "^\\w[\\w|0-9]+" str))))))
			 ("^[0-9]+" (cons (ppcre:scan-to-strings "^[0-9]+" str)
							  (str-to-parens (subseq str (length (ppcre:scan-to-strings "^[0-9]+" str))))))
			 ("^\\s+" (str-to-parens (subseq str (length (ppcre:scan-to-strings "^\\s+" str)))))

			 (t (append (list (car (ppcre:split " " str))
							  (str-to-parens (format nil "~{~A ~}" (cdr (ppcre:split " " str)))))))))))

(defun intepretor ())

(defun parser-method (lst)
  (mapcar #'(lambda (x)
			  (cond ((listp x)
					 (parser-method x))
					((ppcre:scan-to-strings "\\(|\\)" x)
					 x)
					((ppcre:scan-to-strings "^[0-9]+" x)
					 (parse-integer x))
					((getmethod x)
					 (getmethod x))
					(t (getvar x))))
		  lst))

(defun parser (str)
  (let ((parens (car (tree-str-remove-if
					  (tree-str-remove-if
					   (tree-str-remove-if (str-to-parens str) "") "(") ")"))))
	(parser-method parens)))

(defun lexer-method (parens)
  (cond ((null (car parens)) nil)
		((functionp (car parens))
		 (funcall-args-cons (car parens)
							(mapcar #'(lambda (x) (if (listp x)
													  (lexer-method x)
													  x))
									(cadr parens))))))

(defun lexer (str)
  (lexer-method (parser str)))
