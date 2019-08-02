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

(defun str-parse-method (lst)
  (let* ((formula-str (ppcre:regex-replace-all "\\s+" (string-trim '(#\Space) (format nil "~{~A ~}"
													  (remove-if #'(lambda (x) (= (length x) 0))
																 (mapcar #'(lambda (x) (cond ((null x) "")
																							 ((equal :a x) "")
																							 ((listp x)
																							  (str-parse-method x))
																							 ((and (stringp x)
																								  (string= (string-trim '(#\Space) x) ""))
																							  "")
																							 (t x)))
																		 lst))))
											   " "))
		 (method (ppcre:scan-to-strings "^\\S+" formula-str))
		 ;; 今は数字のみ扱う
		 (args (mapcar #'(lambda (x) (if (not (string= "" x)) (parse-integer x))) (cdr (ppcre:split " " formula-str)))))
	(write-to-string (funcall-args-cons (getmethod method) args))))

(defun str-parse (str)
  (let ((to-html-lst (tree-str-remove-if (nth 2
										  (nth 3
											   (closure-html:parse
												(ppcre:regex-replace-all "\\)"
																		 (ppcre:regex-replace-all "\\(" str "<a>")
																		 "</a>")
												(closure-html:make-lhtml-builder))))
										 " ")))
	(str-parse-method to-html-lst)))

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
  (str-parse str))

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
