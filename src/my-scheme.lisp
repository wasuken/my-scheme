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

(defparameter *variable-alist*
  `(("+" ,(function (lambda (x y) (+ x y))))
	("-" ,(function (lambda (x y) (- x y))))
	("*" ,(function (lambda (x y) (* x y))))
	("/" ,(function (lambda (x y) (/ x y))))
	("def" ,(function (lambda (x y) (push `(,x ,y) *variable-alist*))))
	("lambda"  ,(function (lambda (x y)　(lambda x y))))))

(defun getvar (var)
  (cadr (find-if #'(lambda (x) (string= (car x) var)) *variable-alist*)))

(defun setvar (var contents)
  (push `(,var ,contents) *variable-alist*))

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

(defun tree-remove-if (tree tar)
  (cond ((null tree) nil)
		((not (listp (car tree)))
		 (if (eql (car tree) tar)
			 (tree-remove-if (cdr tree) tar)
			 (append (list (car tree))
					 (tree-remove-if (cdr tree) tar))))
		((null (car tree)) (tree-remove-if (cdr tree) tar))
		((null (cdr tree)) (list (tree-remove-if (car tree) tar)))
		(t (append (list (tree-remove-if (car tree) tar))
				   (tree-remove-if (cdr tree) tar)))))

(defun parser (str)
  (let ((to-html-lst (tree-remove-if (nth 2
										  (nth 3
											   (closure-html:parse
												(ppcre:regex-replace-all "\\)"
																		 (ppcre:regex-replace-all "\\(" str "<a>")
																		 "</a>")
												(closure-html:make-lhtml-builder))))
										 " ")))
	(tree-remove-if to-html-lst :a)))
;;; 未実装
(defun intepretor ())

(defun lexer-method (lst)
  (let* ((formula-lst (remove-if #'(lambda (x) (= (length x) 0))
						 (mapcar #'(lambda (x) (cond ((null x) "")
													 ((listp x)
													  (lexer-method x))
													 ((and (stringp x)
														   (string= (string-trim '(#\Space) x) ""))
													  "")
													 (t x)))
								 lst)))
		 (formula-str (ppcre:regex-replace-all "\\s+"
											   (string-trim '(#\Space) (format nil "~{~A ~}"
																			   formula-lst))
											   " "))
		 (method (ppcre:scan-to-strings "^\\S+" formula-str))
		 ;; 今は数字のみ扱う
		 (args (mapcar #'(lambda (x)
						   (cond ((ppcre:scan-to-strings "^([a-z|A-Z][0-9|\\w]+|[a-z|A-Z])" x)
								  (if (string= method "def")
									  x
									  (getvar x)))
								 ((ppcre:scan-to-strings "^[0-9]+")
								  (parse-integer x))))
					   (cdr (ppcre:split " " formula-str)))))
	(print method)
	(write-to-string (funcall-args-cons (getvar method) args))))

(defun lexer (str)
  (lexer-method (parser str)))
