(in-package #:my-scheme)

(defun plistp (lst)
  (and (evenp (length lst))
	   (> (length (remove-if #'symbolp lst)) 0)))

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
	))

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

(defun type-check (str)
  (cond ((ppcre:scan-to-strings "^([a-z|A-Z][0-9|\\w]+|[a-z|A-Z])" str)
		 `(:|value| ,str :|type| :|symbol|))
		((ppcre:scan-to-strings "^[0-9]+" str)
		 `(:|value| ,(parse-integer str) :|type| :|integer|))
		((ppcre:scan-to-strings "\".*\"" str)
		 `(:|value| ,str :|type| :|string|))))

(defun str-to-abs-tree (str args)
  (let* ((nodes (remove-if-not #'(lambda (x) (ppcre:scan-to-strings "\\S+" x))
							   (ppcre:split "\\s+" str)))
		 (func (if (getvar (string-trim '(#\Space) (car nodes)))
				   (string-trim '(#\Space) (car nodes))
				   (getf (type-check (string-trim '(#\Space) (car nodes))) :|value|)))
		 (type (if (functionp (getvar (string-trim '(#\Space) (car nodes))))
				   :|method|
				   (getf (type-check str) :|type|))))
	`(:|value| ,func
	   :|type| ,type :|args| ,(if (null args)
								  (mapcar #'(lambda (x) (str-to-abs-tree x nil))
										  (cdr nodes))
								  (tree-replace-abs-node args)))))

(defun tree-replace-abs-node (tree)
  (cond ((null tree) nil)
		((and (null (car tree)) (null (cdr tree))) nil)
		((not (listp (car tree)))
		 (if (not (ppcre:scan-to-strings "^\\s+$" (car tree)))
			 (str-to-abs-tree (car tree) (cdr tree))
			 (if (not (null (cdr tree)))
				 (tree-replace-abs-node (cdr tree)))))
		(t (cons (tree-replace-abs-node (car tree))
				 (tree-replace-abs-node (cdr tree))))))

(defun lexer (str)
  (let ((to-html-lst (tree-remove-if (nth 2
										  (nth 3
											   (closure-html:parse
												(ppcre:regex-replace-all "\\)"
																		 (ppcre:regex-replace-all "\\(" str "<a>")
																		 "</a>")
												(closure-html:make-lhtml-builder))))
									 " ")))
	(tree-replace-abs-node (tree-remove-if to-html-lst :a))))

;;; 未実装
(defun intepretor ())

(defun ast-tree-to-element-list (args)
  (cond ((null args) nil)
		((not (plistp args))
		 (append (list (ast-tree-to-element-list (car args)))
				 (ast-tree-to-element-list (cdr args))))
		((and (eql (getf args :|type|) :|method|)
			  (string= (getf args :|value|) "quote"))
		 ;; そのままリストに変換する。
		 `(quote ,(mapcar #'ast-tree-to-element-list args)))
		((eql (getf args :|type|) :|method|)
		 (let ((as (mapcar #'ast-tree-to-element-list args)))
		   (funcall-args-cons (getf args :|value|) as)))
		((getf args :|value|)
		 (getf args :|value|))
		((listp (car args))
		 (append (list (ast-tree-to-element-list (car args)))
				 (ast-tree-to-element-list (cdr args))))))

(defun my-eval-method (ast-tree)
  (if (not (null ast-tree))
	  (if (plistp ast-tree)
		  (let ((type (getf ast-tree :|type|))
				(value (getf ast-tree :|value|))
				(args (getf ast-tree :|args|)))
			(cond ((null ast-tree) nil)
				  ((null type) (mapcar #'my-eval-method args))
				  ((and (eql type :|method|) (string= value "quote"))
				   ;; そのままリストに変換する。
				   `(quote ,(mapcar #'ast-tree-to-element-list args)))
				  ((eql type :|method|)
				   (print args)
				   (funcall-args-cons (getvar value)
									  (mapcar #'(lambda (x) (if (eql (getf x :|type|) :|method|)
																(my-eval-method (getf x :|args|))
																(getf x :|value|)))
											  args)))))
		  (append (list (ast-tree-to-element-list (car ast-tree)))
				  (ast-tree-to-element-list (cdr ast-tree))))))

(defun my-eval (str)
  (my-eval-method (lexer str)))
