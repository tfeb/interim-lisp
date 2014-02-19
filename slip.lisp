;;; -*- Mode: LISP; Base: 10; Syntax: Ansi-common-lisp; Package: CL-USER -*-

(in-package :cl-user)

(defvar *slip-readtable* (copy-readtable))

(set-syntax-from-char #\] #\) *slip-readtable* *readtable*)

(set-macro-character #\[
		     #'(lambda (stream char)
			 (declare (ignore char))
			 (let ((l (read-delimited-list #\] stream t)))
			   (cond ((null (cdr l))
				  l)
				 ((eql (car l) '@)
				  `#'(lambda () ,@(cdr l)))
				 ((eql (cadr l) '@)
				  `#'(lambda ,(car l) ,@(cddr l)))
				 (t
				  `(,(cadr l) ,(car l) ,@(cddr l))))))
		     t *slip-readtable*)

(defun slip-rep ()
  (let ((*readtable* *slip-readtable*)
	(e (gensym)))
    (loop
      (multiple-value-bind (form errorp)
	  (with-simple-restart (continue-slip "Continue SLIP-REP")
	    (progn (format t "~&> "))
	    (read *standard-input* nil e))
	(when (eq form e)
	  (return-from slip-rep (values)))
	(if errorp
	    (princ "[Read error]" *error-output*)
	    (multiple-value-bind (val errorp)
		(with-simple-restart (continue-slip "Continue SLIP-REP")
		  (values (eval form)))
	      (if errorp
		  (princ "[Eval error]" *error-output*)
		  (print val *standard-output*))))))))

