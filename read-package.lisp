;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File		     - read-package.lisp
;; Description	     - Read-time package hacks
;; Author	     - Tim Bradshaw (tfb at lostwithiel)
;; Created On	     - Fri Jul  6 12:13:59 2001
;; Last Modified On  - Tue Apr 30 14:23:26 2002
;; Last Modified By  - Tim Bradshaw (tfb at lostwithiel)
;; Update Count	     - 13
;; Status	     - Unknown
;; 
;; $Id: //depot/www-tfeb-org/before-2013-prune/www-tfeb-org/html/programs/lisp/read-package.lisp#1 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Symbolics-style read-time packages.
;;;
;;; read-package.lisp is copyright 2001 by me, Tim Bradshaw, and may
;;; be used for any purpose whatsoever by anyone. It has no warranty
;;; whatsoever. I would appreciate acknowledgement if you use it in
;;; anger, and I would also very much appreciate any feedback or bug
;;; fixes.

;;; In genera you could give package prefixes to entire forms:
;;;     cl-user:(cons 1 2)
;;; meant `read in package CL-USER.
;;;
;;; This can't easily be implemented in standard CL, but something
;;; similar can.  This file implements a dispatching macro char, @,
;;; which forces the following form to be read in the package named:
;;; #@cl-user (cons 1 2) does about the same thing as above.
;;;
;;; The principal hack here is that CL gives no hook into the reader
;;; before it interns things, so you can't say `give me a token', but
;;; have to accept something like a symbol.  Hence the hack of using a
;;; secret package in which things get interned & then uninterned (to
;;; avoid leaks).
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (not (find-package ':org.tfeb.hax))
    (make-package ':org.tfeb.hax)))

(in-package :org.tfeb.hax)

(defvar *read-package-package* 
  (make-package "READ-PACKAGE-PACKAGE" :use '()))

(when (get-dispatch-macro-character #\# #\@)
  (error "Someone is already using #@"))

(set-dispatch-macro-character 
 #\# #\@ 
 #'(lambda (stream char infix)
     (declare (ignore char infix))
     (let* ((*package* *read-package-package*)
	    (tok (read stream t nil t))
	    (string (typecase tok
		      (symbol
		       (if (eq (symbol-package tok) *read-package-package*)
			   (unintern tok)
			   (warn 
			    "Dubious syntax for read-package: symbol in package ~A"
			    (package-name (symbol-package tok))))
		       (symbol-name tok))
		      (string
		       (warn "Dubious syntax for read-package: string read")
		       tok)
		      (t
		       (error "read-package: got a ~A, expecting a symbol"
			      (type-of tok)))))
	    (package (find-package string)))
       (unless package
	 (error "No package with name ~A for read-package" string))
       (let ((*package* package))
	 (read stream t nil t)))))
    
