;;;; Fake Global Lexical variables
;;; $Id: //depot/www-tfeb-org/before-2013-prune/www-tfeb-org/html/programs/lisp/glex.lisp#1 $

(in-package :cl-user)

;;; This makes the symbol-macro easier to write (want an error on unbound)
;;;
(declaim (inline glex-value (setf glex-value)))

(define-condition unbound-global-lexical (unbound-variable)
  ;; I feel guilty about using UNBOUND-VARIABLE because it isn't.
  ())

;;; This just uses property lists behind the scenes now
;;;

(defun glex-value (sym)
  (multiple-value-bind (boundp val) (get-properties (symbol-plist sym) 
                                                    '(glex-value))
    (unless boundp
      (error 'unbound-global-lexical :name sym))
    val))

(defun (setf glex-value) (new sym)
  (setf (get sym 'glex-value) new))

(defmacro defglex (x &optional (value nil valuep) (documentation nil docp))
  ;; DEFGLEX is like DEFVAR, (not DEFPARAMETER), but for global lexicals
  `(progn
     ,@(if valuep
           `((unless (get-properties (symbol-plist ',x) '(glex-value))
               (setf (glex-value ',x) ,value)))
         '())
     ;; This must be known at compile time so users can be compiled.
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (define-symbol-macro ,x (glex-value ',x)))
     ,@(if docp
           `((setf (documentation ',x 'variable) ',documentation))
         '())
     ',x))

(defmacro defglpar (x &optional (value nil valuep) (documentation nil docp))
  ;; DEFGLPAR is like DEFPARAMETER, but for global lexicals
  `(progn
     ,@(if valuep
           `((setf (glex-value ',x) ,value))
         '())
     ;; This must be known at compile time so users can be compiled.
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (define-symbol-macro ,x (glex-value ',x)))
     ,@(if docp
           `((setf (documentation ',x 'variable) ',documentation))
         '())
     ',x))


  
