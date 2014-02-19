;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File		     - abstract-classes.lisp
;; Description	     - Abstract classes in CL
;; Author	     - Tim Bradshaw (tfb at lostwithiel)
;; Created On	     - Sun Dec 10 18:21:40 2000
;; Last Modified On  - Tue Apr 30 14:23:27 2002
;; Last Modified By  - Tim Bradshaw (tfb at lostwithiel)
;; Update Count	     - 14
;; Status	     - Unknown
;; 
;; $Id: //depot/www-tfeb-org/before-2013-prune/www-tfeb-org/html/programs/lisp/abstract-classes.lisp#1 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Abstract classes
;;;
;;; abstract-classes.lisp is copyright 2000-2001 by me, Tim Bradshaw,
;;; and may be used for any purpose whatsoever by anyone. It has no
;;; warranty whatsoever. I would appreciate acknowledgement if you use
;;; it in anger, and I would also very much appreciate any feedback or
;;; bug fixes.
;;;

(defpackage :org.tfeb.hax.abstract-classes
  (:nicknames :org.tfeb.hax.final-classes)
  ;; use whatever you need to get the MOP
  (:use :cl 
	#+allegro :clos
	#+cmu :pcl
	#+lispworks :hcl)
  ;; CMU needs to get these things from PCL, not CL, as they are
  ;; different because it has this weirdo wrapper stuff such that
  ;; cl:standard-class is not pcl::standard-class & so on.  This will,
  ;; I hope, go away at some point, but it's true for cmucl 18c
  ;; 2000-09-07.
  #+cmu
  (:shadowing-import-from :pcl #:standard-class #:built-in-class
			  #:find-class #:class-name #:class-of)
  #+genera
  (:import-from :clos-internals #:validate-superclass)
  (:export #:abstract-class
	   #:define-abstract-class
	   #:final-class
	   #:define-final-class))

(in-package :org.tfeb.hax.abstract-classes)

(provide :org.tfeb.hax.abstract-classes)
(provide :org.tfeb.hax.final-classes)


(defclass abstract-class (standard-class)
  ()
  (:documentation "The class of abstract classes"))

(defmethod make-instance ((c abstract-class) &rest junk)
  (declare (ignore junk))
  (error "Trying to make an instance of ~A which is an abstract class"
	 (class-name c)))

;;; The MOP requires this, but it's not clear that implementations do.
;;; VALIDATE-SUPERCLASS specifies when a superclass is suitable for a
;;; subclass. You have to be pretty specific, It's probably not in
;;; general safe to do what we do here.
;;;

(defmethod validate-superclass ((class abstract-class) 
				(superclass standard-class))
  ;; This is, in general, somewhat too permissive, but we are going to
  ;; allow any instance of (a subclass of) STANDARD-CLASS to act as a
  ;; superclass of any instance of ABSTRACT-CLASS...
  t)

(defmethod validate-superclass ((class standard-class)
				(superclass abstract-class))
  ;; ... and the other way around.
  t)


;;; I don't want to have to say ... (:metaclass abstract-class), but
;;; there is no easy hook into processing the options to DEFCLASS:
;;; ENSURE-CLASS-USING-CLASS, which would be the logical place to do
;;; this, is called with a class of NIL if there is no existing class,
;;; and so can't usefully be specialized.
;;;

(defmacro define-abstract-class (class supers slots &rest options)
  (when (assoc ':metaclass options)
    (error "Defining an abstract class with a metaclass?"))
  `(defclass ,class ,supers ,slots
	     ,@options
	     (:metaclass abstract-class)))

;;; Samples of abstract classes
#||
(define-abstract-class abstract-thing ()
  ((s :accessor thing-s)))

(defclass thing (abstract-thing)
  ((s :initform 1)))
||#

;;; Benchmarks: for ACL 6.0 there is no performance hit.
#||
(define-abstract-class ac () ())
(defclass ac-instantiable (ac) ())
(defclass nac () ())
(defclass nac-instantiable (nac) ())

(defun make-n-aci (n)
  (declare (type fixnum n)
	   (optimize speed))
  (loop repeat n
      do (make-instance 'ac-instantiable)))

(defun make-n-naci (n)
  (declare (type fixnum n)
	   (optimize speed))
  (loop repeat n
      do (make-instance 'nac-instantiable)))

(defun make-n-general (n cn)
  (declare (type fixnum n)
	   (optimize speed))
  (loop repeat n
      do (make-instance cn)))
||#


;;;; Final classes
;;;
;;; Classes which may not be subclassed.
;;;
;;; I just know someone is going to ask for an abstract final class.

(defclass final-class (standard-class)
  ()
  (:documentation "The class of classes which may not be subclassed"))

;;; The MOP requires this, but it's not clear that implementations do.
;;; VALIDATE-SUPERCLASS specifies when a superclass is suitable for a
;;; subclass. You have to be pretty specific, It's probably not in
;;; general safe to do what we do here.
;;;

(defmethod validate-superclass ((class final-class) 
				(superclass standard-class))
  ;; This is, in general, somewhat too permissive, but we are going to
  ;; allow any instance of (a subclass of) STANDARD-CLASS to act as a
  ;; superclass of any instance of ABSTRACT-CLASS...
  t)

(defmethod validate-superclass ((class standard-class)
				(superclass final-class))
  (error "Attempting to subclass a final class"))


;;; I don't want to have to say ... (:metaclass final-class), but
;;; there is no easy hook into processing the options to DEFCLASS:
;;; ENSURE-CLASS-USING-CLASS, which would be the logical place to do
;;; this, is called with a class of NIL if there is no existing class,
;;; and so can't usefully be specialized.
;;;

(defmacro define-final-class (class supers slots &rest options)
  (when (assoc ':metaclass options)
    (error "Defining a final class with a metaclass?"))
  `(defclass ,class ,supers ,slots
	     ,@options
	     (:metaclass final-class)))
