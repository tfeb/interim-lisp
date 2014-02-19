;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File		     - singleton-class.lisp
;; Description	     - Singleton classes
;; Author	     - Tim Bradshaw (tfb at lostwithiel)
;; Created On	     - Tue Apr 30 14:22:26 2002
;; Last Modified On  - Thu Apr 19 23:12:37 2007
;; Last Modified By  - Tim Bradshaw (tfb at fowey.cley.com)
;; Update Count	     - 4
;; Status	     - Unknown
;; 
;; $Id: //depot/www-tfeb-org/before-2013-prune/www-tfeb-org/html/programs/lisp/singleton-class.lisp#1 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Singleton classes using the MOP
;;;
;;; singleton-class.lisp is copyright 2002 by me, Tim Bradshaw,
;;; and may be used for any purpose whatsoever by anyone. It has no
;;; warranty whatsoever. I would appreciate acknowledgement if you use
;;; it in anger, and I would also very much appreciate any feedback or
;;; bug fixes.
;;;

(defpackage :org.tfeb.hax.singleton-classes
  ;; use whatever you need to get the MOP
  (:use :cl 
	#+allegro :clos
	#+cmu :pcl
	#+lispworks :hcl
	#+sbcl :sb-mop)
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
  (:export #:singleton-class
	   #:reset-singleton-classes))

(in-package :org.tfeb.hax.singleton-classes)


(defclass singleton-class (standard-class)
  ((instance :initform nil)))

(defmethod validate-superclass ((class singleton-class)
                                (superclass standard-class))
  ;; it's OK for a standard class to be a superclass of a singleton
  ;; class
  t)

(defmethod validate-superclass ((class singleton-class)
                                (superclass singleton-class))
  ;; it's OK for a singleton class to be a subclass of a singleton class
  t)

(defmethod validate-superclass ((class standard-class)
                                (superclass singleton-class))
  ;; but it is not OK for a standard class which is not a singleton class
  ;; to be a subclass of a singleton class
  nil)

(defmethod make-instance ((class singleton-class)
                          &key)
  (with-slots (instance) class
    (or instance
        (setf instance (call-next-method)))))

(defvar *singleton-classes* '())

(defmethod initialize-instance :after ((c singleton-class) &key)
  (pushnew c *singleton-classes*))

(defun reset-singleton-classes ()
  ;; This means you will get new singletons from now on.
  (loop for c in *singleton-classes*
        do (setf (slot-value c 'instance) nil)))

#||
(defclass foo ()
  ()
  (:metaclass singleton-class))
||#

