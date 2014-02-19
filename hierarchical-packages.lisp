;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File		     - hp.lisp
;; Description	     - cleaned-up hierarchical packages from acl 6
;; Author	     - Tim Bradshaw (tfb at lostwithiel)
;; Created On	     - Wed Aug 30 01:44:37 2000
;; Last Modified On  - Wed Feb  6 23:06:09 2013
;; Last Modified By  - Tim Bradshaw (tfb at kingston.local)
;; Update Count	     - 12
;; Status	     - Unknown
;; 
;; $Id: //depot/www-tfeb-org/before-2013-prune/www-tfeb-org/html/programs/lisp/hierarchical-packages.lisp#1 $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This code is based on that found at
;;; http://www.franz.com/support/documentation/6.0/doc/packages.htm,
;;; and is probably related to later versions of this code, although
;;; no attempt has been made to merge any later changes.  The
;;; following notice comes from the original file:
;;;
;;; The following source code is in the public domain.  Provided "as
;;; is" with no warranty of any kind.  Use at your own risk.
;;;
;;; Despite the header above, I, Tim Bradshaw, am not the author of this
;;; code, I just changed it a bit.
;;;
;;; This code supports LispWorks, Clozure CL, CMU CL, SBCL.  Although
;;; this is derived from Allegro code it has not been tested in
;;; Allegro: the conditionalisation around the redefinition of
;;; CL:FIND-PACKAGE would need to be changed and that redefinition
;;; would need to be checked in Allegro.
;;;
;;; Support for Clozure CL and SBCL from Jason Aeschliman & Andrew
;;; Philpot: thanks!
;;;
;;; NOTE: redefining things interactively in this file can cause
;;; catastrophic failures if (for instance) CL:FIND-PACKAGE is called
;;; an an inopportune moment during the redefinition (which is quite
;;; likely).  For instance if CL:FIND-PACKAGE is called while
;;; *PER-PACKAGE-ALIAS-TABLE* is not bound or is bound to something
;;; not a hashtable then bad things will happen.  It should be safe to
;;; compile and reload the file as a whole assuming there are no
;;; errors.
;;;

;;; This is broken in CCL
#+CCL
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "Not currently working in CCL"))

(defpackage :org.tfeb.hierarchical-packages
  (:nicknames 
   :org.tfeb.hax.relative-package-names
   :org.tfeb.hax.hierarchical-packages)
  (:use :cl)
  (:export #:*per-package-alias-table* #:*find-package-hook*))

(in-package :org.tfeb.hierarchical-packages)

(provide :org.tfeb.hierarchical-packages)

(pushnew :org.tfeb.relative-package-names *features*)
(pushnew :org.tfeb.hierarchical-packages *features*)

(defvar *find-package-hook* nil
  ;; Prototype hook implementation
  "If this is not NIL, it should be a function of one argument, a package name.
The function should return two values
- a package and T: use this package
- NIL and T: lookup failed
- NIL and NIL: call the underlying package lookup implementation
When the function is called, this hook is bound to NIL.  The function is never called
with an argument which is a package.")

;;; Stash the original definition of CL:FIND-PACKAGE. Do this at
;;; compile time too, to stop the compiler complaining about unknown
;;; functions.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (not (fboundp 'real-find-package))
    (setf (symbol-function 'real-find-package)
	  (symbol-function 'cl:find-package))))

(define-condition hierarchical-package-error (package-error)
  ())

(define-condition simple-hierarchical-package-error
    (hierarchical-package-error simple-condition)
  ())

(declaim (inline ps->string))
(defun ps->string (ps)
  (declare (type (or package symbol string character) ps))
  (typecase ps
    (package (package-name ps))
    (symbol (symbol-name ps))
    (string ps)
    (character (string ps))
    ;; this kind of can't be a hierarchical-package-error...
    (t (error "Illegal package specifier: ~s." ps))))
	      
(defun relative-package-name-to-package (name)
  ;; Given a package name, a string, do a relative package name lookup.
  ;;
  ;; It is intended that this function will be called from find-package.
  ;; In Allegro, find-package calls package-name-to-package, and the latter
  ;; function calls this function when it does not find the package.
  ;;
  ;; Because this function is called via the reader, we want it to be as
  ;; fast as possible.
  (declare (optimize speed)
	   #+(or CMU SBCL)
	   (type simple-base-string name)
	   #-(or CMU SBCL)
	   (type string name))
  (flet ((relative-to (package name)
	   (declare (type string name))
           (if (string= "" name)
              package
              (real-find-package
	       (concatenate 'simple-string
			    (package-name package) "." name))))
         (find-non-dot (name)
	   #+(or CMU SBCL)
	   (declare (type simple-base-string name))
	   #-(or CMU SBCL) 
	   (declare (type string name))
           (do* ((len (length name))
                 (i 0 (1+ i)))
               ((= i len) nil)
             (declare (fixnum len i))
             (when (char/= #\. (char name i)) (return i)))))
    (when (char= #\. (char name 0))
      (let* ((last-dot-position (or (find-non-dot name) (length name)))
             (n-dots last-dot-position)
             (name (subseq name last-dot-position)))
        (cond ((= 1 n-dots)
               ;; relative to current package
               (relative-to *package* name))
              (t
               ;; relative to our (- n-dots 1)'th parent
               (let ((p *package*)
                     tmp)
                 (dotimes (i (1- n-dots))
                   (when (not (setq tmp (package-parent p)))
                     (error 'simple-hierarchical-package-error
                            :package p
                            :format-control "The parent of ~a does not exist."
                            :format-arguments (list p)))
                   (setq p tmp))
                 (relative-to p name))))))))

(defun package-parent (package-specifier)
  ;; Given package-specifier, a package, symbol or string, return the
  ;; parent package.  If there is not a parent, signal an error.
  ;;
  ;; Because this function is called via the reader, we want it to be as
  ;; fast as possible.
  (declare (optimize speed)
	   (type (or package symbol string character) package-specifier))
  (flet ((find-last-dot (name)
	   (declare (type string name))
           (do* ((len (1- (length name)))
                 (i len (1- i)))
               ((= i -1) nil)
             (declare (fixnum len i))
             (when (char= #\. (schar name i)) (return i)))))
    (let* ((child (ps->string package-specifier))
           (dot-position (find-last-dot child)))
      (cond (dot-position
             (let ((parent (subseq child 0 dot-position)))
               (or (real-find-package parent)
                   (error 'simple-hierarchical-package-error
                          :package child
                          :format-control "The parent of ~a does not exist."
                          :format-arguments (list child)))))
            (t (error 'simple-hierarchical-package-error
                      :package child
                      :format-control "There is no parent of ~a." 
                      :format-arguments (list child)))))))

(defun package-children (package-specifier &key (recurse t))
  ;; Given package-specifier, a package, symbol, character or string,
  ;; return all the packages which are in the hierarchy "under" the
  ;; given package.  If :recurse is nil, then only return the
  ;; immediate children of the package.
  ;;
  ;; While this function is not called via the reader, we do want it to be
  ;; fast.
  (declare (optimize speed)
	   (type (or package symbol string character) package-specifier))
  (let ((res ())
        (parent (ps->string package-specifier)))
    (labels
        ((string-prefix-p (prefix string)
           ;; Return length of `prefix' if `string' starts with `prefix'.
           ;; We don't use `search' because it does much more than we need
           ;; and this version is about 10x faster than calling `search'.
           (let ((prefix-len (length prefix))
                 (seq-len (length string)))
             (declare (fixnum prefix-len seq-len)
		      #+cmu
		      (type simple-base-string string))
             (when (>= prefix-len seq-len)
               (return-from string-prefix-p nil))
             (do* ((i 0 (1+ i)))
                 ((= i prefix-len) prefix-len)
               (declare (fixnum i))
               (when (not (char= (schar prefix i) (schar string i)))
                 (return nil)))))
         (test-package (package-name package)
           (let ((prefix
                  (string-prefix-p (concatenate 'simple-string parent ".")
                                   package-name)))
             (cond (recurse (when prefix (pushnew package res)))
                   (t (when (and prefix
                                 (not (find #\. package-name :start prefix)))
                        (pushnew package res)))))))

      ;; In Allegro, list-all-packages calls `sort', so use an internal
      ;; method to get the package names.
      #+Allegro
      (maphash #'test-package *package-names*)
      #-Allegro
      (dolist (package (list-all-packages))
        (funcall #'test-package (package-name package) package))
      
      res)))

;;; Per package aliases.  Nothing in this code actually defines these.
;;;

(defvar *per-package-alias-table*
  ;; maps from package -> alist of alias -> real names.
  ;; Lookups are not recursive in this list.
  ;; (could this be a constant?)
  ;;
  ;; If the implementation supports it, this should be weak on its
  ;; keys, so that if a package is deleted its alias mappings also
  ;; evaporate.  CCL requires weak tables to have a test of EQ and
  ;; since EQ and EQL are the same for packages, we use that
  ;; throughout.
  #+LispWorks (make-hash-table :test #'eq :weak-kind ':key)
  #+CCL (make-hash-table :test #'eq :weak ':key)
  #+SBCL (make-hash-table :test #'eq :weakness ':key)
  #+Allegro (make-hash-table :test #'eq :weak-keys t)
  #-(or LispWorks CCL SBCL) (make-hash-table :test #'eq))


;;; Hook function
;;;
(defun find-package-hook (package)
  (declare (optimize speed))
  ;; PN is package name, EPN is effective (aliased) name
  ;; if there is one
  (let* ((pn (string package))
         (map (gethash *package* *per-package-alias-table*))
         (epn (and map (cdr (assoc pn map :test #'string=))))
         ;; if there is an EPN, then do FIND-PACKAGE on it, 
         ;; otherwise use PACKAGE. not PN, in case it can do some
         ;; magic.  Otherwise look up a relative name.
         (found (or (find-package (or epn package))
                    (relative-package-name-to-package (or epn pn)))))
    (if found
        (values found t)
      (values found nil))))

(eval-when (:load-toplevel :execute)
  (if (null *find-package-hook*)
      (setf *find-package-hook* #'find-package-hook)
    (warn "Already hooked, not hooking package lookup")))

;;; This stuff is probably very system-dependent: this assumes that
;;; (a) you can redefine FIND-PACKAGE (may need to unlock the CL
;;; package), and (b) the reader actually goes through FIND-PACKAGE.
;;;
;;; This works OK for CMUCL, not for CLISP or Genera, which both have
;;; locks and don't go through FIND-PACKAGE.
;;;

#+(or lispWorks CMU CCL SBCL)
(let (#+LispWorks (lispworks:*handle-warn-on-redefinition* nil)
      #+CCL (ccl:*warn-if-redefine-kernel* nil))
  (unwind-protect
      (progn
        #+SBCL (sb-ext:unlock-package :common-lisp)
        (defun cl:find-package (name/package)
          (declare (optimize speed))          ;this is critical code
          (typecase name/package
            (package name/package)
            (t                                ;should be STRINGable
             (if (not (null *find-package-hook*))
                 (let ((hook *find-package-hook*)
                       (*find-package-hook* nil))
                   (multiple-value-bind (package foundp) (funcall hook name/package)
                     (if foundp 
                         (if (or (packagep package) (null package))
                             package
                           (progn
                             (warn "bogus object returned by package hook")
                             (real-find-package name/package)))
                       (real-find-package name/package)))))))))
    ;; unwind
    #+SBCL (sb-ext:lock-package :common-lisp)))


;;; CCL needs CCL::PKG-ARG to be redefined.  There is also a compiler
;;; macro, and I am not sure about that.  Anyway, it is easier to just
;;; switch package so the definition looks like CCL's rather than
;;; having a mass of internal symbol refs
;;;

#+CCL
(in-package "CCL")

#+CCL
(let ((*warn-if-redefine-kernel* nil))
  (defun pkg-arg (thing &optional deleted-ok)
    (let* ((xthing (cond ((or (symbolp thing) (typep thing 'character))
                          (string thing))
                         ((typep thing 'string)
                          (ensure-simple-string thing))
                         (t
                          thing))))
      (let* ((typecode (typecode xthing)))
        (declare (fixnum typecode))
        (cond ((= typecode target::subtag-package)
               (if (or deleted-ok (pkg.names xthing))
                   xthing
                 (error "~S is a deleted package ." thing)))
              ((= typecode target::subtag-simple-base-string)
               (or (cl:find-package xthing)
                   (%kernel-restart $xnopkg xthing)))
              (t (report-bad-arg thing 'simple-string)))))))

#+CCL
(in-package :org.tfeb.hax.hierarchical-packages) ;in case there is ever more code

#-(or LispWorks CMU CCL SBCL)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (warn "PACKAGE SYSTEM NOT MODIFIED! you need to add glue"))
