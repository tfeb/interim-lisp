;;;; NCASE - optimise CASE to a jump table
;;; $Id: //depot/www-tfeb-org/before-2013-prune/www-tfeb-org/html/programs/lisp/ncase.lisp#1 $

;;; This is an example of using macros to do some fairly low-level
;;; optimisation.  NCASE is like CASE, but in suitable cases it will
;;; compile a jump-table instead of lots of tests.  This is actually
;;; not a really good example because the compiler is likely to be
;;; able to do a better job of the decisions involved, and it can also
;;; use real jumps rather than function calls which only may turn into
;;; jumps.
;;;
;;; This code has had basically no testing and may be buggy.
;;;

(in-package :cl-user)

;;; *NCASE-PREDICATE* gets called with a key list (all integers)
;;; and can decide if this should be optimised or not.  This version
;;; only optimises if there are enough keys (so the NCASE statement is
;;; fairly big, requiring lots of tests, and if they are dense enough
;;; in their range to avoid making a huge sparse array.

(defvar *ncase-length-threshold* 10)

(defvar *ncase-density-threshold* 0.2)

(defvar *ncase-predicate* 
  #'(lambda (keys)
      (let ((l (length keys))
            (r (- (reduce #'max keys)
                  (reduce #'min keys))))
        (and (>= l *ncase-length-threshold*)
             (not (zerop r))            ;weird case with one clause
             (>= (/ l r)
                 *ncase-density-threshold*)))))

(defmacro ncase (v &body clauses)
  ;; Semantics just like CASE, unless I've got it wrong.
  (if (every #'(lambda (clause)
                 (and (consp clause)
                      (let ((key/s (first clause)))
                        ;; what about NIL: just give up for now.
                        (or (and (consp key/s)
                                 (every #'integerp key/s))
                            (integerp key/s)
                            (member key/s '(otherwise t))))))
             clauses)
      ;; OK this is a candidate for us.
      (let ((keys '())
            (key-body-map '())
            (otherwise '(nil)))
        (loop with seen-otherwise-p = nil
              for (key/s . body) in clauses
              do
              (cond ((member key/s '(otherwise t))
                     (when seen-otherwise-p
                       (error "Multiple OTHERWISE clauses."))
                     (setf otherwise body
                           seen-otherwise-p t))
                    ((integerp key/s)
                     (when (member key/s keys)
                       (error "Multiple identical keys"))
                     (push key/s keys)
                     (let ((found (rassoc body key-body-map
                                          :test #'equal)))
                       (if found
                           (push key/s (car found))
                         (push (cons (list key/s) body)
                               key-body-map))))
                    ((consp key/s)
                     (loop for key in key/s
                           do
                           (when (member key keys)
                             (error "Multiple identical keys"))
                           (push key keys)
                           (let ((found (rassoc body key-body-map
                                                :test #'equal)))
                             (if found
                                 (push key (car found))
                               (push (cons (list key) body)
                                     key-body-map)))))
                    (t (error "This can't happen"))))
        ;; OK, now we are set...
        (if (funcall *ncase-predicate* keys)
            ;; dense enough
            (let ((mname (make-symbol "MAP"))
                  (vname (make-symbol "V"))
                  (min (reduce #'min keys))
                  (max (reduce #'max keys)))
              `(let ((,vname ,v))
                 (if (or (not (integerp ,vname))
                         (not (<= ,min ,vname ,max)))
                     (progn ,@otherwise)
                   ;; The map is bound here just so we can
                   ;; optimize only the bit we want.
                   (let ((,mname
                          ;; Use LOAD-TIME-VALUE to avoid any issues
                          ;; of dumping the map in a FASL - especially
                          ;; make sure that functions in it are not 
                          ;; duplicated which I can't otherwise see how
                          ;; to do.  This may be hopeless in interpreted
                          ;; code.
                          (load-time-value
                           (let ((,mname (make-array ,(1+ (- max min)))))
                             ,@(loop with lname = (make-symbol "L")
                                     and iname = (make-symbol "I")
                                     for (keys . body) in
                                     (cons (cons
                                            (loop for i from min to max
                                                  unless (member i keys) 
                                                  collect i)
                                            otherwise)
                                           key-body-map)
                                     collect
                                     `(loop with ,lname = (lambda () ,@body)
                                            for ,iname in ',keys
                                            do (setf (aref ,mname (- ,iname
                                                                     ,min))
                                                     ,lname)))
                             ,mname)
                           ;; it's a constant
                           t)))
                     (locally
                       (declare (optimize speed (safety 0)))
                       (funcall (aref ,mname (- ,vname ,min))))))))
          ;; punt - failed *NCASE-PREDICATE*
          `(case ,v ,@clauses)))
    ;; punt - not all keys are integers
    `(case ,v ,@clauses)))
  
