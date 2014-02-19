;;;; LW commands
;;; $Id: //depot/www-tfeb-org/before-2013-prune/www-tfeb-org/html/programs/lisp/lw-commands.lisp#1 $

;;; lw-commands.lisp is copyright 2002, 2012 by me, Tim Bradshaw, and may be
;;; used for any purpose whatsoever by anyone. It has no warranty
;;; whatsoever. I would appreciate acknowledgement if you use it in
;;; anger, and I would also very much appreciate any feedback or bug
;;; fixes.

(defpackage :org.tfeb.lw.lw-commands
  (:add-use-defaults t)
  (:export 
   #:declare-extra-lw-commands
   #:*module-path-descriptions*
   #:add-module-path-descriptions
   #:require-module
   #:forgetting-systems))               ;should be elsewhere...

(in-package :org.tfeb.lw.lw-commands)

(provide :org.tfeb.lw.lw-commands)
(pushnew :org.tfeb.lw.lw-commands *features*)

(defvar *extra-lw-commands* '())

(defmacro declare-extra-lw-commands (&body command-forms)
  `(loop for c in ',command-forms
         do (let ((found (assoc (car c) *extra-lw-commands*)))
              (if found 
                  (setf (cdr found) (cdr c))
                (setf *extra-lw-commands*
                      (nconc *extra-lw-commands* (list c)))))
         finally (return (values))))

(defadvice (sys::%top-level extra-commands :around)
    (&rest args &key (extra-commands nil) &allow-other-keys)
  (if (null extra-commands)
      ;; I suppose this is OK, though I'm not sure it should be!
      (apply #'call-next-advice :extra-commands *extra-lw-commands*
             args)
    (apply #'call-next-advice :extra-commands (append *extra-lw-commands*
                                                      extra-commands)
           args)))

;;;; Package stack
;;;

(defvar *package-stack* '())

;;; must bind per process
(unless (assoc '*package-stack* mp:*process-initial-bindings*)
  (push (list '*package-stack*) mp:*process-initial-bindings*))

(defun package-command (cmd &rest args)
  (case cmd
    ((:pkg>)
     (when (or (null args) (not (null (cdr args))))
       (warn "Expecting a package name, got ~S" args)
       (return-from package-command (values)))
     (destructuring-bind (p/n) args
       (let ((p (find-package p/n)))
         (when (null p)
           (warn "No package ~S" p/n)
           (return-from package-command (values)))
         (push *package* *package-stack*)
         (setf *package* p))))
    ((:pkg~)
     (unless (null (rest args))
       (warn "ignoring spurious arguments"))
     (when (null *package-stack*)
       (warn "Empty stack")
       (return-from package-command (values)))
     (let ((pkg *package*))
       ;; this stops it setting *PACKAGE* even if ROTATEF fails
       (rotatef pkg (nth (or (first args) 0) *package-stack*))
       (setf *package* pkg)))
    ((:pkg<)
     (unless (null (rest args))
       (warn "Ignoring spurious arguments"))
     (loop repeat (or (first args) 1)
           do
           (when (null *package-stack*)
             (warn "Empty stack")
             ;; I think it is OK to return *PACKAGE* here, 
             ;; as it seems reasonable to sat :pkg< 100
             ;; to drain the stack
             (return-from package-command *package*))
           (setf *package* (pop *package-stack*)))
     *package*)
    ((:pkg)
     (when (or (null args) (not (null (cdr args))))
       (warn "Expecting a package name, got ~S" args)
       (return-from package-command (values)))
     (destructuring-bind (p/n) args
       (let ((p (find-package p/n)))
         (when (null p)
           (warn "No package ~S" p/n)
           (return-from package-command (values)))
         (setf *package* p))))
    ((:pkg?)
     (when (not (null args))
       (warn "Ignoring arguments"))
     (format t "~&Current package ~A~%~@[Package stack ~{~<~%              ~:;~A~>~^ ~}~%~]"
             (package-name *package*)
             (mapcar #'package-name *package-stack*))
     (values))
    (otherwise
     (warn "Unknown package command ~S" cmd)
     (values))))

(declare-extra-lw-commands
  (:pkg> package-command "Push a package")
  (:pkg< package-command "Pop a package")
  (:pkg~ package-command "Swap packages")
  (:pkg package-command "Set package")
  (:pkg? package-command "Show package stack"))
                

;;;; Doing things to files
;;;

(defvar *file-command-default-files '())

(defun file-command (cmd &rest args)
  (let ((files (if args
                   (setf *file-command-default-files args)
                 *file-command-default-files)))
    (case cmd
      ((:ld)
       (dolist (f files (values))
         (load f)))
      ((:cf)
       (dolist (f files (values))
         (compile-file f)))
      ((:cl)
       (dolist (f files (values))
         (let ((cp (compile-file-pathname f))
               (sp (merge-pathnames f (load-time-value
                                       (make-pathname :type "lisp")))))
           (when (or (not (probe-file cp))
                     (< (file-write-date cp) (file-write-date sp)))
             (compile-file f))
           (load cp))))
      (otherwise
       (warn "Unknown file command ~S" cmd)
       (values)))))

(declare-extra-lw-commands
  (:ld file-command "Load a file")
  (:cf file-command "Compile a file")
  (:cl file-command "Compile a file and load it"))


;;;; Doing things to systems
;;;

(defvar *system-command-default-args* '())

(defun system-command (cmd &rest args)
  (case cmd
    ((:sysdcl)
     (loop for d in (or args (list (get-working-directory)))
           do
           (if (typep d '(or string pathname))
               (let* ((sysdcl (merge-pathnames 
                               d 
                               (load-time-value 
                                (make-pathname :name "sysdcl" 
                                               :type "lisp"))))
                      (sysdclc (compile-file-pathname sysdcl))
                      (sysdclt (and (probe-file sysdcl)
                                    (file-write-date sysdcl)))
                      (sysdclct (and (probe-file sysdcl)
                                     (file-write-date sysdcl))))
                 (if sysdclct
                     (load (if sysdclt
                               (if (> sysdclct sysdclt)
                                   sysdclc
                                 sysdcl)
                             sysdclc))
                   (if sysdclt
                       (load sysdcl)
                     (warn "Can't find sysdcl, tried ~A and ~A"
                           sysdcl sysdclc))))
             (warn "~S doesn't look like a pathname" d))))
    ((:lss)
     (unless (null args)
       (warn "Ignoring arguments"))
     (format t "~&~{~<~%~:;~S~>~^ ~}~%"
             (mapcar #'scm:module-name (scm:all-systems))))
    (otherwise
     (let ((args (if args
                     (setf *system-command-default-args* args)
                   *system-command-default-args*)))
       (case cmd
         ((:lds)
          (unless (scm:find-system (first args) nil)
            (warn "No system ~S" (first args))
            (return-from system-command (values)))
          (apply #'load-system args))
         ((:cps)
          (unless (scm:find-system (first args) nil)
            (warn "No system ~S" (first args))
            (return-from system-command (values)))
          (apply #'compile-system args))))))
  (values))
         
(declare-extra-lw-commands
  (:sysdcl system-command "Load sysdcl files")
  (:lss system-command "List known systems")
  (:cps system-command "Compile a system")
  (:lds system-command "Load a system"))


;;;; Inspector
;;;

(defun gi (cmd &rest args)
  (case cmd
    ((:gi)
     (let ((*inspect-through-gui* t))
       (declare (special *inspect-through-gui*))
       (cond (args
              (mapc #'(lambda (a)
                        (inspect (eval a)))
                    args))
             ((null /))
             ((null (cdr /))
              (inspect (car /)))
             (t (inspect /))))))
  (values))

(declare-extra-lw-commands
  (:gi gi "Inspect graphically"))


;;;; Background
;;;
     
(defun bg (cmd &rest forms)
  (case cmd
    ((:&)
     (if forms
         (mp:process-run-function
          (format nil "~{~S~^ ~}" forms)
          '()
          (compile nil `(lambda ()
                          ,@forms)))
       (progn
         (warn "Need something to run")
         (values))))))

(declare-extra-lw-commands
  (:& bg "Run in the background"))


;;;; Module loading.
;;;

(defvar *module-path-descriptions*
  ;; A list of (logical) path descriptions.  Each element should have a 
  ;; wild name component with a single wildcard, into which the name is spliced
  ;; using "*" as a from-wildcard.  If it does not have a wild name, then
  ;; the name is used as the last component of the directory only.
  ;;
  ;; Note this is Cley-specific!
  '((:host "CLEY"
     :directory (:absolute "LIB" "MODULES")
     :name "*-LOADER"
     :type "LISP")
    (:host "CLEY"
     :directory (:absolute "LIB" "MODULES")
     :name "LOADER"
     :type "LISP")
    (:host "CLEY"
     :directory (:absolute "LIB" "MODULES")
     :name "*"
     :type "LISP")))

(defun add-module-path-descriptions (descs &key (before t) (uniquely t) (test #'equal))
  ;; Add some module path descriptions: by default add them at the start, and only
  ;; add them if they are not already there.  Do not fash about the efficiency of this:
  ;; you do not have that many descriptions.
  (let ((mpds (copy-list *module-path-descriptions*)))
    (if before
        (dolist (desc (reverse descs))
          (if uniquely
              (pushnew desc mpds :test test)
            (push desc mpds)))
      (dolist (desc descs)
        (when (or (not uniquely) (not (member desc mpds :test test)))
          (setf mpds (nconc mpds descs)))))
    (setf *module-path-descriptions* mpds)))


(defun locate-module (module-name &key (verbose nil))
  ;; MODULE-NAME is a string or symbol, maybe with . chars in.  This
  ;; function encodes the search order, which is hairy but basically
  ;; as follows (this is just a sample).
  ;; Component is :COM.CLEY.WELD, implied pathnames from 
  ;; *MODULE-PATH-DESCRIPTIONS* CLEY:LIB;MODULES;*-LOADER.LISP and 
  ;; CLEY:LIB;MODULES;LOADER.LISP.  Then search for the following paths,
  ;; (using upcase variants of the module name only)
  ;; (1a) CLEY:LIB;MODULES;COM;CLEY;WELD;WELD-LOADER.LISP, 
  ;; (1b) CLEY:LIB;MODULES;COM;CLEY;WELD-LOADER.LISP, 
  ;; (2a) CLEY:LIB;MODULES;COM;CLEY;WELD;LOADER.LISP,
  ;; (2b) CLEY:LIB;MODULES;WELD;WELD-LOADER.LISP,
  ;; (3) CLEY:LIB;MODULES;WELD-LOADER.LISP,
  ;; (4) CLEY:LIB;MODULES;WELD;LOADER.LISP.
  ;; For each of these, take the compiled file if it exists and is newer
  ;; (or if the source does not exist)
  ;; If it exists and is older take the source file and warn.
  ;; In the above, (1a) and (1b) come from the first elt of 
  ;; *MODULE-PATH-DESCRIPTIONS*, (2a) and (2b) come from the second, (3) comes
  ;; from the first ignoring all but the last part of the module namd, and (4)
  ;; comes from the second in the same way.
  ;; If the elements of *MODULE-PATH-DESCRIPTIONS* do not specify logical
  ;; pathnames then for each try (each number above), try the name as is,
  ;; then downcased, then upcased.  So in the above, we'd get for (1a) and (1b):
  ;; (1a.s) /cley/lib/modules/COM/CLEY/WELD/WELD-loader.lisp, 
  ;; (1b.s) /cley/lib/modules/COM/CLEY/WELD-loader.lisp, 
  ;; (1a.d) /cley/lib/modules/com/cley/weld/weld-loader.lisp, 
  ;; (1b.d) /cley/lib/modules/com/cley/weld-loader.lisp, 
  ;; (1a.u) /cley/lib/modules/COM/CLEY/WELD/WELD-loader.lisp, 
  ;; (1b.u) /cley/lib/modules/COM/CLEY/WELD-loader.lisp
  ;; .. and so on
  ;;
  ;; Yes, it's complicated.
  (when verbose
    (format *standard-output* "~&Looking for module ~S~%" module-name))
  (let* ((name (etypecase module-name
                 (string module-name)
                 (symbol (symbol-name module-name))))
         (nlist (if (find #\. name)
                    (loop with len = (length name)
                          for c upfrom 0
                          for opos = 0 then (+ pos 1)
                          for pos = (or (and (<= opos len)
                                             (position #\. name :start opos))
                                        len)
                          until (> opos len)
                          collect (subseq name opos pos) into results
                          finally (return results))
                  (list name)))
         (ndir (butlast nlist))
         (nname (first (last nlist))))
    (when (null nname)
      (error "Didn't get a name from ~A" module-name))
    (labels ((probe (pathlist from)
               ;; Find something that looks good to load in PATH
               (loop for path in pathlist
                     for found = 
                     (progn 
                       (when verbose
                         (format *standard-output*
                                 "~&Probing ~A~@[~% as     ~A~]~% from ~:W~%"
                                 path 
                                 (and (typep path 'logical-pathname)
                                      (translate-logical-pathname path))
                                 from))
                       (let* ((p (probe-file path))
                              (pt (and p (file-write-date p)))
                              (cp (probe-file (compile-file-pathname path)))
                              (cpt (and cp (file-write-date cp))))
                         (cond ((and p cp)
                                (let ((got
                                       (if (> cpt pt)
                                           cp
                                         (progn
                                           (warn 
                                            "Loader source ~A is newer than bin ~A, loading source"
                                            p cp)
                                           p))))
                                  (when verbose
                                    (format *standard-output*
                                            "~&Found ~A~%" got))
                                  got))
                               (p 
                                (when verbose
                                  (format *standard-output*
                                          "~&Found ~A~%" p))
                                p)
                               (cp
                                (when verbose
                                  (format *standard-output*
                                          "~&Found ~A~%" cp))
                                cp)
                               (t nil))))
                     when found return found
                     finally (return nil)))
             (merge-path (wild-path dirlist name)
               ;; Do the work of merging DIRLIST and NAME into
               ;; WILD-PATH Returns a list of merges (note!).  This
               ;; implements part of the search order: for foo.bar.zap
               ;; and .../modules/*-loader return
               ;; .../modules/foo/bar/zap/zap-loader,
               ;; .../modules/foo/bar/zap-loader; for foo.bar.zap and
               ;; ../modules/loader return .../modules/foo/bar/zap/loader
               ;; only.
               (if (wild-pathname-p wild-path)
                   (let ((results
                          (list (translate-pathname 
                                 name "*" 
                                 (merge-pathnames
                                  (make-pathname 
                                   :host (pathname-host wild-path)
                                   :directory `(:relative ,@dirlist ,name))
                                  wild-path))
                                (translate-pathname
                                 name "*"
                                 (merge-pathnames
                                  (make-pathname 
                                   :host (pathname-host wild-path)
                                   :directory `(:relative ,@dirlist))
                                  wild-path)))))
                     (unless (notany #'wild-pathname-p results)
                       (error 
                        "One of the paths ~{~A~^, ~} is still wild after merging"
                        results))
                     results)
                 (let ((results
                        (list (merge-pathnames
                               (make-pathname
                                :host (pathname-host wild-path)
                                :directory `(:relative ,@dirlist ,name))
                               wild-path))))
                   (when (wild-pathname-p (first results))
                     (error 
                      "Path ~A is still wild after merging" (first results)))
                   results))))
      ;; First look for the whole path along all the elements
      (loop for d in *module-path-descriptions*
            for path = (apply #'make-pathname d)
            for found =
            (typecase path
              (logical-pathname
               ;; consider only upcase variants
               (probe (merge-path
                       path
                       (mapcar #'string-upcase ndir)
                       (string-upcase nname))
                      d))
              (pathname
               ;; Consider: thing, thing downcased, thing upcased
               ;; (yes, this is just a hardwired order)
               (or (probe (merge-path path ndir nname) d)
                   (probe (merge-path
                           path
                           (mapcar #'string-downcase ndir)
                           (string-downcase nname))
                          d)
                   (probe (merge-path
                           path
                           (mapcar #'string-upcase ndir)
                           (string-upcase nname))
                          d))))
            when found do (return-from locate-module found))
      ;; Now search ignoring the dir components
      (loop for d in *module-path-descriptions*
            for path = (apply #'make-pathname d)
            for found =
            (typecase path
              (logical-pathname
               ;; consider only upcase variants
               (probe (merge-path path '() (string-upcase nname)) d))
              (pathname
               ;; Consider: thing, thing downcased, thing upcased
               ;; (yes, this is just a hardwired order)
               (or (probe (merge-path path '() nname) d)
                   (probe (merge-path
                           path '() (string-downcase nname))
                          d)
                   (probe (merge-path
                           path '() (string-upcase nname))
                          d))))
            when found do (return-from locate-module found))
      ;; Or give up
      nil)))


(defun call/forgetting-systems (function)
  ;; Call FUNCTION, and afterwards blow away any systems it defines
  (declare (dynamic-extent function))
  (let ((systems (scm:all-systems)))
    (unwind-protect
        (funcall function)
      (dolist (new (set-difference (scm:all-systems) systems
                                   :key #'scm:module-name))
        (lw:delete-system new)))))
    
(defmacro forgetting-systems (&body forms)
  `(let ((.fn. #'(lambda () ,@forms)))
     (declare (dynamic-extent .fn.))
     (call/forgetting-systems .fn.)))

(defun require-module (m &key (verbose nil) (forget-systems t)
                         (test #'string=)
                         (pretend nil)
                         (error t))
  (if (member (etypecase m
                (string m)
                (symbol (symbol-name m)))
              *modules*
              :test test)
      (values m nil)
    (let ((location (locate-module m :verbose verbose)))
      (unless location
        (if error
            (error "No location found for ~S" m)
          (return-from require-module (values nil nil))))
      (if (not pretend)
          (progn
            (when verbose
              (format *standard-output* "~&Loading ~S from ~A"
                      m location))
            (if forget-systems
                (forgetting-systems
                  (require m location))
              (require m location)))
        (format *standard-output* "~&Would load ~S from ~A" m location))
      (values m t))))

(defun require-module-command (cmd &rest args)
  (declare (ignore cmd))
  (destructuring-bind (module/s . kws) args
    (if (consp module/s)
        (loop for m in module/s
              collect (apply #'require-module m kws))
      (apply #'require-module module/s kws))))

(declare-extra-lw-commands
  (:require require-module-command
   "Require modules, doing some fancy searching
         :VERBOSE T means be verbose, :FORGET-SYSTEMS NIL means
         do not forget systems defined when loading.
         :PRETEND T just tells you what it would have done"))


;;;; Directory
;;;

;;; I don't think this makes sense on a per-thread basis.
;;;
(defvar *directory-stack* '())

(defun directory-command (cmd &optional arg)
  (flet ((show-dirstack (&optional (return nil))
           (cond (return
                  (values (get-working-directory) *directory-stack*))
                 (t
                  (format t "~&~A~@[ ~{~<~% ~:;~A~>~^ ~}~]~%" 
                          (get-working-directory) *directory-stack*)
                  (values)))))
    (case cmd
      ((:cd)
       (if arg (change-directory arg) (cd))
       (values))
      ((:pwd)
       (format t "~&~A~%" (get-working-directory))
       (values))
      ((:pushd)
       (cond (arg
              (let ((old (get-working-directory)))
                (change-directory arg)
                (push old *directory-stack*)
                (show-dirstack)))
             (*directory-stack*
              (let ((here (get-working-directory)))
                (rotatef here (first *directory-stack*))
                (change-directory here)
                (show-dirstack)))
             (t
              (warn "Empty directory stack")
              (values))))
      ((:popd)
       (when arg
         (warn "Ignoring argument"))
       (if *directory-stack*
           (cd (pop *directory-stack*))
         (warn "Empty directory stack"))
       (show-dirstack))
      ((:dirs)
       (show-dirstack arg))
      (otherwise
       (warn "Unrecognised directory command ~S" cmd)
       (values)))))

(declare-extra-lw-commands
  (:cd directory-command
   "Change directory")
  (:pwd directory-command
   "Return the current working directory")
  (:pushd directory-command
   "Push a directory, or swap directories with no argument")
  (:popd directory-command
   "Pop a directory")
  (:dirs directory-command
   "Show the current working directory and stack.  With arfgument return them"))


;;;; Load CLC commands if needed
;;;
(eval-when (:load-toplevel)
  (when (member :org.tfeb.clc.server *features*)
    (load (merge-pathnames #P"lw-commands-clc"
                           *load-pathname*))))