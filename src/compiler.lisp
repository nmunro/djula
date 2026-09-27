(in-package #:djula)

(defvar *compiler-lock* (bordeaux-threads:make-recursive-lock "djula-compiler-lock")
  "Recursive lock protecting template compilation and cache updates.")

(defgeneric compile-template (compiler name &optional error-p)
  (:documentation "Provides a hook to customize template compilation."))

(defclass compiler ()
  ((template-cache
    :accessor template-cache
    :initarg :template-cache
    :initform (make-hash-table :test 'equal)
    :documentation "A hash-table cache mapping template names/pathnames to compiled-template instances."))
  (:documentation "Abstract class. Top-level class for template compilers."))

(defclass compiled-template ()
  ((compiled-template :initarg :compiled-template
                      :initform nil
                      :accessor compiled-template
                      :documentation "The compiled template (a closure)")
   (linked-templates :initarg :linked-templates
                     :accessor linked-templates
                     :initform '()
                     :documentation "Extends for Include files.")
   (template-file :initarg :template-file
                  :accessor template-file
                  :initform (error "Provide the template file")
                  :documentation "The filepath of the template")
   (template-file-write-date :accessor template-file-write-date
                             :documentation "The write date of the template file"))
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation "A compiled template"))

(defmethod print-object ((compiled-template compiled-template) stream)
  (print-unreadable-object (compiled-template stream :type t :identity t)
    (format stream "~A" (template-file compiled-template))))

(defmethod compile-template-file ((compiled-template compiled-template))
  (bordeaux-threads:with-recursive-lock-held (*compiler-lock*)
    ;; Set the template file write date
    (setf (template-file-write-date compiled-template)
          (file-write-date (template-file compiled-template)))

    ;; Compile the template file
    (let ((*block-alist* nil)
          (*linked-templates* nil))
      (let ((compiled-str
              (compile-string (fetch-template* (template-file compiled-template)))))
        (setf (compiled-template compiled-template) compiled-str
              (linked-templates compiled-template) *linked-templates*)))))

(defmethod template-changed ((compiled-template compiled-template))
  (let ((file (template-file compiled-template)))
    (or (not (uiop:file-exists-p file))
        (not (equalp (template-file-write-date compiled-template)
                     (file-write-date file))))))

(defmethod initialize-instance :after ((compiled-template compiled-template) &rest initargs)
  (declare (ignore initargs))

  (compile-template-file compiled-template)

  (closer-mop:set-funcallable-instance-function
   compiled-template
   (if *recompile-templates-on-change*
       (lambda (stream)
         ;; Recompile the template if *recompile-templates-on-change* is enabled and files changed
         (when *recompile-templates-on-change*
           (let ((template-file-write-date (template-file-write-date compiled-template)))
             (when (or (not (equalp (file-write-date (template-file compiled-template))
                                    template-file-write-date))
                       (loop for linked-template in (linked-templates compiled-template)
                               thereis (template-changed linked-template)))
               (bordeaux-threads:with-recursive-lock-held (*compiler-lock*)
                 (let ((template-file-write-date (template-file-write-date compiled-template)))
                   (when (or (not (equalp (file-write-date (template-file compiled-template))
                                          template-file-write-date))
                             (loop for linked-template in (linked-templates compiled-template)
                                     thereis (template-changed linked-template)))
                     (compile-template-file compiled-template)))))))
         (funcall (compiled-template compiled-template) stream))
       ;; else, avoid the automatic template file checking and recompilation
       (compiled-template compiled-template))))

(defmethod compile-template ((compiler compiler) name &optional (error-p t))
  (bordeaux-threads:with-recursive-lock-held (*compiler-lock*)
    (let ((cache (template-cache compiler)))
      ;; 1. Check cache by name (or string/pathname alternative)
      (let ((cached (or (gethash name cache)
                        (and (pathnamep name) (gethash (namestring name) cache))
                        (and (stringp name) (gethash (pathname name) cache)))))
        (when cached
          (when *recompile-templates-on-change*
            (when (or (template-changed cached)
                      (loop for linked-template in (linked-templates cached)
                              thereis (template-changed linked-template)))
              (compile-template-file cached)))
          (return-from compile-template cached)))

      ;; 2. Not in cache by name; resolve file
      (when-let ((template-file (find-template* name error-p)))
        ;; Check if template-file itself is in cache
        (let ((cached (or (gethash template-file cache)
                          (gethash (namestring template-file) cache))))
          (if cached
              (progn
                (setf (gethash name cache) cached)
                (when *recompile-templates-on-change*
                  (when (or (template-changed cached)
                            (loop for linked-template in (linked-templates cached)
                                    thereis (template-changed linked-template)))
                    (compile-template-file cached)))
                cached)
              (let ((compiled (make-instance 'compiled-template
                                             :template-file template-file)))
                (setf (gethash name cache) compiled)
                (setf (gethash template-file cache) compiled)
                (setf (gethash (namestring template-file) cache) compiled)
                compiled)))))))

(defclass toplevel-compiler (compiler)
  ((fragment-compiler
    :reader fragment-compiler
    :initarg :fragment-compiler))
  (:documentation "The default Djula template compiler."))

(defmethod initialize-instance :after ((compiler toplevel-compiler) &rest initargs)
  (declare (ignore initargs))
  (unless (slot-boundp compiler 'fragment-compiler)
    (setf (slot-value compiler 'fragment-compiler)
          (make-instance 'compiler :template-cache (template-cache compiler)))))

(defvar *current-compiler* (make-instance 'toplevel-compiler)
  "The current template compiler.")

(defmethod compile-template ((compiler toplevel-compiler) name &optional (error-p t))
  (let ((*block-alist* nil)
        (*linked-templates* nil))
    (let ((*current-compiler* (fragment-compiler compiler)))
      (compile-template *current-compiler* name error-p))))

(defun compile-template* (name)
  "Compiles template NAME with compiler in *CURRENT-COMPILER*."
  (compile-template *current-compiler* name))

(defun clear-template-cache (&optional (compiler *current-compiler*))
  "Clears the template cache of COMPILER."
  (bordeaux-threads:with-recursive-lock-held (*compiler-lock*)
    (clrhash (template-cache compiler))
    (when (and (typep compiler 'toplevel-compiler)
               (slot-boundp compiler 'fragment-compiler))
      (clrhash (template-cache (fragment-compiler compiler))))))

(defun %collect-template-files (dir extensions)
  (let ((dir (uiop:ensure-directory-pathname dir))
        (result nil))
    (when (uiop:directory-exists-p dir)
      (labels ((walk (current-dir)
                 (dolist (subdir (uiop:subdirectories current-dir))
                   (walk subdir))
                 (dolist (file (uiop:directory-files current-dir))
                   (let ((name (file-namestring file))
                         (type (pathname-type file)))
                     (unless (or (alexandria:emptyp name)
                                 (char= (char name 0) #\.))
                       (when (or (null extensions)
                                 (and type (member type extensions :test #'string-equal)))
                         (push file result)))))))
        (walk dir)))
    (nreverse result)))

(defun precompile-templates (&rest args)
  "Precompiles templates in a store or directory.
Can be called with positional or keyword arguments:
  (precompile-templates)
  (precompile-templates directory &key compiler extensions error-p)
  (precompile-templates store &key compiler extensions error-p)
  (precompile-templates &key (store-or-directory *current-store*) (compiler *current-compiler*) extensions error-p)
Returns the number of precompiled templates."
  (let ((store-or-dir *current-store*)
        (compiler *current-compiler*)
        (extensions nil)
        (error-p nil))
    (let ((rest-args args))
      (when (and rest-args (not (keywordp (first rest-args))))
        (setf store-or-dir (pop rest-args)))
      (loop for (key val) on rest-args by #'cddr do
        (case key
          ((:store-or-directory :template-store-or-directory) (setf store-or-dir val))
          (:compiler (setf compiler val))
          (:extensions (setf extensions val))
          (:error-p (setf error-p val)))))
    (let ((dirs (cond
                  ((typep store-or-dir 'filesystem-template-store)
                   (search-path store-or-dir))
                  ((listp store-or-dir)
                   store-or-dir)
                  (t
                   (list store-or-dir))))
          (count 0))
      (dolist (dir dirs)
        (let ((dir-path (uiop:ensure-directory-pathname dir)))
          (when (uiop:directory-exists-p dir-path)
            (dolist (file (%collect-template-files dir-path extensions))
              (let ((rel (enough-namestring file dir-path)))
                (flet ((do-compile ()
                         (let ((compiled (compile-template compiler file error-p)))
                           (when compiled
                             (bordeaux-threads:with-recursive-lock-held (*compiler-lock*)
                               (setf (gethash rel (template-cache compiler)) compiled))
                             (incf count)))))
                  (if error-p
                      (do-compile)
                      (handler-case
                          (do-compile)
                        (error (e)
                          (warn "Error precompiling template ~A: ~A" rel e))))))))))
      count)))

(defun render-template* (template &optional destination &rest *template-arguments*)
  "Render TEMPLATE into DESTINATION passing *TEMPLATE-ARGUMENTS*.
DESTINATION is the same as FORMAT destination.
TEMPLATE-ARGUMENTS is a property-list. "
  (cond
    ((or (pathnamep template)
       (stringp template))
      ;; Accept strings and pathnames as template designators.
      (apply #'render-template* (compile-template* template) destination *template-arguments*))
    ((functionp template)
      (let ((*template-arguments* (append *template-arguments* *default-template-arguments*))
             (*accumulated-javascript-strings* nil)
             (*current-language* *current-language*)
             (*current-template* template))
        (handler-case
          (uiop:with-output (stream destination)
            ;; if debugging mode is enabled, append debugging information to the template output
            (when *debug-mode*
              (if *fancy-debug-p*
                (print-fancy-debugging-information stream)
                (print-debugging-information stream)))
            ;; render the template
            (funcall template stream))
          (error (e)
            ;; template rendering error handling
            (if *catch-template-errors-p*
              (if *fancy-error-template-p*
                ;; disable error templates when rendering error templates
                ;; to prevent infinite recursion
                (let ((*fancy-error-template-p* nil))
                  (render-error-template e destination
                    :backtrace (with-output-to-string (s)
                                 (trivial-backtrace:print-backtrace-to-stream s))
                    :template template
                    :context (list :arguments *template-arguments*
                               :language *current-language*
                               :template-package *template-package*)))
                ;; else, print the error plainly
                (uiop:with-output (stream destination)
                  (princ e stream)))
              ;; if no catch-template-errors, then resignal the error
              (error e))))))
    (t (error 'simple-error
         :format-control "~A is not a valid template"
         :format-arguments (list template)))))

(defun compile-string (string)
  "Compile the template in STRING.
Returns a funcallable template."
  (let ((fs (mapcar #'compile-token (process-tokens (parse-template-string string)))))
    (lambda (stream)
      (dolist (f fs)
        (funcall f stream)))))

(defun compile-token (token)
  "Compiles TOKEN.
The compilation of a TOKEN creates a LAMBDA that given a stream renders the token."
  ;; Note that given that the compilation of a token is also in charge
  ;; of determining how to render the token, there are both error handlers for
  ;; compile-time and also run-time here.
  (destructuring-bind (name . args) token
    (let ((compiler (find-token-compiler name)))
      (if (null compiler)
          (lambda (stream)
            (princ (template-error-string "unknown token ~A" name) stream))
          (handler-case
              ;; Handle compile-time errors.
              (let ((f (apply compiler args)))
                (assert (functionp f)
                        nil
                        "Compiling the token ~A did not return a function"
                        name)
                (lambda (stream)
                  ;; This is the rendering lambda, so handle run-time errors here.
                  (handler-case
                      (funcall f stream)
                    (template-error (e1)
                      (if (and *catch-template-errors-p*
                               (not *fancy-error-template-p*))
                          (princ e1 stream)
                          ;; If fancy-errors are enabled, then resignal the condition. Fancy errors are handled higher in the condition handlers, in RENDER-TEMPLATE*.
                          (error e1)))
                    (error (e2)
                      (let ((msg (template-error-string* e2 "rendering the token ~A" (write-to-string token :length 10))))
                        (if (and *catch-template-errors-p*
                                 (not *fancy-error-template-p*))
                            (princ msg stream)
                            (template-error msg)))))))
            (template-error (e1)
              (if (and *catch-template-errors-p*
                       (not *fancy-error-template-p*))
                  (lambda (stream)
                    (princ e1 stream))
                  (error e1)))
            (error (e2)
              (let ((msg (template-error-string* e2 "compiling the token ~A" token)))
                (if (and *catch-template-errors-p*
                         (not *fancy-error-template-p*))
                    (lambda (stream)
                      (princ msg stream))
                    (template-error msg)))))))))

(def-token-compiler :string (string)
  ":STRING tokens compile into a function that simply returns the string"
  (lambda (stream)
    (write-string string stream)))
