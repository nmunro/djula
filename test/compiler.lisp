(in-package #:djula-test)

(in-suite djula-test)

(def-test compiler (:compile-at :definition-time)
  (let ((fn (compile-string
             "{% ifequal day \"Thursday\" %}
I never could get the hang of {{ day }}s.
{% endifequal %}")))
    (is (string= "
I never could get the hang of Thursdays.
"
                 (render-template* fn nil :day "Thursday")))))

(def-test template-cache-hit-test (:compile-at :definition-time)
  (djula:add-template-directory (asdf:system-relative-pathname :djula "test/templates/"))
  (clear-template-cache)
  (let* ((t1 (compile-template* "t1.djula"))
         (t2 (compile-template* "t1.djula"))
         (path (merge-pathnames "t1.djula" (asdf:system-relative-pathname :djula "test/templates/")))
         (t3 (compile-template* path)))
    (is (not (null t1)))
    (is (eq t1 t2))
    (is (eq t1 t3))))

(def-test clear-template-cache-test (:compile-at :definition-time)
  (djula:add-template-directory (asdf:system-relative-pathname :djula "test/templates/"))
  (clear-template-cache)
  (let ((t1 (compile-template* "t1.djula")))
    (clear-template-cache)
    (let ((t2 (compile-template* "t1.djula")))
      (is (not (null t1)))
      (is (not (null t2)))
      (is (not (eq t1 t2))))))

(def-test precompile-templates-test (:compile-at :definition-time)
  (clear-template-cache)
  (let ((count (precompile-templates (asdf:system-relative-pathname :djula "test/templates/"))))
    (is (> count 0))
    (is (not (null (gethash "t1.djula" (djula:template-cache djula:*current-compiler*)))))))

(def-test template-hot-reload-test (:compile-at :definition-time)
  (let* ((temp-dir (merge-pathnames "djula-test-temp/" (uiop:temporary-directory)))
         (template-file (merge-pathnames "reload-test.html" temp-dir)))
    (ensure-directories-exist temp-dir)
    (djula:add-template-directory temp-dir)
    (unwind-protect
         (let ((*recompile-templates-on-change* t))
           ;; Version 1
           (alexandria:write-string-into-file "Version 1: {{ val }}" template-file :if-exists :supersede)
           (clear-template-cache)
           (let ((tpl (compile-template* "reload-test.html")))
             (is (string= "Version 1: hello" (render-template* tpl nil :val "hello")))
             (is (string= "Version 1: hello" (render-template* "reload-test.html" nil :val "hello")))

             ;; Wait briefly for file-write-date resolution
             (sleep 1.1)

             ;; Version 2
             (alexandria:write-string-into-file "Version 2: {{ val }}" template-file :if-exists :supersede)
             (is (string= "Version 2: hello" (render-template* tpl nil :val "hello")))
             (is (string= "Version 2: hello" (render-template* "reload-test.html" nil :val "hello")))

             ;; Test that when *recompile-templates-on-change* is nil, changes are ignored
             (let ((*recompile-templates-on-change* nil))
               (sleep 1.1)
               (alexandria:write-string-into-file "Version 3: {{ val }}" template-file :if-exists :supersede)
               (is (string= "Version 2: hello" (render-template* tpl nil :val "hello")))
               (is (string= "Version 2: hello" (render-template* "reload-test.html" nil :val "hello"))))))
      (uiop:delete-directory-tree temp-dir :validate t :if-does-not-exist :ignore))))

(def-test concurrent-rendering-test (:compile-at :definition-time)
  (djula:add-template-directory (asdf:system-relative-pathname :djula "test/templates/"))
  (clear-template-cache)
  (let* ((threads nil)
         (errors nil)
         (error-lock (bordeaux-threads:make-lock "error-lock")))
    (dotimes (i 8)
      (push (bordeaux-threads:make-thread
             (lambda ()
               (handler-case
                   (dotimes (j 20)
                     (render-template* "t1.djula" nil)
                     (render-template* "t2.djula" nil)
                     (render-template* "t3.djula" nil))
                 (error (e)
                   (bordeaux-threads:with-lock-held (error-lock)
                     (push e errors))))))
            threads))
    (dolist (th threads)
      (bordeaux-threads:join-thread th))
    (is (null errors))))
