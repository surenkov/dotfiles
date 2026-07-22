;;; custom-gptel-tools-test.el --- Tests for custom gptel tools  -*- lexical-binding: t; -*-
;;
;; To run these tests in batch mode, execute the following command:
;;   emacs -batch -l doom/my/custom-gptel-tools-test.el -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cl-lib)

;; Create mock gptel environment so we can load the files standalone
(provide 'gptel)

;; Mock gptel variables
(defvar gptel-model nil)
(defvar gptel--fsm-last nil)
(defvar gptel-send--transitions nil)
(defvar gptel-tools nil)
(defvar gptel-confirm-tool-calls nil)
(defvar gptel-backend nil)
(defvar gptel-mode nil)
(defvar gptel-use-header-line nil)
(defvar gptel--header-line-info nil)
(defvar gptel--tool-preview-alist nil)
(defvar gptel-tool-call-actions-map (make-sparse-keymap))

;; Mock gptel functions
(defun gptel--model-name (&rest _args) "mock-model")
(defun gptel--display-tool-calls (&rest _args))
(defun gptel--transform-add-context (&rest _args))
(defun gptel-make-fsm (&rest _args))
(defun gptel-request (&rest _args))
(defun gptel-with-preset (&rest _args))
(defun gptel--update-status (&rest _args))
(defun gptel--format-tool-call (&rest _args) "mock-tool-call")
(defun gptel-fsm-info (&rest _args) '(:position 1))
(defun gptel--strip-mode-suffix (mode-sym)
  "Mock of `gptel--strip-mode-suffix` for testing."
  (let ((name (symbol-name mode-sym)))
    (if (string-match "\\(.*\\)-mode$" name)
        (match-string 1 name)
      name)))

(defun gptel--handle-tool-result (&rest _args))
(defun gptel--handle-post-tool (&rest _args))
(defun gptel--handle-tool-use (&rest _args))
(defun gptel--fsm-transition (&rest _args))
(defun gptel--handle-pre-tool (&rest _args))
(defun gptel--handle-wait (&rest _args))
(defun gptel-tool-name (&rest _args) "mock-tool")

;; Structure definition and mocks for gptel-tool
(cl-defstruct gptel-tool args)
(defun gptel-get-tool (&rest _args) (make-gptel-tool))

;; Mock doom-project-root and make-temp-file/with-temp-file dependencies if needed
(unless (fboundp 'doom-project-root)
  (defun doom-project-root () default-directory))

;; Mock other doom functions/vars if any
(unless (fboundp 'doom-user-dir)
  (defun doom-user-dir () default-directory))

(unless (fboundp 'doom-project-name)
  (defun doom-project-name () "mock-project"))

;; Mock flymake/flycheck variables/functions to avoid compilation warnings
(defvar flycheck-mode nil)
(defvar flycheck-current-errors nil)
(defvar flymake-mode nil)
(defun flymake-diagnostics (&rest _args) nil)

;; Mock shr-render-region
(unless (fboundp 'shr-render-region)
  (defun shr-render-region (&rest _args) nil))

(add-to-list 'load-path (expand-file-name "doom/my/"))

;; Load the tools
(load-file (expand-file-name "doom/my/custom-gptel-tools.el"))
(load-file (expand-file-name "doom/my/custom-gptel-ui.el"))

(ert-deftest test-truncate-output-under-limits ()
  "Verify that text under both line and byte limits is not truncated."
  (let ((text "Line 1\nLine 2\nLine 3"))
    (should (equal (my--truncate-output text 10) text))))

(ert-deftest test-truncate-output-from-buffer ()
  "Verify that my--truncate-output accepts a buffer object directly."
  (with-temp-buffer
    (insert "Line 1\nLine 2\nLine 3\nLine 4\nLine 5")
    (let ((result (my--truncate-output (current-buffer) 3)))
      (should (string-match-p "Truncated" result))
      (should (string-match-p "gptel-output-" result)))))

(ert-deftest test-truncate-output-exceeds-lines ()
  "Verify truncation and caching when lines exceed the line limit."
  (let* ((text "1\n2\n3\n4\n5\n6\n7\n8\n9\n10")
         (my/custom-gptel-tool-output-limit 1000) ;; Large enough to not trigger byte limit
         (result (my--truncate-output text 4)))
    ;; The result should contain truncation messages and a reference to the temp file
    (should (string-match-p "Truncated" result))
    (should (string-match-p "gptel-output-" result))
    (should (string-match-p "saved to temporary file" result))))

(ert-deftest test-truncate-output-exceeds-bytes ()
  "Verify truncation and caching when byte size exceeds the byte limit."
  (let* ((text "Some very long text that will exceed the custom byte limit easily.")
         (my/custom-gptel-tool-output-limit 10) ;; Extremely small to trigger byte limit
         (result (my--truncate-output text 100)))
    (should (string-match-p "Truncated" result))
    (should (string-match-p "gptel-output-" result))
    (should (string-match-p "saved to temporary file" result))))

(ert-deftest test-truncate-output-extremely-long-lines ()
  "Verify that extremely long lines are handled safely."
  (let* ((text (make-string 1000 ?A))
         (my/custom-gptel-tool-output-limit 50)
         (result (my--truncate-output text 100)))
    (should (string-match-p "Truncated" result))
    (should (string-match-p "saved to temporary file" result))
    ;; Head/tail parts should be shorter than the half limit (10 characters/bytes)
    (should (< (length result) (length text)))))

(ert-deftest test-truncate-output-temp-file-readability ()
  "Verify that the cached temp file can be read and matches the original text."
  (let* ((text "Line A\nLine B\nLine C\nLine D\nLine E\nLine F")
         (result (my--truncate-output text 2)))
    (string-match "temporary file: \\([^\n ]+\\)" result)
    (let ((temp-file (match-string 1 result)))
      (should (file-exists-p temp-file))
      (with-temp-buffer
        (insert-file-contents temp-file)
        (should (equal (buffer-string) text)))
      (delete-file temp-file))))

(ert-deftest test-truncate-output-exempt ()
  "Verify that text with a negative MAX-LINES limit is not truncated."
  (let* ((text "Line 1\nLine 2\nLine 3\nLine 4\nLine 5")
         (my/custom-gptel-tool-output-limit 2)
         (result (my--truncate-output text -1)))
    (should (equal result text))))

(ert-deftest test-gptel-prompts-buffer-mode-variables ()
  "Test that gptel-prompts-buffer-mode-variables returns correct mode variables."
  (let ((major-mode 'org-mode))
    (let ((vars (gptel-prompts-buffer-mode-variables nil)))
      (should (equal (cdr (assoc "major_mode" vars)) "org-mode"))
      (should (equal (cdr (assoc "mode_syntax" vars)) "org"))
      (should (eq (cdr (assoc "is_org_mode" vars)) t))
      (should (eq (cdr (assoc "is_markdown_mode" vars)) nil))))
  (let ((major-mode 'markdown-mode))
    (let ((vars (gptel-prompts-buffer-mode-variables nil)))
      (should (equal (cdr (assoc "major_mode" vars)) "markdown-mode"))
      (should (equal (cdr (assoc "mode_syntax" vars)) "markdown"))
      (should (eq (cdr (assoc "is_org_mode" vars)) nil))
      (should (eq (cdr (assoc "is_markdown_mode" vars)) t))))
  (let ((major-mode 'text-mode))
    (let ((vars (gptel-prompts-buffer-mode-variables nil)))
      (should (equal (cdr (assoc "major_mode" vars)) "text-mode"))
      (should (equal (cdr (assoc "mode_syntax" vars)) "text"))
      (should (eq (cdr (assoc "is_org_mode" vars)) nil))
      (should (eq (cdr (assoc "is_markdown_mode" vars)) nil)))))

(ert-deftest test-truncate-output-performance ()
  "Verify that truncating a very large string is fast and doesn't block."
  (let* ((line (make-string 1000 ?A))
         (lines (make-list 10000 line))
         (huge-text (mapconcat #'identity lines "\n"))
         (t1 (float-time))
         (result (my--truncate-output huge-text 200))
         (duration (- (float-time) t1)))
    (should (< duration 0.15))
    (should (string-match-p "Truncated" result))))

(ert-deftest test-gptel-agent-delegated-tools-inheritance ()
  "Verify that sub-agent correctly inherits tools, model, and backend from parent FSM info."
  (let* ((global-gptel-tools nil) ; global is empty
         (parent-tools '("fd" "rg" "cat"))
         (mock-fsm-info (list :position 1
                              :tools parent-tools
                              :model "parent-model-name"
                              :backend "parent-backend-obj"))
         (captured-preset-spec nil)
         (callback-called nil))
    (cl-letf* (((symbol-value 'gptel-tools) global-gptel-tools)
               ((symbol-function 'gptel-fsm-info) (lambda (_fsm) mock-fsm-info))
               ((symbol-function 'gptel-tool-name) (lambda (t-spec) t-spec))
               ((symbol-function 'gptel-with-preset) (lambda (spec &rest _body)
                                                       (setq captured-preset-spec spec)))
               (main-cb (lambda (result)
                          (setq callback-called t)
                          (when (stringp result)
                            (error "Validation failed: %s" result)))))
      (my/gptel-agent--task main-cb "Test task" "Test prompt" '("fd" "rg"))
      (should (not callback-called))
      (should captured-preset-spec)
      (should (equal (plist-get captured-preset-spec :tools) '("fd" "rg")))
      (should (equal (plist-get captured-preset-spec :model) "parent-model-name"))
      (should (equal (plist-get captured-preset-spec :backend) "parent-backend-obj")))))

(ert-deftest test-ast-grep-tool-args ()
  "Verify that my/ast-grep-tool constructs command-line arguments correctly."
  (let ((captured-command nil))
    (cl-letf* (((symbol-function 'executable-find) (lambda (bin) (if (equal bin "ast-grep") "/usr/local/bin/ast-grep" nil)))
               ((symbol-function 'my/get-resolved-skill-dirs) (lambda () nil))
               ((symbol-function 'my--async-exec)
                (lambda (_name command callback &rest _args)
                  (setq captured-command command)
                  (funcall callback 0 "success-output"))))
      
      ;; 1. Run 'run' command
      (my/ast-grep-tool (lambda (out) (should (equal out "success-output")))
                        "run" "." "pattern-to-match" "typescript")
      ;; Captured command contains ast-grep arguments
      (should (member "ast-grep" captured-command))
      (should (member "run" captured-command))
      (should (member "--pattern" captured-command))
      (should (member "pattern-to-match" captured-command))
      (should (member "--lang" captured-command))
      (should (member "typescript" captured-command))
      
      ;; 2. Run 'scan' command
      (my/ast-grep-tool (lambda (out) (should (equal out "success-output")))
                        "scan" "." nil nil "inline-yaml-rules")
      (should (member "scan" captured-command))
      (should (member "--inline-rules" captured-command))
      (should (member "inline-yaml-rules" captured-command))

      ;; 3. Run 'outline' command
      (my/ast-grep-tool (lambda (out) (should (equal out "success-output")))
                        "outline" "." "outline-filter")
      (should (member "outline" captured-command))
      (should (member "--match" captured-command))
      (should (member "outline-filter" captured-command)))))

(ert-deftest test-gptel-agent-dynamic-schema-restriction ()
  (let* ((mock-agent-tool (make-gptel-tool))
         (gptel-tools '("fd" "rg" "agent")))
    (cl-letf* (((symbol-function 'gptel-get-tool) (lambda (name) (if (or (equal name "agent") (equal name '("agent" "agent"))) mock-agent-tool)))
               ((symbol-function 'gptel-tool-name) (lambda (t-spec) t-spec)))
      (setf (gptel-tool-args mock-agent-tool)
            '((:name "description" :type string)
              (:name "prompt" :type string)
              (:name "tools" :type array :items (:type string))))
      (my/gptel-agent-update-tool-schema)
      (let* ((args (gptel-tool-args mock-agent-tool))
             (tools-arg (cl-find "tools" args :key (lambda (arg) (plist-get arg :name)) :test #'equal))
             (items (plist-get tools-arg :items)))
        (should tools-arg)
        (should (member (plist-get items :type) '("string" string)))
        (should (equal (plist-get items :enum) ["fd" "rg"]))))))

(ert-deftest test-gptel-tools-order-invariant ()
  "Ensure `gptel-tools' order is strictly preserved to maintain model-side context caching."
  (let* ((mock-agent-tool (make-gptel-tool))
         (mock-skill-tool (make-gptel-tool))
         (original-tools (list "tool1" "tool2" "agent" "tool3"))
         (gptel-tools original-tools))
    (cl-letf* (((symbol-function 'gptel-get-tool) (lambda (name)
                                                    (cond
                                                     ((or (equal name "agent") (equal name '("agent" "agent"))) mock-agent-tool)
                                                     ((equal name "skill") mock-skill-tool)
                                                     (t nil))))
               ((symbol-function 'gptel-tool-name) (lambda (t-spec) t-spec))
               (original-copy (copy-sequence gptel-tools)))
      (my/gptel-agent-update-tool-schema)
      (my/gptel-skills-update-tool-schema-internal)
      (should (eq gptel-tools original-tools))
      (should (equal gptel-tools original-copy)))))

(ert-deftest test-gptel-agent-schema-updates-correctly-with-category-clash ()
  "Verify that update-tool-schema successfully resolves the agent tool when its name is also a category."
  (let* ((mock-agent-tool (make-gptel-tool))
         (gptel-tools '("fd" "rg" "agent"))
         (called-with-path nil))
    (cl-letf* (((symbol-function 'gptel-get-tool)
                (lambda (path)
                  (setq called-with-path path)
                  (cond
                   ;; If called with string "agent", simulate the bug by returning a list of tools
                   ((equal path "agent")
                    (list (make-gptel-tool) mock-agent-tool (make-gptel-tool)))
                   ;; If called with list, return the correct single tool
                   ((equal path '("agent" "agent"))
                    mock-agent-tool)
                   (t nil))))
               ((symbol-function 'gptel-tool-name) (lambda (t-spec) t-spec)))
      (setf (gptel-tool-args mock-agent-tool)
            '((:name "description" :type string)
              (:name "prompt" :type string)
              (:name "tools" :type array :items (:type string))))
      (my/gptel-agent-update-tool-schema)
      (should (equal called-with-path '("agent" "agent")))
      (let* ((args (gptel-tool-args mock-agent-tool))
             (tools-arg (cl-find "tools" args :key (lambda (arg) (plist-get arg :name)) :test #'equal))
             (items (plist-get tools-arg :items)))
        (should tools-arg)
        (should (equal (plist-get items :enum) ["fd" "rg"]))))))

(ert-deftest test-sandbox-cache ()
  "Verify that sandbox wrapping utilize cache and clear correctly."
  (my/custom-gptel-clear-caches)
  (let ((root (doom-project-root)))
    (let ((wrapped (my--wrap-sandbox-command '("ls"))))
      (should (member "/usr/bin/sandbox-exec" wrapped))
      (should (cl-some (lambda (arg) (string-prefix-p "TARGET_DIR=" arg)) wrapped)))
    ;; Test cache invalidation
    (my/custom-gptel-clear-caches)
    (should (zerop (hash-table-count my/custom-gptel--path-cache)))
    ;; Test truename caching
    (let ((home-truename (my/custom-gptel--get-truename "~")))
      (should (stringp home-truename))
      (should (> (hash-table-count my/custom-gptel--path-cache) 0))
      (should (equal (gethash "~" my/custom-gptel--path-cache) home-truename)))
    (my/custom-gptel-clear-caches)
    (should (zerop (hash-table-count my/custom-gptel--path-cache)))))

(defun my-test--cat-file-tool (path &optional offset limit)
  "Synchronous test helper for `my/cat-file-tool`."
  (let ((result nil)
        (done nil))
    (my/cat-file-tool (lambda (res)
                        (setq result res)
                        (setq done t))
                      path offset limit)
    (with-timeout (5.0 (error "Timed out waiting for my/cat-file-tool callback"))
      (while (not done)
        (accept-process-output nil 0.05)))
    result))

(ert-deftest test-cat-file-tool-formatting ()
  "Test my/cat-file-tool formatting with offset and limits."
  (let ((my/custom-gptel-sandbox-profile-path nil)
        (temp-file (make-temp-file "gptel-output-test-cat-")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (dotimes (i 100)
              (insert (format "Line content %d\n" (1+ i)))))
          ;; 1. Read first 5 lines
          (let ((res (my-test--cat-file-tool temp-file 1 5)))
            (should (string-match-p "^1:Line content 1" res))
            (should (string-match-p "5:Line content 5" res))
            (should (string-match-p "Showing lines 1-5" res)))
          ;; 2. Read offset 50, limit 5
          (let ((res (my-test--cat-file-tool temp-file 50 5)))
            (should (string-match-p "50:Line content 50" res))
            (should (string-match-p "54:Line content 54" res)))
          ;; 3. Offset beyond file length
          (let ((res (my-test--cat-file-tool temp-file 200 5)))
            (should (string-match-p "Offset 200 is beyond file length" res))))
      (delete-file temp-file))))


(ert-deftest test-cat-file-tool-large-file-chunking ()
  "Test my/cat-file-tool on large files (>256KB) to verify chunked reading."
  (let ((my/custom-gptel-sandbox-profile-path nil)
        (temp-file (make-temp-file "gptel-output-test-cat-large-")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (dotimes (i 10000)
              (insert (format "Line content %05d with extra padding padding padding\n" (1+ i)))))
          ;; Verify file size is > 256KB
          (should (> (file-attribute-size (file-attributes temp-file)) (* 256 1024)))
          ;; 1. Read early chunk (lines 1 to 10)
          (let ((res (my-test--cat-file-tool temp-file 1 10)))
            (should (string-match-p "^1:Line content 00001" res))
            (should (string-match-p "10:Line content 00010" res))
            (should (string-match-p "Showing lines 1-10" res)))
          ;; 2. Read middle chunk with offset (lines 5000 to 5005)
          (let ((res (my-test--cat-file-tool temp-file 5000 5)))
            (should (string-match-p "5000:Line content 05000" res))
            (should (string-match-p "5004:Line content 05004" res))
            (should (string-match-p "Showing lines 5000-5004" res))))
      (delete-file temp-file))))

(ert-deftest test-get-visiting-buffers ()
  "Test my--get-visiting-buffers detects open buffers and handles nonexistent/invalid files safely."
  (let* ((temp-file (make-temp-file "test-visiting-"))
         (buf (find-file-noselect temp-file)))
    (unwind-protect
        (progn
          (should (member buf (my--get-visiting-buffers (list temp-file))))
          (should-not (my--get-visiting-buffers (list "/nonexistent/file.txt")))
          (should-not (my--get-visiting-buffers (list "" nil)))
          (let ((link (expand-file-name "loop-link" (file-name-directory temp-file))))
            (make-symbolic-link link link)
            (unwind-protect
                (should-not (my--get-visiting-buffers (list link)))
              (ignore-errors (delete-file link)))))
      (when (buffer-live-p buf) (kill-buffer buf))
      (delete-file temp-file))))

(ert-deftest test-edit-tool-fast-path ()
  "Test my/edit-tool skips timer delay when no files are open in visiting buffers."
  (let ((callback-called nil)
        (result-out nil)
        (patch "--- a/nonexistent.txt\n+++ b/nonexistent.txt\n@@ -0,0 +1 @@\n+test\n"))
    (cl-letf* (((symbol-function 'my--async-exec)
                (lambda (_name _command callback &rest _args)
                  (funcall callback 0 "Patch applied successfully")
                  nil)))
      ;; 1. Fast path: no visiting buffers for nonexistent.txt
      (my/edit-tool (lambda (out)
                      (setq callback-called t)
                      (setq result-out out))
                    patch)
      (should callback-called)
      (should (equal result-out "Patch applied successfully"))))

  ;; 2. Delayed path: visiting buffer exists
  (let* ((temp-file (make-temp-file "test-edit-tool-"))
         (buf (find-file-noselect temp-file))
         (rel-file (file-relative-name temp-file (doom-project-root)))
         (patch (format "--- a/%s\n+++ b/%s\n@@ -0,0 +1 @@\n+test\n" rel-file rel-file))
         (timer-scheduled nil)
         (callback-called nil)
         (result-out nil))
    (unwind-protect
        (cl-letf* (((symbol-function 'my--async-exec)
                    (lambda (_name _command callback &rest _args)
                      (funcall callback 0 "Patch applied successfully")
                      nil))
                   ((symbol-function 'run-with-timer)
                    (lambda (delay _repeat fn &rest _args)
                      (setq timer-scheduled t)
                      (should (= delay my/custom-gptel-edit-diagnostic-delay))
                      (funcall fn)
                      nil)))
          (my/edit-tool (lambda (out)
                          (setq callback-called t)
                          (setq result-out out))
                        patch)
          (should timer-scheduled)
          (should callback-called)
          (should (equal result-out "Patch applied successfully")))
      (when (buffer-live-p buf) (kill-buffer buf))
      (delete-file temp-file))))

(ert-deftest test-clean-patch-string ()
  "Verify stripping markdown fences from patch strings."
  (let ((markdown-patch "```diff\n--- a/foo\n+++ b/foo\n@@ -1 +1 @@\n-a\n+b\n```"))
    (should (equal (my--clean-patch-string markdown-patch)
                   "--- a/foo\n+++ b/foo\n@@ -1 +1 @@\n-a\n+b\n"))))

(ert-deftest test-cat-file-tool-string-arguments ()
  "Verify cat tool handles string numbers for offset and limit safely."
  (let ((my/custom-gptel-sandbox-profile-path nil)
        (temp-file (make-temp-file "test-cat-str-")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "line1\nline2\nline3\nline4\nline5\n"))
          (let ((res nil))
            (my/cat-file-tool (lambda (out) (setq res out))
                              temp-file "2" "2")
            ;; Wait briefly for async execution
            (while (null res)
              (accept-process-output nil 0.01))
            (should (string-match-p "2:line2" res))
            (should (string-match-p "3:line3" res))
            (should-not (string-match-p "1:line1" res))
            (should-not (string-match-p "4:line4" res))))
      (delete-file temp-file))))

(ert-deftest test-edit-tool-existing-file-new-file-patch ()
  "Verify edit tool removes an existing file if patch specifies --- /dev/null."
  (let* ((my/custom-gptel-sandbox-profile-path nil)
         (temp-file (make-temp-file (expand-file-name "test-edit-exist-" (doom-project-root))))
         (rel-file (file-relative-name temp-file (doom-project-root)))
         (patch (format "--- /dev/null\n+++ b/%s\n@@ -0,0 +1 @@\n+new content\n" rel-file))
         (res nil))
    (unwind-protect
        (progn
          (my/edit-tool (lambda (out) (setq res out)) patch)
          (while (null res)
            (accept-process-output nil 0.01))
          (should-not (string-match-p "Error" res))
          (with-temp-buffer
            (insert-file-contents temp-file)
            (should (equal (buffer-string) "new content\n"))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest test-edit-tool-fallback-patch-application ()
  "Verify edit tool falls back to patch command when git apply fails on context mismatch."
  (let* ((my/custom-gptel-sandbox-profile-path nil)
         (temp-file (make-temp-file (expand-file-name "test-edit-fallback-" (doom-project-root))))
         (rel-file (file-relative-name temp-file (doom-project-root)))
         (res nil))
    (unwind-protect
        (progn
          ;; File has extra blank line at start
          (with-temp-file temp-file
            (insert "\nfunc Hello() {\n\tfmt.Println(\"hello\")\n}\n"))
          ;; Patch expects no blank line at start (git apply will fail, patch -p1 will succeed)
          (let ((patch (format "--- a/%s\n+++ b/%s\n@@ -1,3 +1,3 @@\n func Hello() {\n-\tfmt.Println(\"hello\")\n+\tfmt.Println(\"world\")\n }\n"
                               rel-file rel-file)))
            (my/edit-tool (lambda (out) (setq res out)) patch)
            (while (null res)
              (accept-process-output nil 0.01))
            (should-not (string-match-p "Error applying patch" res))
            (with-temp-buffer
              (insert-file-contents temp-file)
              (should (string-match-p "fmt.Println(\"world\")" (buffer-string))))))
      (when (file-exists-p temp-file)
        (delete-file temp-file))
      (let ((rej (concat temp-file ".rej"))
            (orig (concat temp-file ".orig")))
        (when (file-exists-p rej) (delete-file rej))
        (when (file-exists-p orig) (delete-file orig))))))

(ert-deftest test-split-patch-by-file ()
  "Verify splitting a multi-file patch into per-file patches."
  (let ((multi-patch "--- a/file1.go\n+++ b/file1.go\n@@ -1 +1 @@\n-a\n+b\n--- /dev/null\n+++ b/file2.go\n@@ -0,0 +1 @@\n+c\n"))
    (should (equal (my--split-patch-by-file multi-patch)
                   '("--- a/file1.go\n+++ b/file1.go\n@@ -1 +1 @@\n-a\n+b\n"
                     "--- /dev/null\n+++ b/file2.go\n@@ -0,0 +1 @@\n+c\n")))))

(ert-deftest test-normalize-patch-new-file ()
  "Verify new file patch normalization adds '+' to empty and context lines."
  (let ((bad-patch "--- /dev/null\n+++ b/new.go\n@@ -0,0 +1,3 @@\n+package foo\n\n func bar() {}\n"))
    (should (equal (my--normalize-patch-string bad-patch)
                   "--- /dev/null\n+++ b/new.go\n@@ -0,0 +1,3 @@\n+package foo\n+\n+func bar() {}\n"))))

(ert-deftest test-normalize-patch-existing-file ()
  "Verify existing file patch normalization adds leading spaces to context lines."
  (let ((bad-patch "--- a/old.go\n+++ b/old.go\n@@ -1,3 +1,3 @@\nfunc foo() {\n\tbar()\n+	baz()\n}\n"))
    (should (equal (my--normalize-patch-string bad-patch)
                   "--- a/old.go\n+++ b/old.go\n@@ -1,3 +1,3 @@\n func foo() {\n \tbar()\n+\tbaz()\n }\n"))))

(ert-deftest test-edit-tool-new-file-empty-lines ()
  "Verify edit-tool handles new file patches containing bare empty lines."
  (let* ((my/custom-gptel-sandbox-profile-path nil)
         (target (expand-file-name "test-new-empty.go" (doom-project-root)))
         (rel-file (file-relative-name target (doom-project-root)))
         (patch (format "--- /dev/null\n+++ b/%s\n@@ -0,0 +1,3 @@\n+package main\n\n+func main() {}\n" rel-file))
         (res nil))
    (unwind-protect
        (progn
          (my/edit-tool (lambda (out) (setq res out)) patch)
          (while (null res)
            (accept-process-output nil 0.01))
          (should-not (string-match-p "Error" res))
          (should (file-exists-p target))
          (with-temp-buffer
            (insert-file-contents target)
            (should (equal (buffer-string) "package main\n\nfunc main() {}\n"))))
      (when (file-exists-p target) (delete-file target)))))

(ert-deftest test-edit-tool-multi-file-patch ()
  "Verify edit-tool applies multi-file patches successfully."
  (let* ((my/custom-gptel-sandbox-profile-path nil)
         (file1 (make-temp-file (expand-file-name "test-multi1-" (doom-project-root))))
         (file2 (expand-file-name "test-multi2.go" (doom-project-root)))
         (rel1 (file-relative-name file1 (doom-project-root)))
         (rel2 (file-relative-name file2 (doom-project-root)))
         (patch (format "--- a/%s\n+++ b/%s\n@@ -1 +1 @@\n-old\n+new\n--- /dev/null\n+++ b/%s\n@@ -0,0 +1 @@\n+created\n" rel1 rel1 rel2))
         (res nil))
    (unwind-protect
        (progn
          (with-temp-file file1 (insert "old\n"))
          (my/edit-tool (lambda (out) (setq res out)) patch)
          (while (null res)
            (accept-process-output nil 0.01))
          (should-not (string-match-p "Error" res))
          (with-temp-buffer
            (insert-file-contents file1)
            (should (equal (buffer-string) "new\n")))
          (with-temp-buffer
            (insert-file-contents file2)
            (should (equal (buffer-string) "created\n"))))
      (when (file-exists-p file1) (delete-file file1))
      (when (file-exists-p file2) (delete-file file2)))))
