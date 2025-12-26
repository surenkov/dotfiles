;;; custom-gptel-tools.el --- Custom tools for gptel -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file defines custom asynchronous tools for use with gptel,
;; including file listing, searching, and command execution.

;;; Code:

(declare-function doom-project-root "doom-project")

(defun my/--async-exec (name command callback &optional input)
  "Generic helper to run a process asynchronously.
NAME is the process name, COMMAND is the list of args,
CALLBACK is called with (exit-code output), and optional INPUT is sent to stdin."
  (let* ((default-directory (doom-project-root))
         (output-buffer (generate-new-buffer (format "*%s*" name)))
         (proc (make-process
                :name name
                :buffer output-buffer
                :command (remq nil command)
                :connection-type (if input 'pipe 'pty)
                :sentinel
                (lambda (proc _event)
                  (when (memq (process-status proc) '(exit signal))
                    (let ((exit-code (process-exit-status proc))
                          (output (with-current-buffer output-buffer
                                    (string-trim (buffer-string)))))
                      (kill-buffer output-buffer)
                      (funcall callback exit-code output)))))))
    (when input
      (process-send-string proc input)
      (process-send-eof proc))))

(defun my/cat-file-tool (path &optional offset limit)
  "Read the contents of file at PATH and return it as a string.
OFFSET is the line to start from (default 1).
LIMIT is the maximum number of lines to read (default 2000)."
  (let ((default-directory (doom-project-root))
        (offset (or offset 1))
        (limit (or limit 2000))
        ;; SAFETY: Prevent freezing on massive files (e.g., >10MB)
        (max-size (* 10 1024 1024)))
    (cond
     ((not (file-readable-p path))
      (format "Error: File '%s' is not readable or does not exist." path))
     ((> (file-attribute-size (file-attributes path)) max-size)
      (format "Error: File '%s' is too large (%.2f MB). Limit is 10MB."
              path (/ (file-attribute-size (file-attributes path)) 1048576.0)))
     (t
      (with-temp-buffer
        (insert-file-contents path)
        (let* ((total-lines (count-lines (point-min) (point-max)))
               (end-line (min (+ offset limit -1) total-lines)))
          (goto-char (point-min))
          (when (> offset 1)
            (forward-line (1- offset)))
          (let ((start (point)))
            (forward-line limit)
            (let ((content (buffer-substring-no-properties start (point))))
              (if (or (> offset 1) (< end-line total-lines))
                  (format "%s\n[... File has %d lines. Showing lines %d-%d ...]"
                          content total-lines offset end-line)
                content)))))))))

(defun my/list-files-tool (callback path &optional depth)
  "List files in PATH using fd. Respects .gitignore.
CALLBACK is called with the result string.
DEPTH is the directory traversal limit (integer).
- Default (nil/1): List immediate children only.
- -1: List recursively (no limit)."
  (let* ((default-directory (doom-project-root))
         (full-path (expand-file-name (or path ".") default-directory))
         (rel-path (file-relative-name full-path default-directory)))
    ;; Security: Ensure resolved path is within project root
    (if (not (or (file-equal-p full-path default-directory)
                 (string-prefix-p (expand-file-name default-directory)
                                  (expand-file-name full-path))))
        (funcall callback (format "Error: Access denied. '%s' is outside project root." path))
      (if (not (file-exists-p full-path))
          (funcall callback (format "Error: Path '%s' does not exist." rel-path))
        ;; Handle :json-false (from LLM) or nil as default (1)
        (let* ((depth-val (if (and (integerp depth) (not (eq depth :json-false))) depth 1))
               (command (list "fd"
                              "--color=never"
                              (when (>= depth-val 0) (format "--max-depth=%d" depth-val))
                              "."
                              rel-path)))
          (my/--async-exec
           "gptel-fd"
           command
           (lambda (_code out)
             (funcall callback (if (string-empty-p out) "No files found" out)))))))))

(defun my/rg-tool (callback search-pattern &optional glob path context max-results)
  "Search for SEARCH-PATTERN in files in PATH using ripgrep.
CALLBACK is called with the result string.
GLOB is an optional glob pattern to filter files.
CONTEXT is the number of lines of context to show.
MAX-RESULTS is the maximum number of matches to return."
  (let* ((ignore-file (expand-file-name "~/.config/git/ignore"))
         ;; Handle gptel's :json-false or nil for optional integer args
         (max-results (if (eq max-results :json-false) 50 (or max-results 50)))
         (command (list "rg"
                        "--color=never"
                        "--max-columns=500"
                        "--max-columns-preview"
                        "--with-filename"
                        "--no-heading"
                        "-n"
                        (when (file-exists-p ignore-file) (format "--ignore-file=%s" ignore-file))
                        (when (and (stringp glob) (not (string-empty-p glob))) (format "--glob=%s" glob))
                        (when (and context (> context 0)) (format "--context=%d" context))
                        (when (> max-results 0) (format "--max-count=%d" max-results))
                        "--"
                        search-pattern
                        (when (and (stringp path) (not (string-empty-p path))) path))))
    (my/--async-exec
     "gptel-rg"
     command
     (lambda (_code out)
       (funcall callback (if (string-empty-p out) "No matches found" out))))))

(defun my/fzf-tool (callback pattern &optional exact path depth max-results)
  "Fuzzy search for file names matching PATTERN in PATH using fzf.
CALLBACK is called with the result string.
If EXACT is non-nil, use exact matching.
DEPTH is the directory traversal limit.
MAX-RESULTS is the maximum number of matches to return."
  (let* ((root (doom-project-root))
         ;; 1. Resolve the target path relative to the project root
         (rel-path (if path
                       (file-relative-name (expand-file-name path root) root)
                     "."))
         ;; 2. Use correct flag for rg depth (--max-depth instead of -d)
         (depth-arg (if (and depth (integerp depth)) (format "--max-depth %d" depth) ""))
         (max-results (or max-results 50))
         (ignore-file (expand-file-name "~/.config/git/ignore"))
         (ignore-arg (if (file-exists-p ignore-file)
                         (format "--ignore-file %s" (shell-quote-argument ignore-file))
                       ""))
         (max-lines-arg (if (>= max-results 0)
                            (format " | head -n %s"
                                    (shell-quote-argument (number-to-string max-results)))
                          ""))
         ;; 3. Pass the relative path directly to rg, removing the need for `cd`
         (command (format "rg %s --files %s %s | fzf --ansi --filter=%s %s %s"
                          ignore-arg
                          depth-arg
                          (shell-quote-argument rel-path)
                          (shell-quote-argument pattern)
                          (if exact "--exact" "")
                          max-lines-arg)))
    (my/--async-exec
     "gptel-fzf"
     (list shell-file-name shell-command-switch command)
     (lambda (_code out)
       (funcall callback (if (string-empty-p out) "No matches found" out))))))

(defun my/apply-patch-tool (callback patch)
  "Apply PATCH using git apply and call CALLBACK with the result."
  (let ((patch-content (if (string-suffix-p "\n" patch) patch (concat patch "\n")))
        (command '("git" "apply" "--verbose" "--recount" "--ignore-space-change" "--" "-")))
    (my/--async-exec
     "gptel-git-apply"
     command
     (lambda (code out)
       (funcall callback (if (zerop code)
                             out
                           (format "Error applying patch (exit code %d):\n%s" code out))))
     patch-content)))

(defun my/run-command-tool (callback command)
  "Run COMMAND in a shell and call CALLBACK with the result."
  (my/--async-exec
   "gptel-run-cmd"
   (list shell-file-name shell-command-switch command)
   (lambda (code out)
     (funcall callback (if (zerop code)
                           out
                         (format "Command failed (exit code %d):\n%s" code out))))))

(defun my/read-url-tool (callback url)
  "Fetch content from a URL asynchronously.
CALLBACK is called with the result string."
  (condition-case err
      (url-retrieve
       url
       (lambda (status)
         (condition-case parse-err
             (let ((http-err (plist-get status :error)))
               (if http-err
                   (funcall callback (format "Error fetching URL: %s" http-err))
                 (set-buffer-multibyte t)
                 (goto-char (point-min))
                 ;; Find end of HTTP headers
                 (if (not (re-search-forward "^$" nil t))
                     (funcall callback "Error: Invalid HTTP response (no headers separator found).")
                   (forward-line 1)
                   (let ((content (buffer-substring-no-properties (point) (point-max))))
                     (with-temp-buffer
                       (insert content)
                       (ignore-errors (shr-render-region (point-min) (point-max)))
                       (funcall callback (string-trim (buffer-string))))))))
           (error (funcall callback (format "Error processing response: %s" parse-err))))
         (when (buffer-live-p (current-buffer))
           (kill-buffer (current-buffer))))
       nil t)
    (error (funcall callback (format "Error initiating request: %s" (error-message-string err))))))

(provide 'custom-gptel-tools)
;;; custom-gptel-tools.el ends here

