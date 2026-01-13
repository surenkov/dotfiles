;;; custom-gptel-tools.el --- Custom tools for gptel -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file defines custom asynchronous tools for use with gptel,
;; including file listing, searching, and command execution.

;;; Code:

(require 'cl-lib)
(require 'doom)

(declare-function doom-project-root "doom-lib")

(defvar my/custom-gptel-tools-whitelist-directories nil
  "List of additional directories that tools are allowed to access.
Paths can be absolute or relative to the project root.")

(defvar my/custom-gptel-sandbox-profile-path
  (expand-file-name "my/sandbox-rules.sb" doom-user-dir)
  "Path to the macOS sandbox profile (.sb) for gptel tools.")

(defun my--wrap-sandbox-command (command-args)
  "Wrap COMMAND-ARGS in `sandbox-exec` if the profile exists and we are local.
Injects TARGET_DIR, HOME_DIR, CACHE_DIR, TMP_DIR, and WHITELIST_DIRS."
  (let ((sandbox-exec "/usr/bin/sandbox-exec"))
    (if (and my/custom-gptel-sandbox-profile-path
             (file-exists-p my/custom-gptel-sandbox-profile-path))
        (let* ((root (expand-file-name (doom-project-root)))
               (home (expand-file-name "~"))
               (whitelist (mapcar (lambda (d) (expand-file-name d root))
                                  my/custom-gptel-tools-whitelist-directories))
               (whitelist-str (mapconcat #'identity whitelist ":")))
          (append (list sandbox-exec
                        "-f" my/custom-gptel-sandbox-profile-path
                        "-D" (format "TARGET_DIR=%s" root)
                        "-D" (format "HOME_DIR=%s" home)
                        "-D" (format "TMP_DIR=%s" (expand-file-name temporary-file-directory))
                        "-D" (format "WHITELIST_DIRS=%s" whitelist-str))
                  command-args))
      command-args)))

(defun my--path-allowed-p (path)
  "Return non-nil if PATH is within the project root or whitelisted directories."
  (let* ((root (expand-file-name (doom-project-root)))
         (full-path (expand-file-name path root))
         (allowed-dirs (cons root (mapcar (lambda (d) (expand-file-name d root))
                                          my/custom-gptel-tools-whitelist-directories))))
    (cl-some (lambda (dir)
               (let ((dir-abs (file-name-as-directory (expand-file-name dir))))
                 (string-prefix-p dir-abs (file-name-as-directory full-path))))
             allowed-dirs)))

(defun my--async-exec (name command callback &optional input)
  "Generic helper to run a process asynchronously.
NAME is the process name, COMMAND is the list of args,
CALLBACK is called with (exit-code output), and optional INPUT is sent to stdin."
  (let* ((default-directory (doom-project-root))
         (output-buffer (generate-new-buffer (format "*%s*" name)))
         (proc (make-process
                :name name
                :buffer output-buffer
                :command (remq nil command)
                :connection-type 'pipe
                :sentinel
                (lambda (proc _event)
                  (when (memq (process-status proc) '(exit signal))
                    (let ((exit-code (process-exit-status proc))
                          (output (with-current-buffer output-buffer
                                    (string-trim (buffer-string)))))
                      (kill-buffer output-buffer)
                      (funcall callback exit-code output)))))))
    (when input
      (process-send-string proc input))
    (process-send-eof proc) ;; Close stdin to prevent hanging
    proc)) ;; Return the process object so it can be managed/killed

(defun my/cat-file-tool (path &optional offset limit)
  "Read the contents of file at PATH and return it as a string.
OFFSET is the line to start from (default 1).
LIMIT is the maximum number of lines to read (default 2000)."
  (let ((default-directory (doom-project-root))
        (offset (or offset 1))
        (limit (or limit 2000))
        ;; SAFETY: Prevent freezing on massive files (e.g., >10MB)
        (max-size (* 3 1024 1024)))
    (cond
     ((not (my--path-allowed-p path))
      (format "Error: Access denied. '%s' is outside allowed directories." path))
     ((not (file-readable-p path))
      (format "Error: File '%s' is not readable or does not exist." path))
     ((> (file-attribute-size (file-attributes path)) max-size)
      (format "Error: File '%s' is too large (%.2f MB). Limit is 3MB."
              path (/ (file-attribute-size (file-attributes path)) 1048576.0)))
     (t
      (with-temp-buffer
        (insert-file-contents path)
        (let ((total-lines (max 1 (count-lines (point-min) (point-max)))))
          (if (> offset total-lines)
              (format "[Offset %d is beyond file length (%d lines)...]" offset total-lines)
            (let ((end-line (min (+ offset limit -1) total-lines)))
              (goto-char (point-min))
              (when (> offset 1)
                (forward-line (1- offset)))
              (let ((start (point)))
                (forward-line limit)
                (let* ((lines (split-string (buffer-substring-no-properties start (point)) "\n"))
                       ;; Eliminate the last empty line, if present
                       (lines (if (and (> (length lines) 1) (string-empty-p (car (last lines))))
                                  (butlast lines)
                                lines))
                       (line-num (1- offset))
                       (content (mapconcat (lambda (line)
                                             (format "%d:%s" (setq line-num (1+ line-num)) line))
                                           lines
                                           "\n")))
                  (if (or (> offset 1) (< end-line total-lines))
                      (format "%s\n[... File has %d lines. Showing lines %d-%d ...]"
                              content total-lines offset end-line)
                    content)))))))))))

(defun my/fd-tool (callback path &optional pattern depth include-hidden)
  "List files in PATH using `fd'. Respects .gitignore.
CALLBACK is called with the result string.
PATTERN is an optional regex pattern to filter files.
DEPTH is the directory traversal limit (integer).
- Default (nil/1): List immediate children only.
- -1: List recursively (no limit).
INCLUDE-HIDDEN is a boolean to include hidden files and directories."
  (let* ((root (doom-project-root))
         (pattern (or pattern "."))
         (full-path (expand-file-name (or path "./") root))
         (rel-path (file-relative-name full-path root)))
    (if (not (my--path-allowed-p full-path))
        (funcall callback (format "Error: Access denied. '%s' is outside allowed directories." (or path ".")))
      (if (not (file-exists-p full-path))
          (funcall callback (format "Error: Path '%s' does not exist." rel-path))
        ;; Handle :json-false (from LLM) or nil as default (1)
        (let* ((depth-val (if (and (integerp depth)
                                   (not (eq depth :json-false)))
                              depth 1))
               (command (list "fd"
                              "--color=never"
                              (when (>= depth-val 0) (format "--max-depth=%d" depth-val))
                              (when (and include-hidden (not (eq include-hidden :json-false))) "--hidden")
                              pattern
                              rel-path)))
          (my--async-exec
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
  (if (and path (not (my--path-allowed-p path)))
      (funcall callback (format "Error: Access denied. '%s' is outside allowed directories." path))
    (let* ((ignore-file (expand-file-name "~/.config/git/ignore"))
           (max-results (if (eq max-results :json-false) 50 (or max-results 50)))
           (command (list
                     "rg"
                     "--color=never"
                     "--max-columns=500"
                     "--max-columns-preview"
                     "--with-filename"
                     "--no-heading"
                     "-n"
                     (when (file-exists-p ignore-file) (format "--ignore-file=%s" ignore-file))
                     (when (and (stringp glob) (not (string-empty-p glob))) (format "--glob=%s" glob))
                     (when (and (integerp context) (> context 0)) (format "--context=%d" context))
                     (when (> max-results 0) (format "--max-count=%d" max-results))
                     "--"
                     search-pattern
                     (or path "."))))
      (my--async-exec
       "gptel-rg"
       command
       (lambda (_code out)
         (funcall callback (if (string-empty-p out) "No matches found" out)))))))

(defun my/fzf-tool (callback pattern &optional exact path depth max-results)
  "Fuzzy search for file names matching PATTERN in PATH using fzf.
CALLBACK is called with the result string.
If EXACT is non-nil, use exact matching.
DEPTH is the directory traversal limit.
MAX-RESULTS is the maximum number of matches to return."
  (let* ((default-directory (doom-project-root))
         (path (if (eq path :json-false) nil path))
         (full-path (expand-file-name (or path ".")))
         (rel-path (file-relative-name full-path)))
    (if (not (my--path-allowed-p full-path))
        (funcall callback (format "Error: Access denied. '%s' is outside allowed directories." rel-path))
      (let* ((depth-arg (if (and (integerp depth) (not (eq depth :json-false))) (format "--max-depth %d" depth) ""))
             (max-results (if (eq max-results :json-false) 50 (or max-results 50)))
             (ignore-file (expand-file-name "~/.config/git/ignore"))
             (ignore-arg (if (file-exists-p ignore-file) (format "--ignore-file %s" (shell-quote-argument ignore-file)) ""))
             (max-lines-arg (if (>= max-results 0) (format " | head -n %s" (shell-quote-argument (number-to-string max-results))) ""))
             (command (format "rg %s --files %s %s | fzf --ansi --filter=%s %s %s"
                              ignore-arg
                              depth-arg
                              (shell-quote-argument rel-path)
                              (shell-quote-argument pattern)
                              (if exact "--exact" "")
                              max-lines-arg)))
        (my--async-exec
         "gptel-fzf"
         (list shell-file-name shell-command-switch command)
         (lambda (_code out) (funcall callback (if (string-empty-p out) "No matches found" out))))))))

(defun my/apply-patch-tool (callback patch)
  "Apply PATCH using git apply and call CALLBACK with the result."
  (let ((patch-content (if (string-suffix-p "\n" patch) patch (concat patch "\n")))
        (command '("git" "apply" "--verbose" "--recount" "--ignore-space-change" "--" "-")))
    (my--async-exec
     "gptel-git-apply"
     command
     (lambda (code out)
       (funcall callback (if (zerop code)
                             out
                           (format "Error applying patch (exit code %d):\n%s" code out))))
     patch-content)))

(defun my/bash-tool (callback command)
  "Run COMMAND in a shell and call CALLBACK with the result."
  (my--async-exec
   "gptel-shell-cmd"
   (my--wrap-sandbox-command
    (list shell-file-name shell-command-switch command))
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

(defconst my/gptel-custom-tools
  '((:name "fd"
     :function my/fd-tool
     :category "filesystem"
     :description "List files and directories in a path using `fd'. Like `ls', but more powerful."
     :async t
     :args ((:name "path" :type string :description "Directory path relative to project root. Default is '.' (a project root)" :optional t)
            (:name "pattern" :type string :description "Search pattern (regular expression). Consider using it with recursive `depth', if non-default. Default is '.' (match anything)." :optional t)
            (:name "depth" :type integer :description "Depth limit, e.g. 1: immediate children, -1: recursive. Default: 1." :optional t)
            (:name "include-hidden" :type boolean :description "Include hidden files and directories. Default: false" :optional t)))
    (:name "rg"
     :function my/rg-tool
     :category "filesystem"
     :description "Search file content using regex with `ripgrep'"
     :async t
     :args ((:name "search-pattern" :type string :description "Regex pattern to search in file contents")
            (:name "glob" :type string :description "Glob pattern for files to include in search (e.g., \"*.py\")" :optional t)
            (:name "path" :type string :description "Path to a file or a directory to search in. Default is a project root" :optional t)
            (:name "context" :type integer :description "Show NUM lines before and after each match. Default is to show only a matching line." :optional t)
            (:name "max-results" :type integer :description "Output only the first `max-results'. Default: 50. Set to -1 for no limit." :optional t)))
    (:name "fzf"
     :function my/fzf-tool
     :category "filesystem"
     :description "Fuzzy search patterns in file names using `fzf'. More powerful than `fd', but works on files only."
     :async t
     :args ((:name "pattern" :type string :description "Fuzzy search pattern for file names. Empty string to match all files.")
            (:name "exact" :type boolean :description "Enable exact-match." :optional t)
            (:name "path" :type string :description "Directory to search in. Default is a project root." :optional t)
            (:name "depth" :type integer :description "Limit the traversal depth, if specified. Do not limit by default." :optional t)
            (:name "max-results" :type integer :description "Output only the first `max-results'. Default: 50. Set to -1 for no limit." :optional t)))
    (:name "cat"
     :function my/cat-file-tool
     :category "filesystem"
     :description "Read the content of a file with line numbers in N:line format. To paginate over the file, use the `limit' and `offset' parameters in subsequent `cat' calls"
     :args ((:name "path" :type string :description "Path to the file to read.")
            (:name "offset" :type integer :description "Line number to start reading from (1-based). Default: 1." :optional t)
            (:name "limit" :type integer :description "Maximum number of lines to return. Default: 2000." :optional t)))
    (:name "apply_patch"
     :function my/apply-patch-tool
     :category "filesystem"
     :description "Apply a git-formatted PATCH string using `git apply'. The patch is applied relative to the project root. Use this tool for file edits."
     :async t
     :confirm t
     :args ((:name "patch" :type string :description "A git-formatted patch string to apply.")))
    (:name "bash"
     :function my/bash-tool
     :category "system"
     :description "Run a shell command using `bash'. Use ONLY for non-destructive commands. NEVER use for file manipulation."
     :async t
     :confirm t
     :args ((:name "command" :type string :description "A shell command to run. Only safe commands are allowed.")))
    (:name "read_url"
     :function my/read-url-tool
     :category "network"
     :description "Fetch and read the visible text content of a URL."
     :async t
     :confirm t
     :args ((:name "url" :type string :description "The URL to fetch.")))))

(provide 'custom-gptel-tools)
;;; custom-gptel-tools.el ends here
