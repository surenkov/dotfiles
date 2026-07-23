;;; custom-gptel-tools.el --- Custom tools for gptel -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file defines custom asynchronous tools for use with gptel,
;; including file listing, searching, and command execution.

;;; Code:

(require 'cl-lib)
(require 'custom-gptel-skills)
(require 'custom-gptel-agents)
(require 'custom-gptel-ui)

(defvar doom-user-dir)
(declare-function doom-project-root "doom-lib")

(defvar my/custom-gptel-tools-whitelist-directories nil
  "List of additional directories that tools are allowed to access.
Paths can be absolute or relative to the project root.")

(defvar my/custom-gptel-awk-script-edit-path
  (expand-file-name "my/scripts/edit.awk" (if (boundp 'doom-user-dir) doom-user-dir "~/.config/doom/"))
  "Path to the AWK script for gptel edit tool.")

(defvar my/custom-gptel-edit-pipeline-script-path
  (expand-file-name "my/scripts/edit_pipeline.sh" (if (boundp 'doom-user-dir) doom-user-dir "~/.config/doom/"))
  "Path to the multi-tier edit pipeline script.")

(defvar my/custom-gptel-sandbox-profile-path
  (expand-file-name "my/sandbox-rules.sb" (if (boundp 'doom-user-dir) doom-user-dir "~/.config/doom/"))
  "Path to the macOS sandbox profile (.sb) for gptel tools.")

(defvar my/custom-gptel-skills-directory
  (expand-file-name "my/skills" (if (boundp 'doom-user-dir) doom-user-dir "~/.config/doom/"))
  "Path to the directory containing methodology skills.")

(defvar my/custom-gptel-tool-output-limit (* 100 1024)
  "Maximum output size in bytes for shell commands before truncation/caching.")

(defcustom my/custom-gptel-edit-diagnostic-delay 1.5
  "Delay in seconds before checking diagnostics after applying a patch."
  :type 'number
  :group 'custom-gptel)

(defvar my/custom-gptel--path-cache (make-hash-table :test 'equal)
  "Hash table mapping raw paths to their expanded file truenames.")

(defvar my/custom-gptel--allowed-dirs-cache nil
  "Cached list of allowed directories for file access.")

(defun my/custom-gptel-clear-caches ()
  "Clear cached path resolution and directory structures."
  (interactive)
  (clrhash my/custom-gptel--path-cache)
  (setq my/custom-gptel--allowed-dirs-cache nil))

(defun my/custom-gptel--get-truename (path &optional dir)
  "Return cached truename for PATH expanded relative to optional DIR."
  (let ((key (if dir (cons path dir) path)))
    (or (gethash key my/custom-gptel--path-cache)
        (puthash key (file-truename (expand-file-name path dir)) my/custom-gptel--path-cache))))

(defun my/custom-gptel--get-allowed-cache ()
  "Return cached list of allowed directories."
  (or my/custom-gptel--allowed-dirs-cache
      (setq my/custom-gptel--allowed-dirs-cache
            (let ((root (my/custom-gptel--get-truename (doom-project-root))))
              (cons root
                    (mapcar (lambda (d) (my/custom-gptel--get-truename d root))
                            (append my/custom-gptel-tools-whitelist-directories
                                    (my/get-resolved-skill-dirs))))))))

(defun my--wrap-sandbox-command (command-args)
  "Wrap COMMAND-ARGS in `sandbox-exec` if the profile exists and we are local.
Injects TARGET_DIR, HOME_DIR, TMP_DIR, and WHITELIST_DIRS."
  (let ((sandbox-exec "/usr/bin/sandbox-exec"))
    (if my/custom-gptel-sandbox-profile-path
        (let* ((root (my/custom-gptel--get-truename (doom-project-root)))
               (home (my/custom-gptel--get-truename "~"))
               (tmp (my/custom-gptel--get-truename temporary-file-directory))
               (whitelist (my/custom-gptel--get-allowed-cache))
               (whitelist-str (mapconcat #'identity whitelist ":")))
          (append (list sandbox-exec
                        "-f" my/custom-gptel-sandbox-profile-path
                        "-D" (format "TARGET_DIR=%s" root)
                        "-D" (format "HOME_DIR=%s" home)
                        "-D" (format "TMP_DIR=%s" tmp)
                        "-D" (format "WHITELIST_DIRS=%s" whitelist-str))
                  command-args))
      command-args)))

(defun my--truncate-buffer-in-place (buf max-lines &optional total-bytes orig-string)
  "Truncate buffer BUF in place to MAX-LINES or `my/custom-gptel-tool-output-limit`.
TOTAL-BYTES is optional byte length of BUF.
If ORIG-STRING is provided, it is returned when truncation is not needed.
Returns the output string (truncated with notice if exceeding limits)."
  (if (not (buffer-live-p (get-buffer buf)))
      ""
    (with-current-buffer buf
      (goto-char (point-min))
      (skip-chars-forward " \t\n\r")
      (delete-region (point-min) (point))
      (goto-char (point-max))
      (skip-chars-backward " \t\n\r")
      (delete-region (point) (point-max))
      (if (= (buffer-size) 0)
          ""
        (let ((max-lines (or max-lines 200)))
          (if (and (integerp max-lines) (< max-lines 0))
              (or orig-string (buffer-string))
            (let* ((bytes (or total-bytes
                              (if (and (position-bytes (point-max))
                                       (position-bytes (point-min)))
                                  (- (position-bytes (point-max))
                                     (position-bytes (point-min)))
                                (string-bytes (buffer-substring-no-properties (point-min) (point-max))))))
                   (len (count-lines (point-min) (point-max))))
              (if (and (<= len max-lines)
                       (<= bytes my/custom-gptel-tool-output-limit))
                  (or orig-string (buffer-string))
                (let* ((temp-file (make-temp-file "gptel-output-"))
                       (keep (max 1 (or (/ max-lines 2) 100)))
                       (first-part-raw
                        (progn
                          (goto-char (point-min))
                          (forward-line keep)
                          (buffer-substring-no-properties (point-min) (point))))
                       (last-part-raw
                        (progn
                          (goto-char (point-max))
                          (forward-line (- keep))
                          (buffer-substring-no-properties (point) (point-max))))
                       (half-limit (/ my/custom-gptel-tool-output-limit 2))
                       (head-part (if (> (string-bytes first-part-raw) half-limit)
                                      (substring first-part-raw 0 (min (length first-part-raw) half-limit))
                                    first-part-raw))
                       (tail-part (if (> (string-bytes last-part-raw) half-limit)
                                      (substring last-part-raw (max 0 (- (length last-part-raw) half-limit)) (length last-part-raw))
                                    last-part-raw))
                       (truncated-bytes (- bytes (string-bytes head-part) (string-bytes tail-part)))
                       (formatted-msg (format "[... Truncated %d bytes/lines from middle. Total size: %.2f KB, total lines: %d ...]
%s
...
%s
[... Full output saved to temporary file: %s ...]
You can read the complete output by invoking the `cat` tool on the path above if needed."
                                              truncated-bytes
                                              (/ bytes 1024.0)
                                              len
                                              head-part
                                              tail-part
                                              temp-file)))
                  (write-region (point-min) (point-max) temp-file nil 'no-message)
                  formatted-msg)))))))))

(defun my--truncate-output (output &optional max-lines)
  "Truncate OUTPUT (a string or buffer) to MAX-LINES or `my/custom-gptel-tool-output-limit`.
If MAX-LINES is negative, bypass truncation and return OUTPUT as a string.
If OUTPUT exceeds either limit, write full text to a secure temporary file."
  (cond
   ((null output) "")
   ((bufferp output)
    (my--truncate-buffer-in-place output max-lines))
   ((stringp output)
    (if (string-empty-p output)
        ""
      (if (and (integerp max-lines) (< max-lines 0))
          output
        (with-temp-buffer
          (insert output)
          (my--truncate-buffer-in-place (current-buffer) max-lines (string-bytes output) output)))))
   (t "")))

(defun my--get-visiting-buffers (files &optional project-root)
  "Return a list of live buffers visiting any file in FILES.
FILES is a list of file paths (relative or absolute).
Optional PROJECT-ROOT defaults to `doom-project-root`."
  (let ((root (or project-root (doom-project-root)))
        (bufs nil))
    (dolist (file files)
      (when (and (stringp file) (not (string-empty-p file)))
        (let* ((abs-path (expand-file-name file root))
               (truename (ignore-errors (file-truename abs-path)))
               (buf (or (ignore-errors (find-buffer-visiting abs-path))
                        (and truename (ignore-errors (find-buffer-visiting truename))))))
          (when (and buf (buffer-live-p buf))
            (push buf bufs)))))
    (delete-dups (nreverse bufs))))

(defun my--get-diagnostics-for-file (path &optional project-root)
  "Retrieve active diagnostics/errors for the file at PATH.
Optional PROJECT-ROOT defaults to `doom-project-root`.
Checks Flycheck, Flymake, and LSP (Eglot or lsp-mode) if available."
  (let* ((root (or project-root (doom-project-root)))
         (abs-path (and (stringp path) (not (string-empty-p path)) (expand-file-name path root)))
         (truename (and abs-path (ignore-errors (file-truename abs-path))))
         (buf (and abs-path
                   (or (ignore-errors (find-buffer-visiting abs-path))
                       (and truename (ignore-errors (find-buffer-visiting truename)))))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let (diags)
          ;; 1. Check Flycheck
          (when (and (bound-and-true-p flycheck-mode)
                     (boundp 'flycheck-current-errors))
            (dolist (err flycheck-current-errors)
              (push (format "Flycheck [%s] Line %d: %s"
                            (symbol-name (flycheck-error-level err))
                            (flycheck-error-line err)
                            (flycheck-error-message err))
                    diags)))
          ;; 2. Check Flymake (used by Eglot)
          (when (and (bound-and-true-p flymake-mode)
                     (fboundp 'flymake-diagnostics))
            (dolist (diag (flymake-diagnostics))
              (let* ((locus (flymake-diagnostic-buffer diag))
                     (beg (flymake-diagnostic-beg diag))
                     (line (with-current-buffer locus
                             (line-number-at-pos beg))))
                (push (format "Flymake [%s] Line %d: %s"
                              (symbol-name (flymake-diagnostic-type diag))
                              line
                              (flymake-diagnostic-text diag))
                      diags))))
          (when diags
            (mapconcat #'identity (nreverse diags) "\n")))))))

(defun my--async-exec (name command callback &optional input max-lines)
  "Generic helper to run a process asynchronously.
NAME is the process name, COMMAND is the list of args,
CALLBACK is called with (exit-code output), optional INPUT is sent to stdin,
and MAX-LINES specifies truncation limits."
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
                          (output (my--truncate-output output-buffer (or max-lines 300))))
                      (kill-buffer output-buffer)
                      ;; Truncate output to avoid overloading model context (default 300 lines)
                      (funcall callback exit-code output)))))))
    (when input
      (process-send-string proc input))
    (process-send-eof proc) ;; Close stdin to prevent hanging
    proc)) ;; Return the process object so it can be managed/killed

(defun my--coerce-number (val default)
  "Return VAL as a number, using DEFAULT if VAL is nil or invalid."
  (cond
   ((numberp val) val)
   ((and (stringp val) (string-match-p "\\`-?[0-9]+\\'" val)) (string-to-number val))
   (t default)))

(defun my/cat-file-tool (callback path &optional offset limit)
  "Read the contents of file at PATH and return formatted lines with line numbers.
CALLBACK is called with the result string.
OFFSET is the line to start from (default 1).
LIMIT is the maximum number of lines to read (default 2000)."
  (let ((default-directory (doom-project-root))
        (offset (my--coerce-number offset 1))
        (limit (my--coerce-number limit 2000)))
    (cond
     ((not (file-readable-p path))
      (funcall callback (format "Error: File '%s' is not readable or does not exist." path)))
     (t
      (let* ((start offset)
             (end (+ offset limit -1))
             (abs-path (file-truename (expand-file-name path (doom-project-root))))
             (script "NR >= start && NR <= end { print $0 }\nEND {\n  if (NR == 0) {\n    if (start > 1) {\n      printf \"[Offset %d is beyond file length (0 lines)...]\\n\", start\n    }\n  } else if (start > NR) {\n    printf \"[Offset %d is beyond file length (%d lines)...]\\n\", start, NR\n  } else {\n    end_line = (end < NR) ? end : NR\n    if (start > 1 || end < NR) {\n      printf \"[... File has %d lines. Showing lines %d-%d ...]\\n\", NR, start, end_line\n    }\n  }\n}")
             (command (list "awk"
                            "-v" (format "start=%d" start)
                            "-v" (format "end=%d" end)
                            script
                            abs-path)))
        (my--async-exec
         "gptel-cat"
         (my--wrap-sandbox-command command)
         (lambda (exit-code output)
           (if (zerop exit-code)
               (funcall callback (string-trim-right output))
             (funcall callback (format "Error reading file '%s' (exit code %d):\n%s" path exit-code output))))
         nil
         -1))))))


(defun my/fd-tool (callback path &optional pattern depth include-hidden)
  "List files in PATH using `fd'. Respects .gitignore.
CALLBACK is called with the result string.
PATTERN is an optional regex pattern to filter files.
DEPTH is the directory traversal limit (integer).
- Default (nil/1): List immediate children only.
- -1: List recursively (no limit).
INCLUDE-HIDDEN is a boolean to include hidden files and directories."
  (let* ((root (doom-project-root))
         (full-path (expand-file-name (or path "./") root))
         (rel-path (file-relative-name full-path root)))
    (if (not (file-exists-p full-path))
        (funcall callback (format "Error: Path '%s' does not exist." rel-path))
      (let* ((depth-val (cond
                         ((and depth (not (eq depth :json-false))) (my--coerce-number depth 1))
                         ((and (not (null pattern)) (not (string-empty-p pattern))) -1)
                         ((and (or (null path) (eq path :json-false))) 1)
                         (t 1)))
             (command (list "fd"
                            "--color=never"
                            (when (>= depth-val 0) (format "--max-depth=%d" depth-val))
                            (when (and include-hidden (not (eq include-hidden :json-false))) "--hidden")
                            (or pattern ".")
                            rel-path)))
        (my--async-exec
         "gptel-fd"
         (my--wrap-sandbox-command command)
         (lambda (_code out)
           (funcall callback (if (string-empty-p out) "No files found" out))))))))

(defun my/rg-tool (callback search-pattern &optional glob path context max-results)
  "Search for SEARCH-PATTERN in files in PATH using ripgrep.
CALLBACK is called with the result string.
GLOB is an optional glob pattern to filter files.
CONTEXT is the number of lines of context to show.
MAX-RESULTS is the maximum number of matches to return."
  (let* ((ignore-file (expand-file-name "~/.config/git/ignore"))
         (max-results (my--coerce-number max-results 50))
         (context-num (my--coerce-number context 0))
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
                   (when (> context-num 0) (format "--context=%d" context-num))
                   (when (> max-results 0) (format "--max-count=%d" max-results))
                   "--"
                   search-pattern
                   (or path "."))))
    (my--async-exec
     "gptel-rg"
     (my--wrap-sandbox-command command)
     (lambda (_code out)
       (funcall callback (if (string-empty-p out) "No matches found" out))))))

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
    (let* ((depth-arg (if (and depth (not (eq depth :json-false))) (format "--max-depth %d" (my--coerce-number depth 1)) ""))
           (ignore-file (expand-file-name "~/.config/git/ignore"))
           (ignore-arg (if (file-exists-p ignore-file) (format "--ignore-file %s" (shell-quote-argument ignore-file)) ""))
           (max-results (my--coerce-number max-results 50))
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
       (my--wrap-sandbox-command
        (list shell-file-name shell-command-switch command))
       (lambda (_code out) (funcall callback (if (string-empty-p out) "No matches found" out)))))))

(cl-defun my/edit-tool (callback path old new &optional replace-all)
  "Perform string replacement on file at PATH replacing OLD with NEW.
If REPLACE-ALL is non-nil, replace all occurrences.
Uses a multi-tier pipeline (AWK + rg + fzf) via `my--async-exec` and `my--wrap-sandbox-command` for non-blocking sandboxed execution.
CALLBACK is called with the result string."
  (let* ((actual-path (or (and (stringp path) (not (string-empty-p path)) path)))
         (replace-all (if (eq replace-all :json-false) nil replace-all)))
    (unless actual-path
      (funcall callback "Error: path is required")
      (cl-return-from my/edit-tool nil))
    (when (equal old new)
      (funcall callback "Error: No changes to apply: `old' and `new' strings are identical.")
      (cl-return-from my/edit-tool nil))

    (let* ((root (doom-project-root))
           (file-path (expand-file-name actual-path root))
           (rel-path (file-relative-name file-path root)))
      (condition-case err
          (progn
            (when (and (not (string-empty-p old)) (file-directory-p file-path))
              (error "Path is a directory, not a file: %s" file-path))
            (let ((process-environment (append (list (format "OLD_STRING=%s" old)
                                                     (format "NEW_STRING=%s" new)
                                                     (format "REPLACE_ALL=%s" (if replace-all "1" "0"))
                                                     (format "FILE_PATH=%s" file-path))
                                               process-environment))
                  (command (list my/custom-gptel-edit-pipeline-script-path)))
              (my--async-exec
               "gptel-edit-pipeline"
               (my--wrap-sandbox-command command)
               (lambda (code out)
                 (if (zerop code)
                     (progn
                       ;; Revert visiting buffer if open
                       (let ((buf (find-buffer-visiting file-path)))
                         (when (buffer-live-p buf)
                           (with-current-buffer buf
                             (revert-buffer t t t))))
                       (let ((diags (my--get-diagnostics-for-file file-path root))
                             (msg (format "Edited file successfully: %s\nReplacements: 1" rel-path)))
                         (if diags
                             (funcall callback (format "%s\n\nWARNING: Post-patch compilation/linting errors found:\nFile `%s` diagnostics:\n%s"
                                                       msg rel-path diags))
                           (funcall callback msg))))
                   (funcall callback out))))))
        (error
         (funcall callback (format "Error: %s" (error-message-string err))))))))

(defun my/shell-tool (callback command)
  "Run COMMAND in a shell and call CALLBACK with the result."
  (my--async-exec
   "gptel-shell-cmd"
   (my--wrap-sandbox-command
    (list shell-file-name shell-command-switch command))
   (lambda (code out)
     (if (not (zerop code))
         (funcall callback (format "Command failed (exit code %d):\n%s" code out))
       (funcall callback out)))))

(defvar my/read-url-timeout 10
  "Timeout in seconds for `my/read-url-tool`.")

(defun my/read-url-tool (callback url)
  "Fetch content from a URL asynchronously with a timeout."
  (let* ((timeout-fired nil)
         (timer nil)
         (retrieval-buf nil))
    (setq retrieval-buf
          (condition-case err
              (url-retrieve
               url
               (lambda (status)
                 (when timer (cancel-timer timer) (setq timer nil))
                 (unless timeout-fired
                   (condition-case parse-err
                       (let ((http-err (plist-get status :error)))
                         (if http-err
                             (funcall callback (format "Error fetching URL: %s" http-err))
                           (set-buffer-multibyte t)
                           (goto-char (point-min))
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
                     (kill-buffer (current-buffer)))))
               nil t)
            (error
             (funcall callback (format "Error initiating request: %s" (error-message-string err)))
             nil)))
    (when (buffer-live-p retrieval-buf)
      (setq timer
            (run-with-timer
             my/read-url-timeout nil
             (lambda ()
               (setq timeout-fired t)
               (when (buffer-live-p retrieval-buf) (kill-buffer retrieval-buf))
               (funcall callback (format "Error: Request timed out after %d seconds." my/read-url-timeout))))))))


(defun my/skill-tool (&optional name)
  "List available methodology skills or read a specific skill's instructions."
  (if (and name (not (string-empty-p name)))
      (my/gptel-get-skill-body name)
    (let ((skills (my/gptel-update-skills)))
      (if (null skills)
          "No available methodology skills found."
        (format "Available skills:\n%s"
                (mapconcat (lambda (s) (format "- %s: %s"
                                               (car s)
                                               (or (plist-get (cdr (cdr s)) :description)
                                                   "(no description)")))
                           skills "\n"))))))

(defconst my/gptel-custom-tools
  '((:name "skill"
     :function my/skill-tool
     :category "agent"
     :description "Retrieve detailed instructions and guidelines for a specific development methodology skill."
     :args ((:name "name" :type string :description "The name of the skill to retrieve instructions for.")))
    (:name "fd"
     :function my/fd-tool
     :category "filesystem"
     :description "List files and directories using `fd'. Smart defaults: lists immediate children, but switches to recursive search if a `pattern' is provided."
     :async t
     :args ((:name "path" :type string :description "Directory path relative to project root. Default: project root." :optional t)
            (:name "pattern" :type string :description "Regex pattern to filter files. If provided, search defaults to recursive (-1) unless `depth' is explicitly set." :optional t)
            (:name "depth" :type integer :description "Depth limit, e.g. 1: immediate children, -1: recursive. Default: 1 (or -1 if `pattern` is present)." :optional t)
            (:name "include-hidden" :type boolean :description "Include hidden files/directories. Default: false." :optional t)))
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
     :description "Read the content of a file. To paginate over large files, use the `limit' and `offset' parameters in subsequent `cat' calls."
     :async t
     :args ((:name "path" :type string :description "Path to the file to read.")
            (:name "offset" :type integer :description "Line number to start reading from (1-based). Default: 1." :optional t)
            (:name "limit" :type integer :description "Maximum number of lines to return. Default: 2000." :optional t)))
    (:name "edit"
     :function my/edit-tool
     :category "filesystem"
     :description "Performs string replacements in a file using `old' and `new' strings.
Usage:
- path: Relative path from project root or absolute path of file to modify.
- old: Exact text to replace.
- new: Replacement text (must differ from `old' string).
- replace-all: Optional boolean to replace all occurrences of `old' string (default false).
If `old' string is empty (\"\") and the file does not exist, a new file will be created with `new' string.
If `old' string is empty (\"\") and the file exists, the tool will return an error."
     :async t
     :confirm t
     :args ((:name "path" :type string :description "Path to the file to modify.")
            (:name "old" :type string :description "The exact text to replace.")
            (:name "new" :type string :description "The text to replace it with.")
            (:name "replace-all" :type boolean :description "Replace all occurrences of `old' string (default false)." :optional t)))
    (:name "bash"
     :function my/shell-tool
     :category "system"
     :description "Run a shell command using `bash'. The tool is sandboxed and no destructive commands are allowed."
     :async t
     :confirm t
     :args ((:name "command" :type string :description "A shell command to run. Only safe commands are allowed.")))
    (:name "read_url"
     :function my/read-url-tool
     :category "network"
     :description "Fetch and read the visible text content of a URL."
     :async t
     :confirm t
     :args ((:name "url" :type string :description "The URL to fetch.")))
    (:name "agent"
     :function my/gptel-agent--task
     :category "agent"
     :description "Delegate a complex sub-task to a dedicated sub-agent.
The sub-agent is a derivative of yourself and can only use a subset of the tools currently available to you.
Use this to parallelize work, focus on a narrow problem, or distribute complex research."
     :async t
     :confirm t
     :args ((:name "description" :type string :description "A concise description of the task for the sub-agent.")
            (:name "prompt" :type string :description "The detailed instructions and objective for the sub-agent.")
            (:name "tools"
             :type array
             :description "The subset of your tools to delegate to this sub-agent (e.g., [\"fd\", \"rg\", \"cat\"]). You can only delegate tools that are currently active in your own session."
             :items (:type string))))
    (:name "todo_write"
     :function my/gptel-write-todo
     :category "agent"
     :description "Display a task list/todo list in the buffer. Use this to track plan execution progress. Exactly one item should have status \"in_progress\"."
     :confirm nil
     :args ((:name "todos"
             :type array
             :description "List/vector of maps, each with keys :content, :activeForm, and :status (\"completed\", \"in_progress\", or \"todo\")."
             :items (:type object
                     :properties (:content (:type string :minLength 1 :description "Imperative form describing what needs to be done (e.g., 'Run tests')")
                                  :status (:type string :enum ["todo" "in_progress" "completed"] :description "Task status: todo, in_progress, or completed (exactly one)")
                                  :activeForm (:type string :minLength 1 :description "Present continuous form shown during execution (e.g., 'Running tests')"))))))))

(provide 'custom-gptel-tools)
;;; custom-gptel-tools.el ends here
