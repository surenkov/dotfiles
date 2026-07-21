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

(defvar my/custom-gptel-sandbox-profile-path
  (expand-file-name "my/sandbox-rules.sb" (if (boundp 'doom-user-dir) doom-user-dir "~/.config/doom/"))
  "Path to the macOS sandbox profile (.sb) for gptel tools.")

(defvar my/custom-gptel-skills-directory
  (expand-file-name "my/skills" (if (boundp 'doom-user-dir) doom-user-dir "~/.config/doom/"))
  "Path to the directory containing methodology skills.")

(defvar my/custom-gptel-tool-output-limit (* 100 1024)
  "Maximum output size in bytes for `bash` tool commands before truncation and caching.")

(defun my--wrap-sandbox-command (command-args)
  "Wrap COMMAND-ARGS in `sandbox-exec` if the profile exists and we are local.
Injects TARGET_DIR, HOME_DIR, CACHE_DIR, TMP_DIR, and WHITELIST_DIRS."
  (let ((sandbox-exec "/usr/bin/sandbox-exec"))
    (if my/custom-gptel-sandbox-profile-path
        (let* ((root (expand-file-name (doom-project-root)))
               (home (expand-file-name "~"))
               (whitelist (mapcar (lambda (d) (expand-file-name d root))
                                  (append my/custom-gptel-tools-whitelist-directories
                                          (my/get-resolved-skill-dirs))))
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
  "Return non-nil if PATH is within allowed directories or is a valid gptel bash temp file.
Resolves symlinks before checking."
  (let* ((root (file-truename (expand-file-name (doom-project-root))))
         (full-path (file-truename (expand-file-name path root)))
         (temp-prefix (file-truename (expand-file-name (concat (file-name-as-directory (file-truename temporary-file-directory)) "gptel-bash-output-"))))
         (allowed-dirs (cons root (mapcar (lambda (d) (file-truename (expand-file-name d root)))
                                          (append my/custom-gptel-tools-whitelist-directories
                                                  (my/get-resolved-skill-dirs))))))
    (or (string-prefix-p temp-prefix full-path)
        (cl-some (lambda (dir)
                   (let ((dir-abs (file-name-as-directory dir)))
                     (string-prefix-p dir-abs (file-name-as-directory full-path))))
                 allowed-dirs))))

(defun my--truncate-output (text &optional max-lines)
  "Truncate TEXT to MAX-LINES or `my/custom-gptel-tool-output-limit`.
If MAX-LINES is negative, bypass truncation and return TEXT unchanged.
If TEXT exceeds either limit, write full text to a secure temporary file."
  (if (or (not text) (string-empty-p text))
      ""
    (let* ((max-lines (or max-lines 200))
           (bytes (string-bytes text)))
      (if (and (integerp max-lines) (< max-lines 0))
          text
        (with-temp-buffer
          (insert text)
          (let ((len (count-lines (point-min) (point-max))))
            (if (and (<= len max-lines)
                     (<= bytes my/custom-gptel-tool-output-limit))
                text
              (let* ((temp-file (make-temp-file "gptel-bash-output-"))
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
                     (formatted-msg (format "[... Truncated %d bytes/lines from middle. Total size: %.2f KB, total lines: %d ...]\n%s\n...\n%s\n[... Full output saved to temporary file: %s ...]\nYou can read the complete output by invoking the `cat` tool on the path above if needed."
                                             truncated-bytes
                                             (/ bytes 1024.0)
                                             len
                                             head-part
                                             tail-part
                                             temp-file)))
                (write-region (point-min) (point-max) temp-file nil 'no-message)
                formatted-msg))))))))

(defun my--get-diagnostics-for-file (path)
  "Retrieve active diagnostics/errors for the file at PATH.
Checks Flycheck, Flymake, and LSP (Eglot or lsp-mode) if available."
  (let ((buf (and path (find-buffer-visiting path))))
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
                          (output (with-current-buffer output-buffer
                                    (string-trim (buffer-string)))))
                      (kill-buffer output-buffer)
                      ;; Truncate output to avoid overloading model context (default 300 lines)
                      (funcall callback exit-code (my--truncate-output output (or max-lines 300)))))))))
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
         (full-path (expand-file-name (or path "./") root))
         (rel-path (file-relative-name full-path root)))
    (if (not (file-exists-p full-path))
        (funcall callback (format "Error: Path '%s' does not exist." rel-path))
      (let* ((depth-val (cond
                         ((and (integerp depth) (not (eq depth :json-false))) depth)
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
    (let* ((depth-arg (if (and (integerp depth) (not (eq depth :json-false))) (format "--max-depth %d" depth) ""))
           (ignore-file (expand-file-name "~/.config/git/ignore"))
           (ignore-arg (if (file-exists-p ignore-file) (format "--ignore-file %s" (shell-quote-argument ignore-file)) ""))
           (max-results (if (eq max-results :json-false) 50 (or max-results 50)))
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

(defun my--get-files-from-patch (patch)
  "Extract file paths from PATCH string."
  (let ((files nil)
        (lines (split-string patch "\n")))
    (dolist (line lines)
      (when (string-match "^\\+\\+\\+\\s-+\\(?:b/\\)?\\(.+\\)$" line)
        (let ((file (match-string 1 line)))
          (unless (equal file "/dev/null")
            (push file files)))))
    (delete-dups (nreverse files))))

(defun my/edit-tool (callback patch)
  "Apply PATCH using git apply and call CALLBACK with the result."
  (let* ((patch-content (if (string-suffix-p "\n" patch) patch (concat patch "\n")))
         (command '("git" "apply" "--verbose" "--recount" "--ignore-space-change" "--" "-"))
         (files (my--get-files-from-patch patch)))
    (my--async-exec
     "gptel-git-apply"
     (my--wrap-sandbox-command command)
     (lambda (code out)
       (if (not (zerop code))
           (funcall callback (format "Error applying patch (exit code %d):\n%s" code out))
         ;; Patch applied successfully. Wait 1.5 seconds for flycheck/LSP to update diagnostics.
         (run-with-timer
          1.5 nil
          (lambda ()
            (let ((diag-reports nil))
              (dolist (file files)
                (let ((diags (my--get-diagnostics-for-file file)))
                  (when diags
                    (push (format "File `%s` diagnostics:\n%s" file diags) diag-reports))))
              (if diag-reports
                  (funcall callback (format "%s\n\nWARNING: Post-patch compilation/linting errors found:\n%s"
                                            out
                                            (mapconcat #'identity (nreverse diag-reports) "\n\n")))
                (funcall callback out)))))))
     patch-content
     -1)))

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
     :description "Read the content of a file with line numbers in N:line format. To paginate over the file, use the `limit' and `offset' parameters in subsequent `cat' calls"
     :args ((:name "path" :type string :description "Path to the file to read.")
            (:name "offset" :type integer :description "Line number to start reading from (1-based). Default: 1." :optional t)
            (:name "limit" :type integer :description "Maximum number of lines to return. Default: 2000." :optional t)))
    (:name "edit"
     :function my/edit-tool
     :category "filesystem"
     :description "Apply a GNU unified diff format patch to one or more files. This is the primary tool for code modifications.
- Use relative paths from the project root in --- a/ and +++ b/ headers.
- Include @@ -start,len +start,len @@ hunk headers.
- Provide at least 3 lines of context (lines starting with ' ' or '+' or '-') around changes to ensure the patch applies cleanly.
- To create a new file, use --- /dev/null as the source.
- To delete a file, use +++ /dev/null as the destination.
- Multiple files can be patched in a single call by concatenating their diffs.
The tool automatically handles minor line count mismatches and whitespace variations."
     :async t
     :confirm t
     :args ((:name "patch" :type string :description "The unified diff content to apply.")))
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
             :description "The subset of your active tools to delegate to this sub-agent (e.g., [\"fd\", \"rg\", \"cat\"]). You can only delegate tools that are currently active in your own session."
             :items (:type string))))
    (:name "todo_write"
     :function my/gptel-write-todo
     :category "agent"
     :description "Display a formatted task list/todo list in the buffer. Use this to track plan execution progress.
Completed items are displayed with strikethrough and shadow face. Exactly one item should have status \"in_progress\"."
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
