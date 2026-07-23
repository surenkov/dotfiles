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
             (script "NR >= start && NR <= end { print NR \":\" $0 }\nEND {\n  if (NR == 0) {\n    if (start > 1) {\n      printf \"[Offset %d is beyond file length (0 lines)...]\\n\", start\n    }\n  } else if (start > NR) {\n    printf \"[Offset %d is beyond file length (%d lines)...]\\n\", start, NR\n  } else {\n    end_line = (end < NR) ? end : NR\n    if (start > 1 || end < NR) {\n      printf \"[... File has %d lines. Showing lines %d-%d ...]\\n\", NR, start, end_line\n    }\n  }\n}")
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

(defun my--edit-normalize-line-endings (text)
  "Normalize CRLF line endings to LF in TEXT."
  (replace-regexp-in-string "\r\n" "\n" text t t))

(defun my--edit-detect-line-ending (text)
  "Detect if TEXT uses CRLF (\"\\r\\n\") or LF (\"\\n\")."
  (if (string-match-p "\r\n" text) "\r\n" "\n"))

(defun my--edit-convert-to-line-ending (text ending)
  "Convert TEXT to ENDING line endings (\"\\n\" or \"\\r\\n\")."
  (let ((norm (my--edit-normalize-line-endings text)))
    (if (equal ending "\r\n")
        (replace-regexp-in-string "\n" "\r\n" norm t t)
      norm)))

(defun my--levenshtein (a b)
  "Compute Levenshtein distance between string A and string B."
  (let ((la (length a))
        (lb (length b)))
    (cond
     ((zerop la) lb)
     ((zerop lb) la)
     (t
      (let ((row (vconcat (number-sequence 0 lb))))
        (dotimes (i la)
          (let ((next-row (make-vector (1+ lb) 0))
                (char-a (aref a i)))
            (aset next-row 0 (1+ i))
            (dotimes (j lb)
              (let* ((char-b (aref b j))
                     (cost (if (= char-a char-b) 0 1))
                     (del (1+ (aref row (1+ j))))
                     (ins (1+ (aref next-row j)))
                     (sub (+ (aref row j) cost)))
                (aset next-row (1+ j) (min del ins sub))))
            (setq row next-row)))
        (aref row lb))))))

(defun my--edit-unescape-string (str)
  "Unescape standard escape sequences in STR."
  (let ((i 0)
        (len (length str))
        (res ""))
    (while (< i len)
      (if (and (= (aref str i) ?\\) (< (1+ i) len))
          (let ((next (aref str (1+ i))))
            (pcase next
              (?n (setq res (concat res "\n")))
              (?t (setq res (concat res "\t")))
              (?r (setq res (concat res "\r")))
              (?\' (setq res (concat res "'")))
              (?\" (setq res (concat res "\"")))
              (?\` (setq res (concat res "`")))
              (?\\ (setq res (concat res "\\")))
              (?$ (setq res (concat res "$")))
              (_ (setq res (concat res (string ?\\ next)))))
            (setq i (+ i 2)))
        (setq res (concat res (string (aref str i))))
        (setq i (1+ i))))
    res))

(defun my--edit-count-occurrences (content search)
  "Count occurrences of SEARCH string in CONTENT."
  (if (string-empty-p search)
      (1+ (length content))
    (let ((count 0)
          (pos 0)
          (slen (length search)))
      (while (setq pos (string-search search content pos))
        (setq count (1+ count))
        (setq pos (+ pos slen)))
      count)))

(defun my--edit-is-disproportionate-match (search old-string)
  "Check if SEARCH match span is disproportionately larger than OLD-STRING."
  (let ((old-lines (length (split-string old-string "\n")))
        (search-lines (length (split-string search "\n"))))
    (cond
     ((>= search-lines (max (+ old-lines 3) (* old-lines 2))) t)
     ((= old-lines 1) nil)
     (t (> (length (string-trim search))
           (max (+ (length (string-trim old-string)) 500)
                (* (length (string-trim old-string)) 4)))))))

(defun my--edit-remove-common-indentation (text)
  "Remove common leading whitespace from non-empty lines in TEXT."
  (let* ((lines (split-string text "\n"))
         (non-empty (cl-remove-if (lambda (l) (string-empty-p (string-trim l))) lines)))
    (if (null non-empty)
        text
      (let ((min-indent (apply #'min
                               (mapcar (lambda (l)
                                         (if (string-match "^\\([ \t]*\\)" l)
                                             (length (match-string 1 l))
                                           0))
                                       non-empty))))
        (mapconcat (lambda (l)
                     (if (string-empty-p (string-trim l))
                         l
                       (substring l (min (length l) min-indent))))
                   lines "\n")))))

(defun my--edit-get-substring-by-line-range (lines start-idx end-idx content)
  "Extract substring from CONTENT corresponding to line indices START-IDX to END-IDX in LINES."
  (let ((start-pos 0))
    (dotimes (k start-idx)
      (setq start-pos (+ start-pos (length (nth k lines)) 1)))
    (let ((end-pos start-pos))
      (cl-loop for k from start-idx to end-idx do
               (setq end-pos (+ end-pos (length (nth k lines)) (if (< k end-idx) 1 0))))
      (substring content start-pos (min (length content) end-pos)))))

(defun my--edit-find-candidates (content find)
  "Generate candidate substrings of CONTENT matching FIND using opencode's fallback replacers."
  (let ((candidates nil)
        (orig-lines (split-string content "\n"))
        (search-lines (split-string find "\n")))

    (cl-flet ((add-cand (c)
                (when (and (stringp c)
                           (not (string-empty-p c))
                           (string-search c content)
                           (not (member c candidates)))
                  (push c candidates))))

      ;; 1. SimpleReplacer (exact match)
      (when (string-search find content)
        (add-cand find))

      ;; 2. LineTrimmedReplacer
      (when (null candidates)
        (let ((s-lines (copy-sequence search-lines)))
          (when (and (cdr s-lines) (string-empty-p (car (last s-lines))))
            (setq s-lines (nbutlast s-lines)))
          (let ((s-len (length s-lines))
                (o-len (length orig-lines)))
            (cl-loop for i from 0 to (- o-len s-len) do
                     (let ((matched t))
                       (cl-loop for j from 0 below s-len do
                                (unless (equal (string-trim (nth (+ i j) orig-lines))
                                               (string-trim (nth j s-lines)))
                                  (setq matched nil)))
                       (when matched
                         (add-cand (my--edit-get-substring-by-line-range orig-lines i (+ i s-len -1) content))))))))

      ;; 3. BlockAnchorReplacer
      (when (null candidates)
        (let ((s-lines (copy-sequence search-lines)))
          (when (and (cdr s-lines) (string-empty-p (car (last s-lines))))
            (setq s-lines (nbutlast s-lines)))
          (when (>= (length s-lines) 3)
            (let* ((first-search (string-trim (car s-lines)))
                   (last-search (string-trim (car (last s-lines))))
                   (search-block-size (length s-lines))
                   (max-line-delta (max 1 (floor (* search-block-size 0.25))))
                   (o-len (length orig-lines))
                   (cands nil))
              (cl-loop for i from 0 below o-len do
                       (when (equal (string-trim (nth i orig-lines)) first-search)
                         (cl-loop for j from (+ i 2) below o-len do
                                  (when (equal (string-trim (nth j orig-lines)) last-search)
                                    (let ((actual-size (+ (- j i) 1)))
                                      (when (<= (abs (- actual-size search-block-size)) max-line-delta)
                                        (push (cons i j) cands)))
                                    (cl-return)))))
              (setq cands (nreverse cands))
              (cond
               ((= (length cands) 1)
                (let* ((cand (car cands))
                       (start-line (car cand))
                       (end-line (cdr cand))
                       (actual-size (+ (- end-line start-line) 1))
                       (lines-to-check (min (- search-block-size 2) (- actual-size 2)))
                       (similarity 0.0))
                  (if (> lines-to-check 0)
                      (progn
                        (cl-loop for j from 1 below (min (1- search-block-size) (1- actual-size)) do
                                 (let* ((orig-line (string-trim (nth (+ start-line j) orig-lines)))
                                        (search-line (string-trim (nth j s-lines)))
                                        (max-len (max (length orig-line) (length search-line))))
                                   (when (> max-len 0)
                                     (let ((dist (my--levenshtein orig-line search-line)))
                                       (setq similarity (+ similarity (/ (- 1.0 (/ (float dist) max-len)) lines-to-check)))))))
                        (when (>= similarity 0.65)
                          (add-cand (my--edit-get-substring-by-line-range orig-lines start-line end-line content))))
                    (add-cand (my--edit-get-substring-by-line-range orig-lines start-line end-line content)))))
               ((> (length cands) 1)
                (let ((best-match nil)
                      (max-sim -1.0))
                  (dolist (cand cands)
                    (let* ((start-line (car cand))
                           (end-line (cdr cand))
                           (actual-size (+ (- end-line start-line) 1))
                           (lines-to-check (min (- search-block-size 2) (- actual-size 2)))
                           (similarity 0.0))
                      (if (> lines-to-check 0)
                          (progn
                            (cl-loop for j from 1 below (min (1- search-block-size) (1- actual-size)) do
                                     (let* ((orig-line (string-trim (nth (+ start-line j) orig-lines)))
                                            (search-line (string-trim (nth j s-lines)))
                                            (max-len (max (length orig-line) (length search-line))))
                                       (when (> max-len 0)
                                         (let ((dist (my--levenshtein orig-line search-line)))
                                           (setq similarity (+ similarity (- 1.0 (/ (float dist) max-len))))))))
                            (setq similarity (/ similarity lines-to-check)))
                        (setq similarity 1.0))
                      (when (> similarity max-sim)
                        (setq max-sim similarity)
                        (setq best-match cand))))
                  (when (and best-match (>= max-sim 0.65))
                    (add-cand (my--edit-get-substring-by-line-range orig-lines (car best-match) (cdr best-match) content))))))))))

      ;; 4. WhitespaceNormalizedReplacer
      (when (null candidates)
        (let* ((norm-find (replace-regexp-in-string "[ \t\n\r]+" " " (string-trim find)))
               (o-len (length orig-lines))
               (s-len (length search-lines)))
          (cl-loop for i from 0 to (- o-len s-len) do
                   (let ((block (string-join (cl-subseq orig-lines i (+ i s-len)) "\n")))
                     (when (equal (replace-regexp-in-string "[ \t\n\r]+" " " (string-trim block)) norm-find)
                       (add-cand block))))))

      ;; 5. IndentationFlexibleReplacer
      (when (null candidates)
        (let* ((norm-find (my--edit-remove-common-indentation find))
               (o-len (length orig-lines))
               (s-len (length search-lines)))
          (cl-loop for i from 0 to (- o-len s-len) do
                   (let ((block (string-join (cl-subseq orig-lines i (+ i s-len)) "\n")))
                     (when (equal (my--edit-remove-common-indentation block) norm-find)
                       (add-cand block))))))

      ;; 6. EscapeNormalizedReplacer
      (when (null candidates)
        (let ((unescaped-find (my--edit-unescape-string find)))
          (if (string-search unescaped-find content)
              (add-cand unescaped-find)
            (let ((unescaped-lines (split-string unescaped-find "\n"))
                  (o-len (length orig-lines)))
              (let ((s-len (length unescaped-lines)))
                (cl-loop for i from 0 to (- o-len s-len) do
                         (let ((block (string-join (cl-subseq orig-lines i (+ i s-len)) "\n")))
                           (when (equal (my--edit-unescape-string block) unescaped-find)
                             (add-cand block)))))))))

      ;; 7. TrimmedBoundaryReplacer
      (when (null candidates)
        (let ((trimmed-find (string-trim find)))
          (unless (equal trimmed-find find)
            (if (string-search trimmed-find content)
                (add-cand trimmed-find)
              (let ((t-lines (split-string trimmed-find "\n"))
                    (o-len (length orig-lines)))
                (let ((s-len (length t-lines)))
                  (cl-loop for i from 0 to (- o-len s-len) do
                           (let ((block (string-join (cl-subseq orig-lines i (+ i s-len)) "\n")))
                             (when (equal (string-trim block) trimmed-find)
                               (add-cand block))))))))))

      ;; 8. ContextAwareReplacer
      (when (null candidates)
        (let ((s-lines (copy-sequence search-lines)))
          (when (and (cdr s-lines) (string-empty-p (car (last s-lines))))
            (setq s-lines (nbutlast s-lines)))
          (when (>= (length s-lines) 3)
            (let* ((first-line (string-trim (car s-lines)))
                   (last-line (string-trim (car (last s-lines))))
                   (s-len (length s-lines))
                   (o-len (length orig-lines)))
              (cl-loop for i from 0 to (- o-len s-len) do
                       (when (equal (string-trim (nth i orig-lines)) first-line)
                         (let ((j (+ i s-len -1)))
                           (when (equal (string-trim (nth j orig-lines)) last-line)
                             (let ((block-lines (cl-subseq orig-lines i (1+ j)))
                                   (matching 0)
                                   (total 0))
                               (cl-loop for k from 1 below (1- s-len) do
                                        (let ((bl (string-trim (nth k block-lines)))
                                              (fl (string-trim (nth k s-lines))))
                                          (when (or (> (length bl) 0) (> (length fl) 0))
                                            (setq total (1+ total))
                                            (when (equal bl fl)
                                              (setq matching (1+ matching))))))
                               (when (or (= total 0) (>= (/ (float matching) total) 0.5))
                                 (add-cand (string-join block-lines "\n"))
                                 (cl-return)))))))))))

      ;; 9. MultiOccurrenceReplacer
      (when (null candidates)
        (let ((pos 0)
              (slen (length find)))
          (while (and (> slen 0) (setq pos (string-search find content pos)))
            (add-cand find)
            (setq pos (+ pos slen)))))

      (nreverse candidates))))

(cl-defun my--edit-replace (content old-string new-string replace-all)
  "Replace OLD-STRING with NEW-STRING in CONTENT.
If REPLACE-ALL is non-nil, replace all matches.
Returns updated content string, or signals an error if replacement fails."
  (when (equal old-string new-string)
    (error "No changes to apply: oldString and newString are identical."))
  (when (string-empty-p old-string)
    (error "oldString cannot be empty when editing an existing file. Provide the exact text to replace, or use write for an intentional full-file replacement."))

  (let ((candidates (my--edit-find-candidates content old-string))
        (found-multiple nil))
    (unless candidates
      (error "Could not find oldString in the file. It must match exactly, including whitespace, indentation, and line endings."))
    (dolist (search candidates)
      (when (my--edit-is-disproportionate-match search old-string)
        (error "Refusing replacement because the matched span is much larger than oldString. Re-read the file and provide the full exact oldString for the intended replacement."))
      (if replace-all
          (cl-return-from my--edit-replace
            (replace-regexp-in-string (regexp-quote search) new-string content t t))
        (let ((first-pos (string-search search content))
              (last-pos (let ((p nil) (pos 0) (slen (length search)))
                          (while (setq pos (string-search search content pos))
                            (setq p pos)
                            (setq pos (+ pos slen)))
                          p)))
          (if (equal first-pos last-pos)
              (cl-return-from my--edit-replace
                (concat (substring content 0 first-pos)
                        new-string
                        (substring content (+ first-pos (length search)))))
            (setq found-multiple t)))))
    (if found-multiple
        (error "Found multiple matches for oldString. Provide more surrounding context to make the match unique.")
      (error "Could not find oldString in the file. It must match exactly, including whitespace, indentation, and line endings."))))

(cl-defun my/edit-tool (callback path old-string new-string &optional replace-all &rest kwargs)
  "Perform string replacement on file at PATH replacing OLD-STRING with NEW-STRING.
If REPLACE-ALL is non-nil, replace all occurrences.
CALLBACK is called with the result string."
  (let* ((actual-path (or (and (stringp path) (not (string-empty-p path)) path)
                          (plist-get kwargs :filePath)
                          (plist-get kwargs :path)))
         (actual-replace-all (if (eq replace-all :json-false) nil (or replace-all (plist-get kwargs :replaceAll)))))
    (unless actual-path
      (funcall callback "Error: path is required")
      (cl-return-from my/edit-tool nil))
    (when (equal old-string new-string)
      (funcall callback "Error: No changes to apply: oldString and newString are identical.")
      (cl-return-from my/edit-tool nil))

    (let* ((root (doom-project-root))
           (file-path (expand-file-name actual-path root))
           (rel-path (file-relative-name file-path root)))
      (condition-case err
          (if (string-empty-p old-string)
              ;; Creating new file or erroring if file exists
              (if (file-exists-p file-path)
                  (funcall callback "Error: oldString cannot be empty when editing an existing file. Provide the exact text to replace, or use write for an intentional full-file replacement.")
                (make-directory (file-name-directory file-path) t)
                (with-temp-file file-path
                  (insert new-string))
                ;; Revert visiting buffer if any
                (let ((buf (find-buffer-visiting file-path)))
                  (when (buffer-live-p buf)
                    (with-current-buffer buf
                      (revert-buffer t t t))))
                (let ((diags (my--get-diagnostics-for-file file-path root))
                      (msg (format "Edited file successfully: %s\nReplacements: 1"
                                   rel-path)))
                  (if diags
                      (funcall callback (format "%s\n\nWARNING: Post-patch compilation/linting errors found:\nFile `%s` diagnostics:\n%s"
                                                msg rel-path diags))
                    (funcall callback msg))))

            ;; Existing file modification
            (unless (file-exists-p file-path)
              (error "File %s not found" file-path))
            (when (file-directory-p file-path)
              (error "Path is a directory, not a file: %s" file-path))

            (let* ((content-old (with-temp-buffer
                                  (insert-file-contents-literally file-path)
                                  (buffer-string)))
                   (ending (my--edit-detect-line-ending content-old))
                   (old (my--edit-convert-to-line-ending old-string ending))
                   (replacement (my--edit-convert-to-line-ending new-string ending))
                   (content-new (my--edit-replace content-old old replacement actual-replace-all)))

              (with-temp-buffer
                (insert content-new)
                (let ((coding-system-for-write 'no-conversion))
                  (write-region (point-min) (point-max) file-path nil 'silent)))

              ;; Update visiting buffer if open
              (let ((buf (find-buffer-visiting file-path)))
                (when (buffer-live-p buf)
                  (with-current-buffer buf
                    (revert-buffer t t t))))

              (let ((diags (my--get-diagnostics-for-file file-path root))
                    (msg (format "Edited file successfully: %s\nReplacements: %d"
                                 rel-path
                                 (my--edit-count-occurrences content-old old))))
                (if diags
                    (funcall callback (format "%s\n\nWARNING: Post-patch compilation/linting errors found:\nFile `%s` diagnostics:\n%s"
                                              msg rel-path diags))
                  (funcall callback msg)))))

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

(defun my/ast-grep-tool (callback command &optional path pattern language inline-rules extra-args)
  "Run ast-grep with the specified COMMAND and arguments asynchronously.
CALLBACK is called with the result string.
COMMAND is `run', `scan', or `outline'.
PATH is the relative target path (defaults to project root).
PATTERN is the pattern string to match.
LANGUAGE is the language of the codebase.
INLINE-RULES is the inline YAML rules configuration for `scan'.
EXTRA-ARGS is an optional list of extra string flags."
  (let* ((ast-grep-bin (executable-find "ast-grep")))
    (if (not ast-grep-bin)
        (funcall callback "Error: 'ast-grep' CLI tool is not installed or not found on PATH.")
      (let* ((root (doom-project-root))
             (target-path (expand-file-name (or path ".") root))
             (rel-target (file-relative-name target-path root)))
        (let* ((cmd-args (list "ast-grep" command))
               ;; Formulate parameters based on command
               (cmd-args
                (pcase command
                  ("run"
                   (append cmd-args
                           (when (and pattern (not (string-empty-p pattern)))
                             (list "--pattern" pattern))
                           (when (and language (not (string-empty-p language)))
                             (list "--lang" language))
                           (list rel-target)))
                  ("scan"
                   (append cmd-args
                           (when (and inline-rules (not (string-empty-p inline-rules)))
                             (list "--inline-rules" inline-rules))
                           (list rel-target)))
                  ("outline"
                   (append cmd-args
                           (when (and pattern (not (string-empty-p pattern)))
                             (list "--match" pattern))
                           (list rel-target)))
                  (_ (append cmd-args (list rel-target)))))
               ;; Append extra-args if any
               (cmd-args (append cmd-args (and extra-args (append extra-args nil)))))
          (my--async-exec
           "gptel-ast-grep"
           (my--wrap-sandbox-command cmd-args)
           (lambda (code out)
             (cond
              ((not (zerop code))
               (funcall callback (format "ast-grep failed (exit code %d):\n%s" code out)))
              ((string-empty-p out)
               (funcall callback "No matches/structure found."))
              (t
               (funcall callback out))))))))))

(defconst my/gptel-custom-tools
  '((:name "skill"
     :function my/skill-tool
     :category "agent"
     :description "Retrieve detailed instructions and guidelines for a specific development methodology skill."
     :args ((:name "name" :type string :description "The name of the skill to retrieve instructions for.")))
    (:name "ast-grep"
     :function my/ast-grep-tool
     :category "filesystem"
     :description "Search, scan, or outline codebase structure using ast-grep."
     :async t
     :args ((:name "command" :type string :description "The ast-grep subcommand to run: 'run' (for pattern-based search), 'scan' (for rule-based scanning), or 'outline' (for structural code maps)." :enum ["run" "scan" "outline"])
            (:name "path" :type string :description "Path relative to the project root to perform the search or outline. Defaults to project root '.'." :optional t)
            (:name "pattern" :type string :description "The ast-grep query pattern to search for (e.g. 'console.log($ARG)'). Used with 'run' or for filtering in 'outline'." :optional t)
            (:name "language" :type string :description "The language of the code to search (e.g. 'python', 'go', 'rust', 'typescript'). Used with 'run'." :optional t)
            (:name "inline-rules" :type string :description "An inline YAML rule string for complex structural queries. Used with 'scan'." :optional t)
            (:name "extra-args" :type array :description "Optional list of extra command-line arguments to pass directly to the subcommand (e.g. [\"--items\", \"exports\", \"--view\", \"expanded\"] for outline)." :items (:type string) :optional t)))
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
     :async t
     :args ((:name "path" :type string :description "Path to the file to read.")
            (:name "offset" :type integer :description "Line number to start reading from (1-based). Default: 1." :optional t)
            (:name "limit" :type integer :description "Maximum number of lines to return. Default: 2000." :optional t)))
    (:name "edit"
     :function my/edit-tool
     :category "filesystem"
     :description "Performs string replacements in a file using oldString and newString.
Usage:
- oldString: Exact text to replace.
- newString: Replacement text (must differ from oldString).
- replaceAll: Optional boolean to replace all occurrences of oldString (default false).
- path: Relative path from project root or absolute path of file to modify.
If oldString is empty (\"\") and the file does not exist, a new file will be created with newString.
If oldString is empty (\"\") and the file exists, the tool will return an error."
     :async t
     :confirm t
     :args ((:name "path" :type string :description "Path to the file to modify.")
            (:name "oldString" :type string :description "The exact text to replace.")
            (:name "newString" :type string :description "The text to replace it with.")
            (:name "replaceAll" :type boolean :description "Replace all occurrences of oldString (default false)." :optional t)))
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
