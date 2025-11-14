;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Savva Surenkov"
      user-mail-address "savva@surenkov.space")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-nord
      doom-font (font-spec :family "Iosevka Slab" :size 14)
      doom-big-font (font-spec :family "Iosevka Slab" :size 16)
      doom-variable-pitch-font (font-spec :family "Iosevka Slab" :size 14)
      doom-themes-enable-italic t
      doom-themes-padded-modeline t

      confirm-kill-processes nil
      kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions)

      truncate-string-ellipsis "…"
      scroll-margin 3
      evil-want-fine-undo t
      undo-limit 8000000
      shell-file-name (executable-find "bash")

      display-line-numbers-type 'relative
      transient-show-during-minibuffer-read t

      show-trailing-whitespace t
      select-enable-clipboard t
      select-enable-primary t
      require-final-newline t
      next-line-add-newlines nil

      remote-file-name-inhibit-locks t
      tramp-use-scp-direct-remote-copying t
      remote-file-name-inhibit-auto-save-visited t
      magit-tramp-pipe-stty-settings 'pty
      vc-handled-backends '(Git)

      projectile-project-search-path '("~/Projects/")
      projectile-indexing-method 'hybrid
      org-directory "~/org/"

      ls-lisp-dirs-first t
      dired-listing-switches "-aBhl --group-directories-first" ; requires ls from 'coreutils' on macOS
      dired-dwim-target t)

(cond ((featurep :system 'macos) ;; mac specific settings
       (setq insert-directory-program "/opt/homebrew/bin/gls")
       (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
       (add-to-list 'default-frame-alist '(ns-appearance . dark))))

(connection-local-set-profile-variables
 'remote-direct-async-process
 '((tramp-direct-async-process . t)))

(connection-local-set-profiles
 '(:application tramp :protocol "scp")
 'remote-direct-async-process)

(with-eval-after-load 'tramp
  (with-eval-after-load 'compile
    (remove-hook 'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options)))

(remove-hook 'evil-insert-state-exit-hook #'doom-modeline-update-buffer-file-name)
(remove-hook 'find-file-hook #'doom-modeline-update-buffer-file-name)
(remove-hook 'find-file-hook 'forge-bug-reference-setup)

(unless (display-graphic-p)
  (xterm-mouse-mode 1))

(global-subword-mode 1)                 ; Iterate through CamelCase words

(setq-default cursor-type 'hollow
              vterm-shell (executable-find "fish")
              explicit-shell-file-name (executable-find "fish"))

(add-hook! (prog-mode conf-mode text-mode) #'display-fill-column-indicator-mode)

(after! corfu
  (setq corfu-preview-current 'insert
        corfu-auto-delay 0.05))

(setq lsp-disabled-clients '(flow-ls jsts-ls xmlls semgrep-ls pyright zuban-ls)
      gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      lsp-idle-delay 0.2
      lsp-diagnostics-provider :auto
      lsp-warn-no-matched-clients nil
      lsp-ruff-ruff-args '("--preview")
      lsp-log-io nil
      lsp-idle-delay 0.500
      lsp-use-plists t
      lsp-enable-file-watchers nil
      lsp-restart 'auto-restart
      lsp-enable-dap-auto-configure t)

(after! lsp
  (lsp-defcustom lsp-ty-experimental-rename t
    "Enable the experimental support for renaming symbols in the editor."
    :type 'boolean
    :group 'ty-ls
    :lsp-path "ty.experimental.rename")
  (lsp-defcustom lsp-ty-experimental-auto-import t
    "Enable the experimental support for auto-import code completions."
    :type 'boolean
    :group 'ty-ls
    :lsp-path "ty.experimental.autoImport")
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("zubanls"))
    :activation-fn (lsp-activate-on "python")
    :server-id 'zuban-ls
    :priority -3
    :add-on? t)))

(after! project
  (add-to-list 'project-vc-ignores "./.venv/"))

(after! agent-shell
  (setq agent-shell-google-authentication
        (agent-shell-google-make-authentication :login t)))

(after! gptel
  (require 'gptel-integrations)

  ;; DEFINE: LLMs
  (gptel-make-ollama "Ollama"
    :host "localhost:11434"
    :stream t
    :models '("gemma3:12b"
              "gemma3:27b"
              "gemma3n:latest"
              "gpt-oss:20b"
              "qwen3:30b-a3b"
              "qwen3-coder:30b"
              "deepseek-r1:32b"))
  (gptel-make-anthropic "Claude"
    :stream t
    :key (getenv "ANTHROPIC_API_KEY"))
  (gptel-make-anthropic "Claude-Thinking"
    :stream t
    :key (getenv "ANTHROPIC_API_KEY")
    :models '((claude-opus-4-1
               :description "High-performance model with exceptional reasoning and efficiency"
               :capabilities (media tool-use cache)
               :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
               :context-window 200
               :input-cost 3
               :output-cost 15
               :cutoff-date "2025-03")
              (claude-opus-4-0
               :description "Most capable model for complex reasoning and advanced coding"
               :capabilities (media tool-use cache)
               :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
               :context-window 200
               :input-cost 15
               :output-cost 75
               :cutoff-date "2025-03"))
    :request-params '(:thinking (:type "enabled" :budget_tokens 16384))
    :header (lambda () (when-let* ((key (gptel--get-api-key)))
                         `(("x-api-key" . ,key)
                           ("anthropic-version" . "2023-06-01")
                           ("anthropic-beta" . "pdfs-2024-09-25")
                           ("anthropic-beta" . "prompt-caching-2024-07-31")))))

  ;; DEFINE: Tools
  (defun my/rg-tool (search-pattern &optional glob path max-results)
    "Search for PATTERN in files in PATH using ripgrep."
    (or max-results (setq max-results 200))
    (let* ((root (doom-project-root))
           ;; Resolve path relative to project root, handling ~ expansion
           (default-directory (if path (expand-file-name path root) root))
           (glob-arg (if glob (format "-g %s" (shell-quote-argument glob)) ""))
           (max-lines-arg (if (>= max-results 0)
                              (format " | head -n %s"
                                      (shell-quote-argument (number-to-string max-results)))
                            ""))
           ;; Run rg in the default-directory. Use "." to search current directory.
           (command (format "rg --ignore-file ~/.config/git/ignore --color=never %s -n -- %s . %s"
                            glob-arg
                            (shell-quote-argument search-pattern)
                            max-lines-arg))
           (result (shell-command-to-string command)))
      (if (string-empty-p result)
          "No matches found"
        result)))

  (defun my/fzf-tool (pattern &optional exact path)
    "Fuzzy search for file names matching PATTERN in PATH using fzf."
    (let* ((root (doom-project-root))
           ;; Resolve path relative to project root, handling ~ expansion
           (default-directory (if path (expand-file-name path root) root))
           ;; Run rg in the default-directory. No path argument needed.
           (command (format "rg --ignore-file ~/.config/git/ignore --files | fzf --ansi --filter=%s %s"
                            (shell-quote-argument pattern)
                            (if exact "--exact" "")))
           (result (shell-command-to-string command)))
      (if (string-empty-p result)
          "No matches found"
        result)))

  (defun my/cat-file-tool (path &optional offset limit)
    "Read the contents of file at PATH and return it as a string.
OFFSET is the starting line number (1-based).
LIMIT is the maximum number of lines to return."
    (let ((default-directory (doom-project-root))
          (offset (or offset 1))
          (limit (or limit 2000)))
      (if (file-readable-p path)
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
                    content)))))
        (format "Error: File '%s' is not readable or does not exist." path))))

  (defun my/list-buffers-tool ()
    "List open Emacs buffers."
    (let ((buffers (cl-remove-if-not #'buffer-file-name (buffer-list))))
      (mapconcat #'buffer-name buffers "\n")))

  (defun my/read-buffer-tool (buffer-name &optional offset limit)
    "Return content of BUFFER-NAME with optional OFFSET and LIMIT.
OFFSET is the starting line number (1-based).
LIMIT is the maximum number of lines to return."
    (if-let ((buffer (get-buffer buffer-name)))
        (with-current-buffer buffer
          (save-excursion
            (goto-char (point-min))
            (when (and offset (numberp offset) (> offset 0))
              (forward-line (1- offset)))
            (let ((start (point)))
              (if (and limit (numberp limit) (> limit 0))
                  (progn
                    (forward-line limit)
                    (buffer-substring-no-properties start (point)))
                (buffer-substring-no-properties start (point-max))))))
      (format "Error: Buffer '%s' does not exist." buffer-name)))

  (defun my/apply-patch-tool (patch)
    "Apply a git-formatted PATCH string using `git apply'.
The patch is applied relative to the project root."
    (let ((default-directory (doom-project-root)))
      (with-temp-buffer
        (insert (if (string-suffix-p hard-newline patch) patch (concat patch hard-newline)))
        (let ((output-buffer (generate-new-buffer "*git-apply-output*")))
          (unwind-protect
              (condition-case err
                  ;; Use "-" to read patch from stdin.
                  (let ((exit-code (call-process-region (point-min) (point-max) "git" nil output-buffer t "apply" "--" "-")))
                    (with-current-buffer output-buffer
                      (if (zerop exit-code)
                          "Patch applied successfully."
                        (format "Error applying patch (exit code %d):\n%s" exit-code (buffer-string)))))
                (format "Error executing 'git apply': %s" (error-message-string err)))
            (kill-buffer output-buffer))))))

  (dolist (custom-tool-plist
           '((:name "rg"
              :function my/rg-tool
              :category "filesystem"
              :description "Search file content using regex with ripgrep"
              :args ((:name "search-pattern" :type string :description "Regex pattern to search in file contents")
                     (:name "glob" :type string :description "Glob pattern for files to include in search (e.g., \"*.py\")" :optional t)
                     (:name "path" :type string :description "Directory to search in. Default is a project root" :optional t)
                     (:name "max-results" :type integer :description "Output only the first `max-results'. Default: 200" :optional t)))
             (:name "fzf"
              :function my/fzf-tool
              :category "filesystem"
              :description "Fuzzy search for file names using fzf"
              :args ((:name "pattern" :type string :description "Fuzzy search pattern for file names")
                     (:name "exact" :type boolean :description "Enable exact-match" :optional t)
                     (:name "path" :type string :description "Directory to search in. Default is a project root" :optional t)))
             (:name "cat"
              :function my/cat-file-tool
              :category "filesystem"
              :description "Read the content of a file and return it as a string."
              :args ((:name "path" :type string :description "Path to the file to read.")
                     (:name "offset" :type integer :description "Line number to start reading from (1-based). Default: 1." :optional t)
                     (:name "limit" :type integer :description "Maximum number of lines to return. Default: 2000." :optional t)))
             (:name "list_buffers"
              :function my/list-buffers-tool
              :category "emacs"
              :description "Lists currently open buffers in Emacs.")
             (:name "read_buffer"
              :function my/read-buffer-tool
              :category "emacs"
              :description "Return content of BUFFER-NAME with optional OFFSET and LIMIT. OFFSET is the starting line number (1-based). LIMIT is the maximum number of lines to return."
              :args ((:name "buffer-name" :type string :description "The name of the buffer to view.")
                     (:name "offset" :type integer :description "Line number to start reading from (1-based)." :optional t)
                     (:name "limit" :type integer :description "Maximum number of lines to return." :optional t)))
             (:name "apply_patch"
              :function my/apply-patch-tool
              :category "filesystem"
              :description "Apply a git-formatted PATCH string using `git apply'. The patch is applied relative to the project root."
              :args ((:name "patch" :type string :description "A git-formatted patch string to apply.")))))
    (apply #'gptel-make-tool custom-tool-plist))

  (use-package! mcp
    :config (require 'mcp-hub)
    :custom (mcp-hub-servers
             `(("git" . (:command "uvx" :args ("mcp-server-git")))
               ("atlassian" . (:url "https://mcp.atlassian.com/v1/sse"))
               ("context7" . (:command "bunx" :args ("--bun" "@upstash/context7-mcp"))))))

  ;; DEFINE: Prompts
  (defun gptel-prompts-current-project-variables (_file)
    `(("project_root" . ,(doom-project-root))
      ("current_directory" . ,default-directory)))

  (use-package! gptel-prompts
    :demand t
    :config
    (setq gptel-prompts-directory (concat doom-user-dir "prompts/")
          gptel-prompts-template-functions '(gptel-prompts-add-current-time
                                             gptel-prompts-current-project-variables))
    (gptel-prompts-update))

  (defun my/gptel-remove-headings (beg end)
    (when (derived-mode-p 'org-mode)
      (save-excursion
        (goto-char beg)
        (while (re-search-forward org-heading-regexp end t)
          (forward-line 0)
          (delete-char (1+ (length (match-string 1))))
          (insert-and-inherit "*")
          (end-of-line)
          (skip-chars-backward " \t\r")
          (insert-and-inherit "*")))))
  (add-hook! 'gptel-post-response-functions #'my/gptel-remove-headings)

  (defun my/gptel-init-coding-mcp ()
    (gptel-mcp-connect '("dash")))

  ;; DEFINE: Presets
  (gptel-make-preset 'default
    :description "Default preset"
    :backend "Gemini"
    :system (alist-get 'default gptel-directives)
    :model 'gemini-pro-latest
    :tools nil)
  (gptel-make-preset 'code-analysis
    :description "A preset optimized for read-only coding tasks"
    :backend "Gemini"
    :system (alist-get 'code-analysis gptel-directives)
    :model 'gemini-pro-latest
    ;; :post #'my/gptel-init-coding-mcp
    :tools '("fzf" "rg" "cat" "list_buffers" "read_buffer"))
  (gptel-make-preset 'programming
    :description "A preset optimized for coding tasks"
    :parents 'code-analysis
    :system (alist-get 'programming gptel-directives)
    :tools '("fzf" "rg" "cat" "list_buffers" "read_buffer" "apply_patch"))
  (gptel-make-preset 'gemini-grounded
    :description "Gemini with Google Search grounding"
    :parents 'default
    :backend (gptel-make-gemini "Gemini-Grounded"
               :stream t
               :request-params '(:tools [(:google_search ())])
               :key (getenv "GOOGLE_GENAI_API_KEY")))

  ;; DEFINE: Config

  (set-popup-rule!
    (lambda (bname _action)
      (buffer-local-value 'gptel-mode (get-buffer bname)))
    :select t
    :side 'right
    :size 0.4
    :quit 'other
    :ttl nil)

  (setq! gptel-api-key (getenv "OPENAI_API_KEY")
         gptel-display-buffer-action nil
         gptel-expert-commands t
         gptel-use-tools t
         gptel-include-tool-results t
         gptel-track-media t
         gptel-max-tokens 32000
         gptel--preset 'default
         gptel-backend (gptel-make-gemini "Gemini"
                         :stream t
                         :key (getenv "GOOGLE_GENAI_API_KEY"))
         gptel-model 'gemini-pro-latest
         gptel-default-mode 'org-mode
         gptel-log-level 'info
         gptel-use-context 'user))

(use-package! hydra
  :defer
  :config
  (defhydra hydra/evil-window-resize (:color red)
    "Resize window"
    ("h" evil-window-decrease-width "decrease width")
    ("j" evil-window-decrease-height "decrease height")
    ("k" evil-window-increase-height "increase height")
    ("l" evil-window-increase-width "increase width")
    ("q" nil "quit")))
(map! :leader
      :prefix ("w" . "window")
      :n "r" #'hydra/evil-window-resize/body)

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(map! :n "C-i"      #'evil-jump-forward
      :n "C-o"      #'evil-jump-backward
      :n "SPC f t"  #'treemacs
      :n "g r"      #'+lookup/references
      :n "g i"      #'+lookup/implementations
      :n "g D"      #'+lookup/type-definition)

(map! :leader
      :prefix ("c" . "code")
      :desc "Search LSP Symbols in buffer" "F" #'consult-lsp-file-symbols)

(map! :after transient
      :map transient-map
      [escape] #'transient-quit-one)

(after! gptel
  (map! :mode gptel-mode
        :n "RET"   #'gptel-send
        :n "C-c C-k" #'gptel-abort)
  (map! :mode gptel-context-buffer-mode
        :n "Z Z" #'gptel-context-confirm
        :n "Z Q" #'gptel-context-quit
        :n "RET"     #'gptel-context-visit
        :n "n"       #'gptel-context-next
        :n "p"       #'gptel-context-previous
        :n "d"       #'gptel-context-flag-deletion))

(custom-set-faces!
  '(gptel-context-highlight-face :background "#2C333F")
  '(line-number :italic nil)
  '(line-number-current-line :italic nil)
  '(ts-fold-replacement-face :foreground unspecified :box nil :inherit font-lock-comment-face :weight light))

(if (display-graphic-p)
    (progn
      (custom-set-faces! '(default :background "#000000"))
      (custom-theme-set-faces! 'doom-plain-dark '(default :background "#000000"))
      (custom-theme-set-faces! 'doom-nord '(default :background "#000000")))
  (progn
    (custom-set-faces! '(default :background unspecified))
    (custom-set-faces! '(ein:basecell-input-area-face :background unspecified))
    (custom-theme-set-faces! 'doom-plain-dark '(default :background unspecified))
    (custom-theme-set-faces! 'doom-nord '(default :background unspecified))))
