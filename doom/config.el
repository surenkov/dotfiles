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
      ;; evil-want-fine-undo t
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
    :request-params '(:options (:num_ctx 32000))
    :models '("gemma3n:latest"
              "gpt-oss:latest"
              "qwen3-coder:latest"))
  (gptel-make-anthropic "Claude"
    :stream t
    :key (getenv "ANTHROPIC_API_KEY")
    :request-params `(:thinking (:type "enabled" :budget_tokens 10000)
                      :context_management (:edits [(:type "clear_thinking_20251015")
                                                   (:type "clear_tool_uses_20250919")]))
    :header (lambda () (when-let* ((key (getenv "ANTHROPIC_API_KEY")))
                         `(("x-api-key" . ,key)
                           ("anthropic-version" . "2023-06-01")
                           ("anthropic-beta" . "extended-cache-ttl-2025-04-11")
                           ("anthropic-beta" . "context-management-2025-06-27")))))

  ;; DEFINE: Tools
  (defun my/list-files-tool (path &optional recursive)
    "List files in PATH using fd. Respects .gitignore."
    (let* ((default-directory (doom-project-root))
           (full-path (expand-file-name (or path ".")))
           (rel-path (file-relative-name full-path default-directory)))
      (if (not (string-prefix-p (string-trim-right default-directory "/") full-path))
          (format "Error: Access denied. %s is outside project root." path)
        (if (not (file-exists-p full-path))
            (format "Error: Path '%s' does not exist." path)
          (shell-command-to-string
           (format "fd --color=never %s . %s"
                   (if (or (null recursive) (eq recursive :json-false)) "--max-depth 1" "")
                   (shell-quote-argument rel-path)))))))

  (defun my/rg-tool (search-pattern &optional glob path max-results context)
    "Search for PATTERN in files in PATH using ripgrep."
    (let* ((default-directory (doom-project-root))
           (ignore-file (expand-file-name "~/.config/git/ignore"))
           (base-flags "--color=never --max-columns=500 --max-columns-preview --with-filename --no-heading -n")
           (ignore-arg (if (file-exists-p ignore-file)
                           (format "--ignore-file %s" (shell-quote-argument ignore-file))
                         ""))
           (path-arg (if (and path (not (string-empty-p path)))
                         (shell-quote-argument path)
                       ""))
           (glob-arg (if glob (format "-g %s" (shell-quote-argument glob)) ""))
           (context-arg (if context (format "-C %d" context) ""))
           (max-results (or max-results 50))
           (max-lines-arg (if (>= max-results 0)
                              (format "| head -n %s" (number-to-string max-results))
                            ""))
           (command (format "rg %s %s %s %s -- %s %s %s"
                            base-flags
                            ignore-arg
                            glob-arg
                            context-arg
                            (shell-quote-argument search-pattern)
                            path-arg
                            max-lines-arg))
           (result (shell-command-to-string command)))
      (if (string-empty-p result) "No matches found" result)))

  (defun my/fzf-tool (pattern &optional exact path depth max-results)
    "Fuzzy search for file names matching PATTERN in PATH using fzf."
    (let* ((root (doom-project-root))
           (default-directory (if path (expand-file-name path root) root))
           (depth-arg (if (and depth (integerp depth)) (format "-d %d" depth) ""))
           (max-results (or max-results 50))
           (ignore-file (expand-file-name "~/.config/git/ignore"))
           (ignore-arg (if (file-exists-p ignore-file)
                           (format "--ignore-file %s" (shell-quote-argument ignore-file))
                         ""))
           (max-lines-arg (if (>= max-results 0)
                              (format " | head -n %s"
                                      (shell-quote-argument (number-to-string max-results)))
                            ""))
           (command (format "rg %s --files %s | fzf --ansi --filter=%s %s %s"
                            ignore-arg
                            depth-arg
                            (shell-quote-argument pattern)
                            (if exact "--exact" "")
                            max-lines-arg))
           (result (shell-command-to-string command)))
      (if (string-empty-p result) "No matches found" result)))

  (defun my/cat-file-tool (path &optional offset limit)
    "Read the contents of file at PATH and return it as a string."
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
                  (let ((exit-code (call-process-region
                                    (point-min) (point-max)
                                    "git" nil output-buffer t
                                    "apply"
                                    "--verbose"
                                    "--recount"
                                    "--ignore-space-change"
                                    "--" "-")))
                    (with-current-buffer output-buffer
                      (if (zerop exit-code)
                          "Patch applied successfully."
                        (format "Error applying patch (exit code %d):\n%s" exit-code (buffer-string)))))
                (format "Error executing 'git apply': %s" (error-message-string err)))
            (kill-buffer output-buffer))))))

  (defun my/read-url-tool (url)
    "Fetch content from a URL."
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (re-search-forward "^$" nil 'move)
      (forward-char)
      (let ((content (buffer-substring-no-properties (point) (point-max))))
        ;; Simple cleanup to reduce token usage
        (with-temp-buffer
          (insert content)
          (shr-render-region (point-min) (point-max))
          (buffer-string)))))

  (defun my/run-command-tool (command)
    "Run a shell command in the project root and return output."
    (let ((default-directory (doom-project-root)))
      (shell-command-to-string command)))

  (dolist (custom-tool-plist
           '((:name "fd"
              :function my/list-files-tool
              :category "filesystem"
              :description "List directories in a path using `fd'."
              :args ((:name "path" :type string :description "Directory path relative to project root.")
                     (:name "recursive" :type boolean :description "List subdirectories recursively, if argument is present. Default is false" :optional t)))
             (:name "rg"
              :function my/rg-tool
              :category "filesystem"
              :description "Search file content using regex with ripgrep"
              :args ((:name "search-pattern" :type string :description "Regex pattern to search in file contents")
                     (:name "glob" :type string :description "Glob pattern for files to include in search (e.g., \"*.py\")" :optional t)
                     (:name "path" :type string :description "Path to a file or a directory to search in. Default is a project root" :optional t)
                     (:name "context" :type integer :description "Show NUM lines before and after each match. Default is to show only a matching line." :optional t)
                     (:name "max-results" :type integer :description "Output only the first `max-results'. Default: 50. Set to -1 for no limit." :optional t)))
             (:name "fzf"
              :function my/fzf-tool
              :category "filesystem"
              :description "Fuzzy search for file names using fzf"
              :args ((:name "pattern" :type string :description "Fuzzy search pattern for file names. Empty string to match all files.")
                     (:name "exact" :type boolean :description "Enable exact-match." :optional t)
                     (:name "path" :type string :description "Directory to search in. Default is a project root." :optional t)
                     (:name "depth" :type integer :description "Limit the traversal depth, if specified. Do not limit by default." :optional t)
                     (:name "max-results" :type integer :description "Output only the first `max-results'. Default: 50. Set to -1 for no limit." :optional t)))
             (:name "cat"
              :function my/cat-file-tool
              :category "filesystem"
              :description "Read the content of a file and return it as a string."
              :args ((:name "path" :type string :description "Path to the file to read.")
                     (:name "offset" :type integer :description "Line number to start reading from (1-based). Default: 1." :optional t)
                     (:name "limit" :type integer :description "Maximum number of lines to return. Default: 2000." :optional t)))
             (:name "apply_patch"
              :function my/apply-patch-tool
              :category "filesystem"
              :confirm t
              :description "Apply a git-formatted PATCH string using `git apply'. The patch is applied relative to the project root."
              :args ((:name "patch" :type string :description "A git-formatted patch string to apply.")))
             (:name "run_cmd"
              :function my/run-command-tool
              :category "system"
              :confirm t
              :description "Run a shell command to verify build/tests."
              :args ((:name "command" :type string :description "A shell command to run. Only safe commands are allowed.")))
             (:name "read_url"
              :function my/read-url-tool
              :confirm t
              :category "network"
              :description "Fetch and read the visible text content of a URL."
              :args ((:name "url" :type string :description "The URL to fetch.")))))
    (apply #'gptel-make-tool custom-tool-plist))

  (use-package! mcp
    :config (require 'mcp-hub)
    :custom (mcp-hub-servers
             `(("git" . (:command "uvx" :args ("mcp-server-git")))
               ("context7" . (:command "bunx" :args ("--bun" "@upstash/context7-mcp")))
               ("playwright" . (:command "bunx" :args ("--bun" "@playwright/mcp" "--browser" "webkit")))
               ("atlassian" . (:command "podman"
                               :args ("run" "-i" "--rm"
                                      "-e" "CONFLUENCE_*"
                                      "-e" "JIRA_*"
                                      "-e" "ENABLED_TOOLS"
                                      "mcp/atlassian")
                               :env (:ENABLED_TOOLS "jira_search,jira_get_issue,confluence_search,confluence_get_page"))))))

  ;; DEFINE: Prompts
  (defun gptel-prompts-current-project-variables (_file)
    `(("project_root" . ,(doom-project-root))
      ("current_directory" . ,default-directory)))

  (defun gptel-prompts-project-agents-instructions (_file)
    "Read agent's instructiosn from the project root and return it as list."
    `(("project_agents_instructions" . ,(cl-loop
                                         for item in '("AGENTS.md" "GEMINI.md" "CLAUDE.md")
                                         for path = (expand-file-name item (doom-project-root))
                                         when (file-readable-p path)
                                         collect (with-temp-buffer
                                                   (insert-file-contents path)
                                                   (buffer-string))))))
  (defun gptel-prompts-filter-nindent (content n)
    "Indent each line of CONTENT with N spaces."
    (replace-regexp-in-string "^" (make-string n ?\s) content))

  (defun gptel-prompts-add-filters (env)
    "Add the `nindent' filter to the templatel environment."
    (templatel-env-add-filter env "nindent" #'gptel-prompts-filter-nindent))

  (use-package! gptel-prompts
    :demand t
    :config
    (setq gptel-prompts-directory (concat doom-user-dir "prompts/")
          gptel-prompts-template-functions '(gptel-prompts-add-current-time
                                             gptel-prompts-current-project-variables
                                             gptel-prompts-project-agents-instructions))
    (add-hook 'gptel-prompts-prepare-template-env-functions #'gptel-prompts-add-filters)
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

  ;; DEFINE: Presets
  (gptel-make-preset 'default
    :description "Default preset"
    :backend "Gemini"
    :system (alist-get 'default gptel-directives)
    :tools nil)
  (gptel-make-preset 'code-analysis
    :description "A preset optimized for read-only coding tasks"
    :backend "Gemini"
    :system (alist-get 'code-analysis gptel-directives)
    :tools '("fd" "fzf" "rg" "cat"))
  (gptel-make-preset 'programming
    :description "A preset optimized for coding tasks"
    :parents 'code-analysis
    :system (alist-get 'programming gptel-directives)
    :tools '("fd" "fzf" "rg" "cat" "apply_patch" "run_cmd"))
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
         gptel-temperature 1.0
         gptel-display-buffer-action nil
         gptel-expert-commands t
         gptel-max-tokens 64000
         gptel-use-tools t
         gptel-include-tool-results t
         gptel-track-media t
         gptel--preset 'default
         gptel-backend (gptel-make-gemini "Gemini"
                         :stream t
                         :key (getenv "GOOGLE_GENAI_API_KEY"))
         gptel-model 'gemini-3-pro-preview
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
