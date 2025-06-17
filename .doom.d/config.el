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

(cond ((featurep :system 'macos) ;; mac specific settings
       (setq insert-directory-program "/opt/homebrew/bin/gls")))

(add-hook! (prog-mode conf-mode text-mode) #'display-fill-column-indicator-mode)

(after! company
  (setq company-idle-delay 0
        company-dabbrev-downcase 0
        company-require-match nil
        company-show-quick-access t))

(after! corfu
  (setq corfu-preview-current 'insert
        corfu-auto-delay 0.05))

(after! doom-modeline ;; modeline icons are currently interfering with lsp (somehow)
  (setq doom-modeline-icon nil))

(defun web-mode-better-self-closing-indent (&rest _)
  (setq web-mode-attr-indent-offset nil))
(add-hook 'web-mode-hook #'web-mode-better-self-closing-indent)
(add-hook 'editorconfig-after-apply-functions #'web-mode-better-self-closing-indent)

(setq lsp-disabled-clients '(flow-ls jsts-ls xmlls)
      lsp-diagnostics-provider :auto
      lsp-pyright-langserver-command "basedpyright"
      lsp-pyright-multi-root nil
      lsp-ruff-ruff-args '("--preview")
      lsp-log-io nil
      lsp-idle-delay 0.500
      lsp-use-plists t
      read-process-output-max (* 1024 1024)
      lsp-enable-file-watchers t
      lsp-restart 'auto-restart
      lsp-enable-dap-auto-configure t)

(after! org-modern
  (setq org-modern-checkbox '((?X . "☑") (?- . #("□–" 0 2 (composition ((2))))) (?\s . "□"))))

(after! lsp-mode
  (defvar lsp--warning-suppress-message-regexps '("Unknown notification: semgrep/rulesRefreshed"))

  (defun lsp--display-warning-advise (type message &optional level buffer-name)
    (catch 'exit
      (dolist (regexp lsp--warning-suppress-message-regexps)
        (when (string-match-p regexp message) (throw 'exit nil)))
      (throw 'exit t)))

  (advice-add 'display-warning :before-while #'lsp--display-warning-advise)
  (advice-add #'add-node-modules-path :override #'ignore))

(after! dap-mode
  (add-hook! 'dap-stopped-hook
    (lambda (arg) (call-interactively #'dap-hydra))))

(after! (python dap-mode)
  (require 'dap-python)
  (require 'dap-lldb)
  (setq dap-python-debugger 'debugpy)
  (defun dap-python--pyenv-executable-find (command)
    (executable-find "python")))

(after! gptel
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
    :models '((claude-sonnet-4-0
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
  (use-package! llm-tool-collection)
  (use-package! codel)

  (apply #'gptel-make-tool llm-tc/read-file)
  (apply #'gptel-make-tool llm-tc/list-directory)
  (apply #'gptel-make-tool llm-tc/create-file)
  (apply #'gptel-make-tool llm-tc/create-directory)

  (apply #'gptel-make-tool llm-tc/view-buffer)
  (apply #'gptel-make-tool llm-tc/edit-buffer)

  (dolist (custom-tool-plist
           '((:name "glob"
              :function codel-glob-tool
              :category "filesystem"
              :description "Find files matching glob pattern"
              :args ((:name "pattern" :type string :description "Glob pattern to match files")
                     (:name "path" :type string :description "Directory to search in" :optional t)))
             (:name "grep"
              :function codel-grep-tool
              :category "filesystem"
              :description "Search file content using regex"
              :args ((:name "pattern" :type string :description "Regex pattern to search in file contents")
                     (:name "include" :type string :description "File pattern to include in search")
                     (:name "path" :type string :description "Directory to search in" :optional t)))
             (:name "edit_file"
              :function codel-edit
              :category "filesystem"
              :description "Edit specified file"
              :confirm t
              :args ((:name "file_path" :type string :description "Absolute path of the file to modify")
                     (:name "old_string" :type string :description "Text to replace (must match exactly)")
                     (:name "new_string" :type string :description "New text to replace old_string with")))))
    (apply #'gptel-make-tool custom-tool-plist))

  ;; DEFINE: Presets

  (gptel-make-preset 'default
                     :description "Default preset"
                     :backend "Gemini"
                     :system (alist-get 'default gptel-directives)
                     :model 'gemini-2.5-pro
                     :tools nil)

  (gptel-make-preset 'coding
                     :description "A preset optimized for coding tasks"
                     :backend "Gemini"
                     :model 'gemini-2.5-pro
                     :system "You are an expert coding assistant. Your role is to provide high-quality code solutions, refactorings, and explanations."
                     :tools '("glob" "grep" "list_directory" "read_file" "create_file" "edit_file" "create_directory" "view_buffer" "edit_buffer"))

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
         gptel-max-tokens 32000
         gptel--preset 'default
         gptel-backend (gptel-make-gemini "Gemini"
                         :stream t
                         :key (getenv "GOOGLE_GENAI_API_KEY"))
         gptel-model 'gemini-2.5-pro
         gptel-default-mode 'org-mode
         gptel-log-level 'info
         gptel-use-context 'user))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (haskell . nil)
   (latex . t)
   (ledger . t)
   (hledger . t)
   (python . t)
   (ruby . t)
   (javascript . t)
   (html . t)
   (sh . t)
   (sql . t)
   (sqlite . t)))

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
  '(ts-fold-replacement-face :foreground nil :box nil :inherit font-lock-comment-face :weight light))

(if (display-graphic-p)
    (progn
      (custom-set-faces! '(default :background "#000000"))
      (custom-theme-set-faces! 'doom-plain-dark '(default :background "#000000"))
      (custom-theme-set-faces! 'doom-nord '(default :background "#000000")))
  (progn
    (custom-set-faces! '(default :background nil))
    (custom-set-faces! '(ein:basecell-input-area-face :background nil))
    (custom-theme-set-faces! 'doom-plain-dark '(default :background nil))
    (custom-theme-set-faces! 'doom-nord '(default :background nil))))
