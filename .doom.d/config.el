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

      projectile-project-search-path '("~/Projects/")
      projectile-indexing-method 'hybrid
      org-directory "~/org/"

      ls-lisp-dirs-first t
      dired-listing-switches "-aBhl --group-directories-first" ; requires ls from 'coreutils' on macOS
      dired-dwim-target t)

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

(setq lsp-disabled-clients '(flow-ls jsts-ls)
      lsp-diagnostics-provider :auto
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

(use-package! gptel
  :config
  (gptel-make-ollama "Ollama"
    :host "localhost:11434"
    :stream t
    :models '("qwen2.5-coder:14b"
              "qwen2.5-coder:32b"
              "qwen3:30b-a3b"
              "qwen3:8b"
              "gemma3:12b"
              "gemma3:27b"))
  (gptel-make-anthropic "Claude"
    :stream t
    :key (getenv "ANTHROPIC_API_KEY"))
  (gptel-make-anthropic "Claude-Thinking"
    :stream t
    :key (getenv "ANTHROPIC_API_KEY")
    :models '((claude-3-7-sonnet-latest
               :description "Highest level of intelligence and capability"
               :capabilities (media tool-use cache)
               :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
               :context-window 200
               :input-cost 3
               :output-cost 15
               :cutoff-date "2024-11"))
    :request-params '(:thinking (:type "enabled" :budget_tokens 16384))
    :header (lambda () (when-let* ((key (gptel--get-api-key)))
                         `(("x-api-key" . ,key)
                           ("anthropic-version" . "2023-06-01")
                           ("anthropic-beta" . "pdfs-2024-09-25")
                           ("anthropic-beta" . "output-128k-2025-02-19")
                           ("anthropic-beta" . "prompt-caching-2024-07-31")))))

  (setq! gptel-api-key (getenv "OPENAI_API_KEY")
         gptel-expert-commands t
         gptel-use-tools t
         gptel-max-tokens 32768
         gptel-backend (gptel-make-gemini "Gemini"
                         :stream t
                         :key (getenv "GOOGLE_GENAI_API_KEY"))
         gptel-model 'gemini-2.5-pro-preview-03-25
         gptel-log-level 'debug
         gptel-default-mode 'org-mode
         gptel-tools (list
                      (gptel-make-tool
                       :function (lambda (buffer)
                                   (unless (buffer-live-p (get-buffer buffer))
                                     (error "Error: buffer %s is not live." buffer))
                                   (with-current-buffer buffer
                                     (buffer-substring-no-properties (point-min) (point-max))))
                       :name "read_buffer"
                       :description "Return the contents of an Emacs buffer"
                       :args (list '(:name "buffer"
                                     :type string
                                     :description "The name of the buffer whose contents are to be retrieved"))
                       :category "emacs")
                      (gptel-make-tool
                       :function (lambda (url)
                                   (with-current-buffer (url-retrieve-synchronously url)
                                     (goto-char (point-min))
                                     (forward-paragraph)
                                     (let ((dom (libxml-parse-html-region (point) (point-max))))
                                       (run-at-time 0 nil #'kill-buffer (current-buffer))
                                       (with-temp-buffer
                                         (shr-insert-document dom)
                                         (buffer-substring-no-properties (point-min) (point-max))))))
                       :name "read_url"
                       :description "Fetch and read the contents of a URL"
                       :args (list '(:name "url"
                                     :type string
                                     :description "The URL to read"))
                       :category "web")
                      (gptel-make-tool
                       :function (lambda (directory)
                                   (mapconcat #'identity
                                              (directory-files directory)
                                              "\n"))
                       :name "list_directory"
                       :description "List the contents of a given directory"
                       :args (list '(:name "directory"
                                     :type string
                                     :description "The path to the directory to list"))
                       :category "filesystem")
                      (gptel-make-tool
                       :function (lambda (filepath)
                                   (with-temp-buffer
                                     (insert-file-contents (expand-file-name filepath))
                                     (buffer-string)))
                       :name "read_file"
                       :description "Read and display the contents of a file"
                       :args (list '(:name "filepath"
                                     :type string
                                     :description "Path to the file to read. Supports relative paths and ~."))
                       :category "filesystem"))))

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
   (sql . nil)
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
      :n "g D"      #'+lookup/type-definition
      :nv "SPC c F" #'consult-lsp-file-symbols
      :nv "SPC o C" #'gptel
      :nv "SPC o c" #'gptel-menu
      :nv "SPC b a" #'gptel-add
      :nv "SPC f a" #'gptel-add-file)

(map! :after transient
      :map transient-map
      [escape] #'transient-quit-one)

(after! gptel
  (map! :mode gptel-mode
        :n "RET"     #'gptel-send)
  (map! :mode gptel-context-buffer-mode
        :n "C-c C-c" #'gptel-context-confirm
        :n "C-c C-k" #'gptel-context-quit
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
