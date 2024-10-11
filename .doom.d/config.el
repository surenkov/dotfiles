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
  (setq corfu-auto-delay 0.05))

(after! doom-modeline ;; modeline icons are currently interfering with lsp (somehow)
  (setq doom-modeline-icon nil))

(defun web-mode-better-self-closing-indent (&rest _)
  (setq web-mode-attr-indent-offset nil))
(add-hook 'web-mode-hook #'web-mode-better-self-closing-indent)
(add-hook 'editorconfig-after-apply-functions #'web-mode-better-self-closing-indent)

(setq lsp-disabled-clients '(flow-ls jsts-ls)
      lsp-diagnostics-provider :auto
      lsp-pyright-multi-root nil
      lsp-log-io nil
      lsp-idle-delay 0.500
      lsp-use-plists t
      read-process-output-max (* 1024 1024)
      lsp-enable-file-watchers t
      lsp-restart 'auto-restart)

(after! lsp-mode
  (advice-add #'add-node-modules-path :override #'ignore))
(after! dap-mode
  (setq dap-python-debugger 'debugpy))

(use-package! gptel
  :config
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (gptel-make-openai "Perplexity"
    :host "api.perplexity.ai"
    :key (getenv "PERPLEXITY_API_KEY")
    :endpoint "/chat/completions"
    :stream t
    :models '("llama-3.1-sonar-large-128k-online" "llama-3.1-sonar-huge-128k-online"))
  (gptel-make-ollama "Ollama"
    :host "localhost:11434"
    :stream t
    :models '("llama3.1:8b" "gemma2:9b"))
  (setq! gptel-api-key (getenv "OPENAI_API_KEY")
         gptel-expert-commands t
         gptel-backend (gptel-make-anthropic "Claude"
                         :stream t
                         :key (getenv "ANTHROPIC_API_KEY"))
         gptel-model "claude-3-5-sonnet-20240620"))

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
      :nv "SPC o c" #'gptel
      :nv "SPC o C" #'gptel-menu
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

(if (display-graphic-p)
    (progn
      (custom-set-faces! '(default :background "#000000"))
      (custom-theme-set-faces! 'doom-plain-dark '(default :background "#000000"))
      (custom-theme-set-faces! 'doom-nord '(default :background "#000000"))
      (custom-set-faces! '(gptel-context-highlight-face :background "#2C333F")))
  (progn
    (custom-set-faces! '(default :background nil))
    (custom-theme-set-faces! 'doom-plain-dark '(default :background nil))
    (custom-theme-set-faces! 'doom-nord '(default :background nil))
    (custom-set-faces! '(gptel-context-highlight-face :background "#2C333F"))))
