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
      doom-font (font-spec :family "Iosevka" :size 13)
      doom-big-font (font-spec :family "Iosevka" :size 16)
      doom-variable-pitch-font (font-spec :family "Iosevka" :size 14)

      truncate-string-ellipsis "…"
      scroll-margin 3
      evil-want-fine-undo t
      undo-limit 8000000

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

(global-set-key [mouse-4] (lambda () (interactive) (scroll-down 4)))
(global-set-key [mouse-5] (lambda () (interactive) (scroll-up 4)))
(global-set-key [S-mouse-4] (lambda () (interactive) (scroll-down 1)))
(global-set-key [S-mouse-5] (lambda () (interactive) (scroll-up 1)))
(global-set-key [C-mouse-4] (lambda () (interactive) (scroll-down)))
(global-set-key [C-mouse-5] (lambda () (interactive) (scroll-up)))

(global-subword-mode 1)                 ; Iterate through CamelCase words

(setq-default cursor-type 'hollow)

(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-command-modifier 'meta
        insert-directory-program "/usr/local/bin/gls"))

(add-hook! (prog-mode conf-mode text-mode) #'display-fill-column-indicator-mode)

(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (+vertico/switch-workspace-buffer))

(setq-default flycheck-disabled-checkers '(python-pylint))

(after! company
  (setq company-idle-delay 0
        company-dabbrev-downcase 0
        company-show-quick-access t))

(defun web-mode-better-self-closing-indent (&rest _)
  (setq web-mode-attr-indent-offset nil))
(add-hook 'web-mode-hook #'web-mode-better-self-closing-indent)
(add-hook 'editorconfig-after-apply-functions #'web-mode-better-self-closing-indent)

;; (after! 'c++-mode
;;   (lsp-deferred)
;;   (platformio-conditionally-enable))

(setq lsp-disabled-clients '(flow-ls jsts-ls)
      lsp-pyright-disable-language-services nil
      lsp-pyright-disable-organize-imports nil
      lsp-log-io nil
      lsp-enable-file-watchers nil
      lsp-restart 'auto-restart)

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

(map! :ne "C-i" #'evil-jump-forward
      :ne "C-o" #'evil-jump-backward
      :ne "SPC j" #'evilem-motion-find-char
      :ne "SPC f t" #'treemacs
      :ne "g r" #'+lookup/references)

(unless (display-graphic-p)
  (custom-set-faces! '(default :background "default"))
  (custom-theme-set-faces! 'doom-plain-dark '(default :background "default"))
  (custom-theme-set-faces! 'doom-nord '(default :background "default")))
