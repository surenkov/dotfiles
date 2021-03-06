;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Savva Surenkov"
      user-mail-address "savva@surenkov.space")

(require 'xclip)
(xclip-mode 1)

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

(setq doom-theme 'doom-nord
      doom-font (font-spec :family "Iosevka" :size 13)
      doom-big-font (font-spec :family "Iosevka" :size 16)
      doom-variable-pitch-font (font-spec :family "Iosevka" :size 14)

      display-line-numbers-type 'relative

      select-enable-clipboard t
      select-enable-primary nil
      require-final-newline t
      next-line-add-newlines nil

      mmm-submode-decoration-level 1

      projectile-project-search-path '("~/Projects/")

      ls-lisp-dirs-first t
      dired-listing-switches "-aBhl  --group-directories-first" ; requires ls from 'coreutils' on macOS
      dired-dwim-target t)

(setq-default cursor-type 'hollow)

(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-command-modifier 'meta
        insert-directory-program "/usr/local/bin/gls"))

(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] (lambda ()
                              (interactive)
                              (scroll-down 1)))
  (global-set-key [mouse-5] (lambda ()
                              (interactive)
                              (scroll-up 1)))
  (defun track-mouse ()
    (setq mouse-sel-mode t)))

(setq-default flycheck-disabled-checkers '(python-pylint))
(setq +lookup-provider-url-alist
      (cons '("Dash.app" dash-at-point "dash://%s") +lookup-provider-url-alist))

(add-to-list 'auto-mode-alist '("\\.rml\\'" . web-mode))

;; (use-package! company-tabnine
;;   :after company
;;   :when (featurep! :completion company)
;;   :config
;;   (cl-pushnew 'company-tabnine (default-value 'company-backends)))

;; (setq +lsp-company-backend '(company-tabnine company-capf company-yasnippet))


(after! company
  (setq company-idle-delay 0
        company-dabbrev-downcase 0
        company-show-numbers t))

(after! lsp
  (add-hook 'pipenv-mode-hook #'lsp-restart-workspace))

(defun web-mode-better-self-closing-indent (&rest _)
  (setq web-mode-attr-indent-offset nil))

(add-hook 'web-mode-hook #'web-mode-better-self-closing-indent)
(add-hook 'editorconfig-after-apply-functions #'web-mode-better-self-closing-indent)

(use-package! lsp-pyright)

(setq lsp-disabled-clients '(flow-ls)
      lsp-pyright-disable-language-services nil
      lsp-pyright-disable-organize-imports nil
      lsp-log-io nil
      lsp-enable-file-watchers nil
      lsp-restart 'auto-restart)


(setq projectile-project-root-files #'(".projectile" ".git")
      projectile-indexing-method 'hybrid
      projectile-project-root-functions #'(projectile-root-top-down
                                           projectile-root-top-down-recurring
                                           projectile-root-bottom-up
                                           projectile-root-local))

;; Here are some additional functions/macros that could help you configure Doom:
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
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;

(map! :ne "C-i" #'evil-jump-forward
      :ne "C-o" #'evil-jump-backward
      :ne "SPC j" #'evilem-motion-find-char
      :ne "SPC f t" #'treemacs
      :ne "g r" #'+lookup/references)
