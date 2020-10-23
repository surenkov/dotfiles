;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Savva Surenkov"
      user-mail-address "savva@surenkov.space")

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
      mac-command-modifier 'meta

      select-enable-clipboard t
      ;select-enable-primary t

      mmm-submode-decoration-level 1

      projectile-project-search-path '("~/Projects/")
      ls-lisp-dirs-first t
      dired-dwim-target t)
      ;dired-listing-switches "-al --group-directories-first")

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

(after! company
  (setq company-idle-delay 0
        company-dabbrev-downcase 0))

(after! lsp
  (add-hook 'pipenv-mode-hook #'lsp-restart-workspace))

(defun web-mode-better-self-closing-indent (&rest _)
  (setq web-mode-attr-indent-offset nil))

(add-hook 'web-mode-hook #'web-mode-better-self-closing-indent)
(add-hook 'editorconfig-after-apply-functions #'web-mode-better-self-closing-indent)

(use-package! lsp-pyright)

(with-eval-after-load 'lsp-mode
  ;(push "[/\\\\][^/\\\\]*\\.\\(json\\|html\\|jade\\)$" lsp-file-watch-ignored)

  (setq lsp-enabled-clients '(pyright jsts-ls ts-ls html-ls css-ls vls)
        lsp-pyright-disable-language-services nil
        lsp-pyright-disable-organize-imports nil
        ;lsp-idle-delay 0.500
        lsp-enable-file-watchers nil
        lsp-restart 'auto-restart))



(setq projectile-project-root-files #'(".projectile" ".git")
      projectile-indexing-method 'hybrid
      projectile-project-root-files-functions #'(projectile-root-top-down
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
      :ne "C-o" #'evil-jump-backward)

(map! :ne "SPC f t" #'treemacs
      :ne "g r" #'+lookup/references)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "black")))))
