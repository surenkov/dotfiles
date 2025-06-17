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

(setq lsp-disabled-clients '(flow-ls jsts-ls xmlls)
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

(after! gptel
  ;; DEFINE: LLMs
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

  ;; DEFINE: tools

  (defun gptel--edit-buffer (buffer-name old-string new-string)
    "In BUFFER-NAME, replace OLD-STRING with NEW-STRING."
    (with-current-buffer buffer-name
      (let ((case-fold-search nil))  ;; Case-sensitive search
        (save-excursion
          (goto-char (point-min))
          (let ((count 0))
            (while (search-forward old-string nil t)
              (setq count (1+ count)))
            (if (= count 0)
                (format "Error: Could not find text to replace in buffer %s" buffer-name)
              (if (> count 1)
                  (format "Error: Found %d matches for the text to replace in buffer %s" count buffer-name)
                (goto-char (point-min))
                (search-forward old-string)
                (replace-match new-string t t)
                (format "Successfully edited buffer %s" buffer-name))))))))

  (defun gptel--edit-file (file-path file-edits)
    "In FILE-PATH, apply FILE-EDITS with pattern matching and replacing."
    (if (and file-path (not (string= file-path "")) file-edits)
        (with-current-buffer (get-buffer-create "*edit-file*")
          (erase-buffer)
          (insert-file-contents (expand-file-name file-path))
          (let ((inhibit-read-only t)
                (case-fold-search nil)
                (file-name (expand-file-name file-path))
                (edit-success nil))
            ;; apply changes
            (dolist (file-edit (seq-into file-edits 'list))
              (when-let ((line-number (plist-get file-edit :line_number))
                         (old-string (plist-get file-edit :old_string))
                         (new-string (plist-get file-edit :new_string))
                         (is-valid-old-string (not (string= old-string ""))))
                (goto-char (point-min))
                (forward-line (1- line-number))
                (when (search-forward old-string nil t)
                  (replace-match new-string t t)
                  (setq edit-success t))))
            ;; return result to gptel
            (if edit-success
                (progn
                  ;; show diffs
                  (ediff-buffers (find-file-noselect file-name) (current-buffer))
                  (format "Successfully edited %s" file-name))
              (format "Failed to edited %s" file-name))))
      (format "Failed to edited %s" file-path)))

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
   :category "edit")

  (gptel-make-tool
   :name "edit_buffer"
   :function #'gptel--edit-buffer
   :description "Edits Emacs buffers"
   :args '((:name "buffer_name"
            :type string
            :description "Name of the buffer to modify"
            :required t)
           (:name "old_string"
            :type string
            :description "Text to replace (must match exactly)"
            :required t)
           (:name "new_string"
            :type string
            :description "Text to replace old_string with"
            :required t))
   :category "edit")

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
   :function (lambda (directory) (mapconcat #'identity (directory-files directory) "\n"))
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
   :category "filesystem")

  (gptel-make-tool
   :function (lambda (parent name)
               (condition-case nil
                   (progn
                     (make-directory (expand-file-name name parent) t)
                     (format "Directory %s created/verified in %s" name parent))
                 (error (format "Error creating directory %s in %s" name parent))))
   :name "make_directory"
   :description "Create a new directory with the given name in the specified parent directory"
   :args (list '(:name "parent"
                 :type string
                 :description "The parent directory where the new directory should be created, e.g. /tmp")
               '(:name "name"
                 :type string
                 :description "The name of the new directory to create, e.g. testdir"))
   :category "filesystem")

  (gptel-make-tool
   :function (lambda (path filename content)
               (let ((full-path (expand-file-name filename path)))
                 (with-temp-buffer
                   (insert content)
                   (write-file full-path))
                 (format "Created file %s in %s" filename path)))
   :name "create_file"
   :description "Create a new file with the specified content"
   :args (list '(:name "path"
                 :type string
                 :description "The directory where to create the file")
               '(:name "filename"
                 :type string
                 :description "The name of the file to create")
               '(:name "content"
                 :type string
                 :description "The content to write to the file"))
   :category "filesystem")

  (gptel-make-tool
   :function #'gptel--edit-file
   :name "edit_file"
   :description "Edit file with a list of edits, each edit contains a line-number,
a old-string and a new-string, new-string will replace the old-string at the specified line."
   :args (list '(:name "file-path"
                 :type string
                 :description "The full path of the file to edit")
               '(:name "file-edits"
                 :type array
                 :items (:type object
                         :properties
                         (:line_number
                          (:type integer :description "The line number of the file where edit starts.")
                          :old_string
                          (:type string :description "The old-string to be replaced.")
                          :new_string
                          (:type string :description "The new-string to replace old-string.")))
                 :description "The list of edits to apply on the file"))
   :category "filesystem")

  ;; DEFINE: Presets

  (gptel-make-preset 'coding
    :description "A preset optimized for coding tasks"
    :backend "Gemini"
    :model 'gemini-2.5-pro
    :system "You are an expert coding assistant. Your role is to provide high-quality code solutions, refactorings, and explanations."
    :tools '("read_buffer" "read_url" "list_directory" "read_file" "make_directory" "create_file" "edit_file"))

  ;; DEFINE: Config

  (defun gptel--select-and-resize-window (window)
    (select-window window)
    (evil-resize-window 80 t))

  (setq! gptel-api-key (getenv "OPENAI_API_KEY")
         gptel-display-buffer-action `(nil (body-function . ,#'gptel--select-and-resize-window))
         gptel-expert-commands t
         gptel-use-tools t
         gptel-max-tokens 32000
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
