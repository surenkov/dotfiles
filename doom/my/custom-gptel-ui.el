;;; custom-gptel-ui.el --- UI polish and previewers for custom gptel tools -*- lexical-binding: t; -*-

(require 'cl-lib)

(defvar gptel-tool-call-actions-map)
(defvar gptel--fsm-last)
(defvar gptel-mode)
(defvar gptel-use-header-line)
(defvar gptel--header-line-info)
(defvar gptel--tool-preview-alist)

(declare-function gptel-fsm-info "gptel")
(declare-function diff-mode "diff-mode")
(declare-function font-lock-append-text-property "font-lock")

(defconst my/gptel-agent--hrule
  (propertize "\n" 'face '(:inherit shadow :underline t :extend t)))

(defun my/gptel-confirm-overlay (from to)
  "Set up a simple confirmation overlay from FROM to TO."
  (let ((ov (make-overlay from to nil t)))
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'gptel-tool t)
    (overlay-put ov 'priority 10)
    (overlay-put ov 'keymap gptel-tool-call-actions-map)
    ov))

(defun my/gptel-agent-preview-setup (arg-values _info)
  "Preview setup for Agent."
  (pcase-let ((from (point))
              (`(,desc ,prompt ,tools) arg-values))
    (insert "("
            (propertize "Agent" 'font-lock-face 'font-lock-keyword-face)
            " "
            (propertize (format "%S" desc)
                        'font-lock-face '(:inherit font-lock-constant-face :weight bold))
            " "
            (propertize (format "Tools: %S" (append tools nil))
                        'font-lock-face 'font-lock-warning-face)
            "\n"
            (propertize prompt
                        'line-prefix "  "
                        'wrap-prefix "  "
                        'font-lock-face 'font-lock-comment-face)
            ")\n\n")
    (my/gptel-confirm-overlay from (point))))

(defun my/gptel-fontify-diff (start end)
  "Fontify region from START to END using `diff-mode`'s font-lock rules."
  (let ((org-buffer (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring-no-properties org-buffer start end)
      (insert " ")
      (delay-mode-hooks
        (diff-mode))
      (font-lock-ensure)
      (let ((pos (point-min)))
        (while (< pos (1- (point-max)))
          (let* ((next (next-property-change pos nil (1- (point-max))))
                 (face-prop (get-text-property pos 'face)))
            (when face-prop
              (put-text-property
               (+ start (- pos (point-min)))
               (+ start (- (or next (1- (point-max))) (point-min)))
               'font-lock-face face-prop org-buffer))
            (setq pos (or next (1- (point-max))))))))))

(defun my/gptel-edit-preview-setup (arg-values _info)
  "Preview setup for the custom Edit tool, rendering the diff in `diff-mode`."
  (pcase-let ((from (point))
              (first (car arg-values))
              (second (cadr arg-values))
              (third (caddr arg-values)))
    (if (and (stringp first) (stringp second) (stringp third))
        ;; New edit tool format: (path old-string new-string &optional replace-all)
        (let* ((file first)
               (old (or second ""))
               (new (or third ""))
               (diff (format "--- a/%s\n+++ b/%s\n@@ -1 +1 @@\n- %s\n+ %s"
                             file file
                             (replace-regexp-in-string "\n" "\n- " old)
                             (replace-regexp-in-string "\n" "\n+ " new))))
          (insert
           (propertize "Edit" 'font-lock-face 'font-lock-keyword-face)
           " "
           (propertize (concat "\"" file "\"") 'font-lock-face 'font-lock-constant-face)
           "\n")
          (let ((diff-start (point)))
            (insert diff)
            (unless (bolp) (insert "\n"))
            (require 'diff-mode)
            (my/gptel-fontify-diff diff-start (point))
            (let ((bg-face (cond
                            ((derived-mode-p 'org-mode) 'org-block)
                            ((derived-mode-p 'markdown-mode) 'markdown-code-face)
                            (t `(:background ,(face-attribute 'mode-line-inactive :background)
                                 :extend t)))))
              (font-lock-append-text-property diff-start (1- (point)) 'font-lock-face bg-face)))
          (insert "\n")
          (my/gptel-confirm-overlay from (point)))
      ;; Legacy patch format fallback
      (let ((patch (or first ""))
            (files-affected nil))
        (with-temp-buffer
          (insert patch)
          (goto-char (point-min))
          (while (re-search-forward "^\\+\\+\\+ [ab/]*\\([^ \t\r\n]+\\)" nil t)
            (let ((file (match-string 1)))
              (unless (equal file "/dev/null")
                (push file files-affected))))
          (setq files-affected (delete-dups (nreverse files-affected))))
        (insert
         (propertize "Edit" 'font-lock-face 'font-lock-keyword-face)
         " "
         (mapconcat (lambda (f)
                      (propertize (concat "\"" f "\"")
                                  'font-lock-face 'font-lock-constant-face))
                    files-affected " ")
         "\n")
        (let ((diff-start (point)))
          (insert patch)
          (unless (bolp) (insert "\n"))
          (save-excursion
            (goto-char diff-start)
            (when (looking-at "^ *```\\(diff\\|patch\\)\\s-*\n")
              (delete-region (match-beginning 0) (match-end 0)))
            (goto-char (point-max))
            (skip-chars-backward " \t\r\n")
            (when (looking-back "^ *```\\s-*\\'" (line-beginning-position))
              (delete-region (line-beginning-position) (line-end-position))))
          (require 'diff-mode)
          (my/gptel-fontify-diff diff-start (point))
          (let ((bg-face (cond
                          ((derived-mode-p 'org-mode) 'org-block)
                          ((derived-mode-p 'markdown-mode) 'markdown-code-face)
                          (t `(:background ,(face-attribute 'mode-line-inactive :background)
                               :extend t)))))
            (font-lock-append-text-property diff-start (1- (point)) 'font-lock-face bg-face)))
        (insert "\n")
        (my/gptel-confirm-overlay from (point))))))

;; Todo display
(defvar-local my/gptel-agent--todos nil)

(defun my/gptel-agent-toggle-todos ()
  "Toggle the display of the gptel agent todo list."
  (interactive)
  (pcase-let ((`(,prop-value . ,ov)
               (or (get-char-property-and-overlay (point) 'my/gptel-agent--todos)
                   (get-char-property-and-overlay
                    (previous-single-char-property-change
                     (point) 'my/gptel-agent--todos nil (point-min))
                    'my/gptel-agent--todos))))
    (if (and ov (overlayp ov))
        (if-let* ((fmt (overlay-get ov 'after-string)))
            (progn (overlay-put ov 'my/gptel-agent--todos fmt)
                   (overlay-put ov 'after-string nil))
          (overlay-put ov 'after-string
                       (and (stringp prop-value) prop-value))
          (overlay-put ov 'my/gptel-agent--todos t))
      (message "No task list overlay found at point."))))

(defun my/gptel-write-todo (todos)
  "Display a formatted task list in the buffer.
TODOS is a list/vector of plists with keys :content, :activeForm, and :status."
  (setq my/gptel-agent--todos todos)
  (let* ((info (gptel-fsm-info gptel--fsm-last))
         (where-from
          (previous-single-property-change
           (plist-get info :position) 'gptel nil (point-min)))
         (where-to (plist-get info :position)))
    (unless (= where-from where-to)
      (pcase-let* ((ov-start (max (point-min) (1- where-to)))
                   (`(,_ . ,todo-ov)
                    (get-char-property-and-overlay ov-start 'my/gptel-agent--todos)))
        (if todo-ov
            (move-overlay todo-ov ov-start where-to)
          ;; Place the overlay ONLY on the last character of the response.
          ;; This isolates the TAB hijacking to a single character visually representing
          ;; the task list, preserving standard TAB behavior in the rest of the block.
          (setq todo-ov (make-overlay ov-start where-to nil t))
          (overlay-put todo-ov 'my/gptel-agent--todos t)
          (overlay-put todo-ov 'evaporate t)
          (overlay-put todo-ov 'priority -40)
          (overlay-put todo-ov 'keymap (define-keymap
                                         "<tab>" #'my/gptel-agent-toggle-todos
                                         "TAB"   #'my/gptel-agent-toggle-todos))
          ;; Clean up/delete any other obsolete task list overlays in the buffer
          (dolist (ov (overlays-in (point-min) (point-max)))
            (when (and (overlay-get ov 'my/gptel-agent--todos)
                       (not (eq ov todo-ov)))
              (delete-overlay ov)))

          (plist-put
           info :post
           (cons (lambda (&rest _)
                   (when (and (buffer-live-p (current-buffer))
                              gptel-mode gptel-use-header-line
                              (listp header-line-format)
                              (>= (length header-line-format) 3))
                     (setf (nth 2 header-line-format) gptel--header-line-info)))
                 (plist-get info :post))))
        (let* ((todos-list (append todos nil))
               (formatted-todos
                (mapconcat
                 (lambda (todo)
                   (pcase (plist-get todo :status)
                     ("completed"
                      (concat "✓ " (propertize (plist-get todo :content)
                                               'face '(:inherit shadow :strike-through t))))
                     ("in_progress"
                      (concat "● " (propertize (plist-get todo :activeForm)
                                               'face '(:inherit bold :inherit warning))))
                     (_ (concat "○ " (plist-get todo :content)))))
                 todos-list "\n"))
               (in-progress
                (cl-loop for todo in todos-list
                         when (equal (plist-get todo :status) "in_progress")
                         return (plist-get todo :activeForm)))
               (todo-display
                (concat
                 "\n"
                 my/gptel-agent--hrule
                 (propertize "Task list: [ "
                             'face '(:inherit font-lock-comment-face :inherit bold))
                 ;; Resolve the key binding cleanly without moving the point or risking goto-char crashes
                 (let ((overriding-local-map (overlay-get todo-ov 'keymap)))
                   (propertize (substitute-command-keys "\\[my/gptel-agent-toggle-todos]")
                               'face 'help-key-binding))
                 (propertize " to toggle display ]\n" 'face 'font-lock-comment-face)
                 formatted-todos "\n"
                 my/gptel-agent--hrule)))
          (overlay-put todo-ov 'after-string todo-display)
          (when (and gptel-mode gptel-use-header-line in-progress
                     (listp header-line-format)
                     (>= (length header-line-format) 3))
            (setf (nth 2 header-line-format)
                  (concat (propertize
                           " " 'display
                           `(space :align-to (- right ,(+ 5 (length in-progress)))))
                          (propertize (concat "Task: " in-progress)
                                      'face 'font-lock-escape-face))))))))
  t)

;; Register the previews
(with-eval-after-load 'gptel
  (setf (alist-get "agent" gptel--tool-preview-alist nil nil #'string-equal)
        '(my/gptel-agent-preview-setup))
  (setf (alist-get "edit" gptel--tool-preview-alist nil nil #'string-equal)
        '(my/gptel-edit-preview-setup)))

(defun gptel-prompts-buffer-mode-variables (_file)
  "Expose current buffer's mode and stripped syntax name for templating."
  (let* ((mode-sym major-mode)
         (mode-str (and mode-sym (fboundp 'gptel--strip-mode-suffix)
                        (gptel--strip-mode-suffix mode-sym))))
    `(("major_mode" . ,(symbol-name mode-sym))
      ("mode_syntax" . ,(or mode-str ""))
      ("is_org_mode" . ,(eq mode-sym 'org-mode))
      ("is_markdown_mode" . ,(eq mode-sym 'markdown-mode)))))

(provide 'custom-gptel-ui)
;;; custom-gptel-ui.el ends here
