;;; custom-gptel-agents.el --- Sub-agents execution subsystem for gptel -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'map)
(require 'gptel)

(defvar gptel-model)
(defvar gptel--fsm-last)
(defvar gptel-send--transitions)
(defvar gptel-tools)
(defvar gptel-confirm-tool-calls)
(defvar gptel-backend)

(declare-function gptel--model-name "gptel")
(declare-function gptel--display-tool-calls "gptel")
(declare-function gptel--transform-add-context "gptel")
(declare-function gptel-make-fsm "gptel")
(declare-function gptel-request "gptel")
(declare-function gptel-with-preset "gptel")
(declare-function gptel--update-status "gptel")
(declare-function gptel--format-tool-call "gptel")
(declare-function gptel-fsm-info "gptel")
(declare-function gptel--handle-tool-result "gptel")
(declare-function gptel--handle-post-tool "gptel")
(declare-function gptel--handle-tool-use "gptel")
(declare-function gptel--fsm-transition "gptel")
(declare-function gptel--handle-pre-tool "gptel")
(declare-function gptel--handle-wait "gptel")
(declare-function doom-project-root "doom-lib")
(declare-function gptel-tool-name "gptel")

(defvar my/gptel-agent-request--handlers
  `((WAIT ,#'my/gptel-agent--indicate-wait
          ,#'gptel--handle-wait)
    (TPRE ,#'gptel--handle-pre-tool ,#'gptel--fsm-transition)
    (TOOL ,#'my/gptel-agent--indicate-tool-call
          ,#'gptel--handle-tool-use)
    (TRET ,#'gptel--handle-post-tool
          ,#'gptel--handle-tool-result))
  "FSM Handlers for sub-agent tracking.")

(defun my/gptel-agent--indicate-wait (fsm)
  "Display waiting indicator for agent task FSM."
  (when-let* ((info (gptel-fsm-info fsm))
              (info-ov (plist-get info :context))
              (count (overlay-get info-ov 'count)))
    ;; Cancel any outstanding timer to prevent memory/resource leaks
    (when-let ((old-timer (overlay-get info-ov 'timer)))
      (cancel-timer old-timer))
    (let ((timer
           (run-at-time
            1.5 nil
            (lambda (ov count)
              (when (and (overlay-buffer ov)
                         (eql (overlay-get ov 'count) count))
                (let* ((task-msg (overlay-get ov 'msg))
                       (new-info-msg
                        (concat task-msg
                                (concat
                                 (propertize "Waiting... " 'face 'warning) "\n"
                                 (propertize "\n" 'face
                                             '(:inherit shadow :underline t :extend t))))))
                  (overlay-put ov 'after-string new-info-msg))))
            info-ov count)))
      ;; Store active timer reference directly on the overlay
      (overlay-put info-ov 'timer timer))))

(defun my/gptel-agent--indicate-tool-call (fsm)
  "Display tool call indicator for agent task FSM."
  (when-let* ((info (gptel-fsm-info fsm))
              (tool-use (plist-get info :tool-use))
              (ov (plist-get info :context)))
    ;; Cancel waiting timer as we transitioned into tool calling
    (when-let ((old-timer (overlay-get ov 'timer)))
      (cancel-timer old-timer)
      (overlay-put ov 'timer nil))
    (when (overlay-buffer ov)
      (let* ((task-msg (overlay-get ov 'msg))
             (info-count (overlay-get ov 'count))
             (new-info-msg))
        (setq new-info-msg
              (concat task-msg
                      (concat
                       (propertize "Calling Tools... " 'face 'mode-line-emphasis)
                       (if (= info-count 0) "\n" (format "(+%d)\n" info-count))
                       (mapconcat (lambda (call)
                                    (gptel--format-tool-call
                                     (plist-get call :name)
                                     (map-values (plist-get call :args))))
                                  tool-use)
                       "\n" (propertize "\n" 'face '(:inherit shadow :underline t :extend t)))))
        (overlay-put ov 'count (+ info-count (length tool-use)))
        (overlay-put ov 'after-string new-info-msg)))))

(defun my/gptel-agent--task-overlay (where description)
  "Create overlay for agent task at WHERE with DESCRIPTION."
  (let* ((bounds
          (save-excursion
            (goto-char (or where (point)))
            (if (and (bolp) (eolp))
                (cons (point) (point))
              (cons (line-beginning-position) (line-end-position)))))
         (ov (make-overlay (car bounds) (cdr bounds) nil t))
         (model (propertize (concat (gptel--model-name gptel-model))
                            'face 'font-lock-comment-face))
         (msg (concat
               (unless (eq (char-after (car bounds)) 10) "\n")
               "\n" (propertize "\n" 'face '(:inherit shadow :underline t :extend t))
               (propertize "Sub-agent Task: "
                           'face 'font-lock-escape-face)
               (propertize (or description "(no description)") 'face 'font-lock-doc-face)
               (propertize
                " " 'display
                (if (and (display-graphic-p) (fboundp 'string-pixel-width))
                    `(space :align-to (- right (,(string-pixel-width model))))
                  `(space :align-to (- right ,(+ 5 (string-width model))))))
               model "\n")))
    (prog1 ov
      (overlay-put ov 'gptel-agent t)
      (overlay-put ov 'count 0)
      (overlay-put ov 'msg msg)
      (overlay-put ov 'line-prefix "")
      (overlay-put
       ov 'after-string
       (concat msg (propertize "Waiting..." 'face 'warning) "\n"
               (propertize "\n" 'face '(:inherit shadow :underline t :extend t)))))))

(defun my/gptel-agent--task (main-cb description prompt delegated-tools)
  "Spawn a dynamic, derivative sub-agent."
  (let* ((parent-tool-names (mapcar #'gptel-tool-name gptel-tools))
         (requested-tools (append delegated-tools nil))
         (validated-tools (delete "agent" requested-tools))
         (invalid-tools (cl-remove-if (lambda (tname) (member tname parent-tool-names))
                                      validated-tools))
         (info (gptel-fsm-info gptel--fsm-last))
         (where (or (plist-get info :tracking-marker)
                    (plist-get info :position)
                    (point)))
         (partial (format "Sub-agent result for task: %s\n\n" description)))
    (if invalid-tools
        (funcall main-cb
                 (format "Error: Failed to spawn sub-agent. You attempted to delegate unauthorized tools: %S. You may only delegate from your currently active tools: %S."
                         invalid-tools (delete "agent" parent-tool-names)))
      (let* ((system-prompt
              (concat
               "You are a specialized sub-agent. Your goal is: " description "\n\n"
               "Instructions and constraints:\n"
               prompt "\n\n"
               "You must only use the tools explicitly provided to you. "
               "When your task is complete, summarize your findings or provide your final result."))
             (parent-confirm gptel-confirm-tool-calls)
             (parent-model gptel-model)
             (parent-backend gptel-backend)
             (preset-spec (list :system system-prompt
                                :tools validated-tools
                                :model parent-model
                                :backend parent-backend
                                :confirm-tool-calls parent-confirm)))
        (gptel--update-status " Spawning Sub-agent..." 'font-lock-escape-face)
        (gptel-with-preset preset-spec
          (gptel-request "Please execute your assigned task and report back."
            :context (my/gptel-agent--task-overlay where description)
            :fsm (gptel-make-fsm :table gptel-send--transitions
                                 :handlers my/gptel-agent-request--handlers)
            :transforms (list #'gptel--transform-add-context)
            :callback
            (lambda (resp info)
              (let ((ov (plist-get info :context)))
                (pcase resp
                  ('nil
                   (when-let ((timer (overlay-get ov 'timer))) (cancel-timer timer))
                   (delete-overlay ov)
                   (funcall main-cb
                            (format "Error: Sub-agent failed to complete \"%s\".\nDetails: %S"
                                    description (plist-get info :error))))
                  (`(tool-call . ,calls)
                   (unless (plist-get info :tracking-marker)
                     (plist-put info :tracking-marker where))
                   (gptel--display-tool-calls calls info))
                  ((pred stringp)
                   (setq partial (concat partial resp))
                   (unless (plist-get info :tool-use)
                     (when-let ((timer (overlay-get ov 'timer))) (cancel-timer timer))
                     (delete-overlay ov)
                     (when-let* ((transformer (plist-get info :transformer)))
                       (setq partial (funcall transformer partial)))
                     (funcall main-cb partial)))
                  ('abort
                   (when-let ((timer (overlay-get ov 'timer))) (cancel-timer timer))
                   (delete-overlay ov)
                   (funcall main-cb
                            (format "Error: Sub-agent task \"%s\" was aborted." description))))))))))))

(provide 'custom-gptel-agents)
;;; custom-gptel-agents.el ends here
