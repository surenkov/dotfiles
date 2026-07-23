;;; custom-gptel-skills.el --- Methodology skills for gptel -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'gptel)

(declare-function doom-project-root "doom-lib")

(defvar my/gptel-skill-dirs
  '("~/Projects/dotfiles/doom/my/skills/"
    ".claude/skills/"
    ".agents/skills/")
  "List of directories where skills are located.
Can be absolute or relative to the project root.")

(defvar my/gptel--skills nil
  "Alist of known skills: (name . (dir-path . skill-plist))")

(defvar my/gptel--skills-last-resolved-dirs nil
  "List of resolved skill directories from the last scan.")

(defun my/strip-quotes (str)
  "Strip leading and trailing single or double quotes from STR."
  (when str
    (let ((s (string-trim str)))
      (if (and (> (length s) 1)
               (or (and (= (aref s 0) ?\") (= (aref s (1- (length s))) ?\"))
                   (and (= (aref s 0) ?\') (= (aref s (1- (length s))) ?\'))))
          (substring s 1 (1- (length s)))
        s))))

(defun my/parse-skill-frontmatter (file-path)
  "Parse metadata from frontmatter of FILE-PATH.
Returns a plist with :name and :description if found."
  (when (and (file-readable-p file-path) (file-regular-p file-path))
    (with-temp-buffer
      (insert-file-contents file-path)
      (goto-char (point-min))
      (when (re-search-forward "^---[ \t]*\r?$" nil t)
        (let ((start (point))
              (end nil))
          (when (re-search-forward "^---[ \t]*\r?$" nil t)
            (setq end (match-beginning 0))
            (let ((frontmatter (buffer-substring-no-properties start end))
                  (name nil)
                  (desc nil))
              (with-temp-buffer
                (insert frontmatter)
                (goto-char (point-min))
                (let ((case-fold-search t))
                  (while (not (eobp))
                    (let ((line (string-trim (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
                      (cond
                       ((string-match "^name:[ \t]*\\(.*\\)" line)
                        (setq name (my/strip-quotes (match-string 1 line))))
                       ((string-match "^description:[ \t]*\\(.*\\)" line)
                        (let ((val (my/strip-quotes (match-string 1 line))))
                          (cond
                           ((member val '(">" "|"))
                            (let ((desc-lines nil))
                              (forward-line 1)
                              (while (and (not (eobp))
                                          (or (looking-at "^[ \t]+.*$")
                                              (looking-at "^[ \t]*\r?$")))
                                (push (string-trim (buffer-substring-no-properties (line-beginning-position) (line-end-position))) desc-lines)
                                (forward-line 1))
                              (forward-line -1)
                              (setq desc (mapconcat #'identity (nreverse desc-lines) " "))))
                           (t
                            (setq desc val)))))))
                    (forward-line 1))))
              (let ((plist nil))
                (when name (setq plist (plist-put plist :name name)))
                (when desc (setq plist (plist-put plist :description desc)))
                plist))))))))

(defun my/get-resolved-skill-dirs ()
  "Return absolute paths of existing skill directories.
Resolved against the current project root."
  (let ((root (doom-project-root)))
    (cl-loop for dir in my/gptel-skill-dirs
             for abs-dir = (expand-file-name dir root)
             when (file-directory-p abs-dir)
             collect (file-name-as-directory abs-dir))))

(defun my/gptel--path-in-dirs-p (path allowed-dirs)
  "Return non-nil if PATH is under one of ALLOWED-DIRS.
Resolves symlinks to ensure accuracy."
  (let ((real-path (file-truename path)))
    (cl-some (lambda (dir)
               (string-prefix-p (file-truename dir) real-path))
             allowed-dirs)))

(defun my/gptel-update-skills (&optional force)
  "Scan `my/gptel-skill-dirs` and update `my/gptel--skills` alist.
If FORCE is nil and skills are already cached, returns the cached value.
Iterates from lowest to highest priority to allow proper shadowing."
  (interactive "P")
  (let ((current-dirs (my/get-resolved-skill-dirs)))
    (when (or force
              (null my/gptel--skills)
              (not (equal current-dirs my/gptel--skills-last-resolved-dirs)))
      (setq my/gptel--skills nil)
      (setq my/gptel--skills-last-resolved-dirs current-dirs)
      (let ((allowed-dirs current-dirs))
        (dolist (dir allowed-dirs)
          (when (file-directory-p dir)
            (dolist (skill-file (directory-files-recursively dir "SKILL\\.md$" nil nil nil))
              (when (my/gptel--path-in-dirs-p skill-file allowed-dirs)
                (when-let* ((plist (my/parse-skill-frontmatter skill-file))
                            (name (plist-get plist :name)))
                  (setf (alist-get name my/gptel--skills nil nil #'string-equal)
                        (cons (file-name-directory skill-file) plist))))))))
      (setq my/gptel--skills (sort my/gptel--skills (lambda (a b) (string< (car a) (car b)))))))
  (my/gptel-skills-update-tool-schema-internal)
  my/gptel--skills)

(defun my/gptel-skills-update-tool-schema-internal ()
  (let ((tool (ignore-errors (gptel-get-tool "skill"))))
    (when tool
      (if my/gptel--skills
          (let* ((enum-vals (vconcat (mapcar #'car my/gptel--skills)))
                 (formatted-desc
                  (format "The name of the skill to retrieve instructions for. Available skills:\n%s"
                          (mapconcat (lambda (s) (format "- %s: %s"
                                                         (car s)
                                                         (or (plist-get (cdr (cdr s)) :description)
                                                             "(no description)")))
                                     my/gptel--skills "\n"))))
            (setf (gptel-tool-args tool)
                  `((:name "name"
                     :type "string"
                     :description ,formatted-desc
                     :enum ,enum-vals))))
        (setf (gptel-tool-args tool)
              '((:name "name"
                 :type "string"
                 :description "The name of the skill to retrieve instructions for."
                 :optional t)))))))

(defun my/gptel-skills-update-tool-schema-advice (&rest _args)
  (my/gptel-update-skills))

(defun my/gptel-skills-after-save-hook ()
  (when (and buffer-file-name (string-suffix-p "SKILL.md" buffer-file-name))
    (let ((allowed-dirs (my/get-resolved-skill-dirs)))
      (when (my/gptel--path-in-dirs-p buffer-file-name allowed-dirs)
        (my/gptel-update-skills t)
        (message "Updated gptel skills tool schema.")))))

(with-eval-after-load 'gptel
  (advice-add 'gptel-request :before #'my/gptel-skills-update-tool-schema-advice)
  (add-hook 'after-save-hook #'my/gptel-skills-after-save-hook))

(defun my/gptel-skills-system-message ()
  "Parse known skills and return the XML block describing them."
  (let ((skills (my/gptel-update-skills)))
    (if (null skills)
        ""
      (concat "Load a skill to get detailed instructions for a specific task.\n"
              "Use this when a task matches an available skill's description.\n"
              "<available_skills>\n"
              (mapconcat (lambda (skill-def)
                           (let* ((name (car skill-def))
                                  (plist (cdr (cdr skill-def)))
                                  (desc (plist-get plist :description)))
                             (format "  <skill>\n    <name>%s</name>\n    <description>%s</description>\n  </skill>"
                                     name desc)))
                         skills "\n")
              "\n</available_skills>"))))

(defun my/gptel-get-skill-body (skill)
  "Return body of SKILL with relative script/resource paths rewritten to absolute."
  (let ((skill-def (alist-get skill my/gptel--skills nil nil #'string-equal))
        (allowed-dirs (my/get-resolved-skill-dirs)))
    (if (not skill-def)
        (format "Error: skill '%s' not found." skill)
      (let* ((skill-dir (car skill-def))
             (skill-dir-expanded (expand-file-name skill-dir))
             (skill-file (expand-file-name "SKILL.md" skill-dir-expanded)))
        (cond
         ((not (file-exists-p skill-file))
          (format "Error: SKILL.md not found in %s." skill-dir-expanded))
         ((not (my/gptel--path-in-dirs-p skill-file allowed-dirs))
          (format "Error: Security violation - skill file %s is outside allowed directories." skill-file))
         (t
          (let* ((all-files (directory-files-recursively skill-dir-expanded ".*" nil nil nil))
                 (skill-files (cl-loop for fp in all-files
                                       when (my/gptel--path-in-dirs-p fp allowed-dirs)
                                       collect (cons (file-relative-name fp skill-dir-expanded) fp)))
                 (sorted-skill-files (sort skill-files (lambda (a b) (> (length (car a)) (length (car b))))))
                 (body (with-temp-buffer
                         (insert-file-contents skill-file)
                         (goto-char (point-min))
                         (when (looking-at-p "^---[ \t]*\r?$")
                           (forward-line 1)
                           (when (re-search-forward "^---[ \t]*\r?$" nil t)
                             (forward-line 1)))
                         (buffer-substring-no-properties (point) (point-max)))))
            (if (and body (not (string-empty-p body)))
                (with-temp-buffer
                  (insert "## Skill: " skill "\n- base dir: " skill-dir-expanded "\n\n")
                  (let ((start (point)))
                    (insert body)
                    (pcase-dolist (`(,rel-path . ,full-path) sorted-skill-files)
                      (let* ((prefix-regex "\\(?:file:///\\|file://\\|file:\\|@\\(?:\\.\\./\\|\\./\\|/\\)?\\|\\(?:\\.\\./\\)+\\|\\./\\|/\\)?")
                             (regexp (concat "\\(?:^\\|[^a-zA-Z0-9_./-]\\)\\("
                                             prefix-regex
                                             (regexp-quote rel-path)
                                             "\\)\\(?:$\\|[^a-zA-Z0-9_./-]\\|\\.\\(?:$\\|[^a-zA-Z0-9_-]\\)\\)")))
                        (goto-char start)
                        (while (re-search-forward regexp nil t)
                          (replace-match (expand-file-name full-path) t t nil 1)))))
                  (buffer-string))
              (format "Error: Could not load body of skill %s" skill)))))))))

(provide 'custom-gptel-skills)
;;; custom-gptel-skills.el ends here
