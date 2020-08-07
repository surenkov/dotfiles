;;; lsp-pyright.el --- The lsp-mode client for Microsoft python-language-server -*- lexical-binding: t -*-

;; Author: Savva Surenkov
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (cl-lib "0.6.1") (lsp-mode "6.0"))
;; Homepage: https://github.com/surenkov/pyright
;; Keywords: languages tools


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; from https://vxlabs.com/2018/11/19/configuring-emacs-lsp-mode-and-microsofts-visual-studio-code-python-language-server/

;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'lsp-mode)
(require 'json)
(require 'projectile nil 'noerror)
(require 'find-file-in-project nil 'noerror)

;; Forward declare functions
(declare-function ffip-get-project-root-directory "ext:find-file-in-project")

;; Forward declare variable
(defvar lsp-render-markdown-markup-content)

;; Group declaration
(defgroup lsp-pyright nil
  "LSP support for python using the Microsoft Pyright."
  :group 'lsp-mode
  :link '(url-link "https://github.com/microsoft/pyright"))

(defcustom lsp-pyright-dir (f-join lsp-server-install-dir "ms.pyright")
  "The directory of the Microsoft Pyright."
  :type 'directory
  :group 'lsp-pyright)

(defcustom lsp-pyright-typeshed-dir (f-join lsp-pyright-dir "typeshed")
  "Directory with typeshed info"
  :type 'directory
  :group 'lsp-pyright)

(defcustom lsp-pyright-extra-paths []
  "A list of additional paths to search for python packages.
This should be a list of paths corresponding to additional python
library directories you want to search for completions.  Paths
should be as they are (or would appear) in sys.path.  Paths will
be prepended to the search path, and so will shadow duplicate
names in search paths returned by the interpreter."
  :type 'lsp-string-vector
  :group 'lsp-pyright)
(make-variable-buffer-local 'lsp-pyright-extra-paths)

(defcustom lsp-pyright-python-executable-cmd "python"
  "Command to specify the Python command for the Pyright.
Similar to the `python-shell-interpreter', but used only with mspyls.
Useful when there are multiple python versions in system.
e.g, there are `python2' and `python3', both in system PATH,
and the default `python' links to python2,
set as `python3' to let ms-pyls use python 3 environments."
  :type 'string
  :group 'lsp-pyright)

(defcustom lsp-pyright-cmd '("node"
                             "/Users/surenkov/.vscode/extensions/ms-python.vscode-pylance-2020.8.0/server/server.bundle.js"
                             "--stdio")

  "Path to the Pyright binary."
  :group 'lsp-pyright)

(defcustom lsp-pyright-auto-install-typeshed t
  "Install typeshed automatically."
  :type 'boolean
  :group 'lsp-pyright)

(defcustom lsp-pyright-type-checking-mode "basic"
  "Specifies the default rule set to use."
  :type '(choice
          (const "off")
          (const "basic")
          (const "strict"))
  :type 'string
  :group 'lsp-pyright)

(defcustom lsp-pyright-parse-dot-env-enabled t
  "Automatically parse .env file in the project root if non-nil."
  :type 'boolean
  :group 'lsp-pyright)

(defcustom lsp-pyright-typeshed-repo "https://github.com/python/typeshed.git"
  "The base url to get nupkg package. The alternative is `https://pvsc.azureedge.net'."
  :type 'string
  :group 'lsp-pyright)

(defcustom lsp-pyright-extra-major-modes '()
  "A list of additional major modes in which to activate.
  In addition to the python-mode, you may wish the Microsoft Python
  Language Server to activate in other major modes. If so, list them
  here."
  :type 'list
  :group 'lsp-pyright)

(defun lsp-pyright--install-typeshed (_client callback error-callback)
  "Downloading Pyright to the specified path."
  (let* ((install-dir (expand-file-name lsp-pyright-typeshed-dir)))

    (when (f-exists? install-dir)
      (lsp--info "Cleaning up previous Python Typeshed installation...")
      (f-delete install-dir t)
      (lsp--info "Cleaning up previous Python Typeshed installation...done"))

    (lsp--info "Downloading Python Typeshed...")

    (lsp-async-start-process
     (lambda ()
       (lsp--info "Downloading Python Typeshed...done")
       (and lsp-mode (lsp))
       (funcall callback))
     error-callback
     "git"
     "clone"
     "--depth=1"
     lsp-pyright-typeshed-repo
     lsp-pyright-typeshed-dir)))

;;;###autoload
(defun lsp-pyright-install-typeshed ()
  "Update Pyright.
  On Windows, if the server is running, the updating will fail.
  After stopping or killing the process, retry to update."
  (interactive)
  (lsp-pyright--install-typeshed nil #'ignore #'lsp--error))

(defun lsp-pyright-locate-python ()
  "Look for virtual environments local to the workspace"
  (let* ((sys-python (executable-find lsp-pyright-python-executable-cmd))
         (pipenv (and (executable-find "pipenv")
                      (locate-dominating-file default-directory "Pipfile")))
         (venv (locate-dominating-file default-directory "venv/"))
         (venv-python (f-expand "venv/bin/python" venv)))
    (cond
     ((and venv (f-executable? venv-python)) venv-python)
     (pipenv (string-trim-right (shell-command-to-string "pipenv --py")))
     (sys-python))))


(defun lsp-pyright--locate-pipenv-venv (root)
  "Locate current venv name from pipenv --venv output"
  (let* ((pipenv (executable-find "pipenv"))
         (pipfile (locate-dominating-file root "Pipfile")))
    (when (and pipenv pipfile)
      (file-name-nondirectory
        (string-trim-right (shell-command-to-string "pipenv --venv"))))))

;; it's crucial that we send the correct Python version to MS PYLS,
;; else it returns no docs in many cases furthermore, we send the
;; current Python's (can be virtualenv) sys.path as searchPaths
(defun lsp-pyright--get-python-ver-and-syspath (workspace-root)
  "Return list with pyver-string and list of python search paths.
  The WORKSPACE-ROOT will be prepended to the list of python search
  paths and then the entire list will be json-encoded."
  (when-let ((python (lsp-pyright-locate-python))
             (default-directory workspace-root)
             (init "from __future__ import print_function; import sys; \
  sys.path = list(filter(lambda p: p != '', sys.path)); import json;")
             (ver "v=(\"%s.%s\" % (sys.version_info[0], sys.version_info[1]));")
             (sp (concat "sys.path.insert(0, '" workspace-root "'); p=sys.path;"))
             (ex "e=sys.executable;")
             (val "print(json.dumps({\"version\":v,\"paths\":p,\"executable\":e}))"))
    (with-temp-buffer
      (call-process python nil t nil "-c" (concat init ver sp ex val))
      (let* ((json-array-type 'vector)
             (json-key-type 'string)
             (json-object-type 'hash-table)
             (json-string (buffer-string))
             (json-hash (json-read-from-string json-string)))
        (list (gethash "version" json-hash)
              (gethash "paths" json-hash)
              (gethash "executable" json-hash))))))

(defun lsp-pyright--workspace-root ()
  "Get the path of the root of the current workspace.
Use `lsp-workspace-root', which is pressent in the \"new\"
lsp-mode and works when there's an active session.  Next try ffip
or projectile, or just return `default-directory'."
  (cond
   ((fboundp #'lsp-workspace-root) (lsp-workspace-root))
   ((fboundp #'ffip-get-project-root-directory) (ffip-get-project-root-directory))
   ((fboundp #'projectile-project-root) (projectile-project-root))
   ((fboundp #'project-current) (when-let ((project (project-current)))
                                  (car (project-roots project))))
   (t default-directory)))

;; I based most of this on the vs.code implementation:
;; https://github.com/microsoft/vscode-python/blob/master/src/client/activation/languageServer/analysisOptions.ts
;; (it still took quite a while to get right, but here we are!)
(defun lsp-pyright--extra-init-params (&optional workspace)
  "Return form describing parameters for language server.
Old lsp will pass in a WORKSPACE, new lsp has a global
lsp-workspace-root function that finds the current buffer's
workspace root.  If nothing works, default to the current file's
directory"
  (let* ((workspace-root (if workspace (lsp--workspace-root workspace) (lsp-pyright--workspace-root)))
         (venv-path (expand-file-name "~/.local/share/virtualenvs"))
         (pipenv-venv-dir (lsp-pyright--locate-pipenv-venv workspace-root)))

    (when lsp-pyright-parse-dot-env-enabled
      (lsp-pyright--parse-dot-env workspace-root))
    (cl-destructuring-bind (pyver pysyspath _pyintpath)
        (lsp-pyright--get-python-ver-and-syspath workspace-root)
      `(
        :root ,workspace-root
        :pythonVersion ,pyver

        :typeCheckingMode ,lsp-pyright-type-checking-mode
        :venvPath ,venv-path
        :venv ,pipenv-venv-dir

        :extraPaths ,(vconcat lsp-pyright-extra-paths pysyspath)))))
        ;:typeshedPath ,(expand-file-name lsp-pyright-typeshed-dir)))))

(defun lsp-pyright--filter-nbsp (str)
  "Filter nbsp entities from STR."
  (let ((rx "&nbsp;"))
    (when (eq system-type 'windows-nt)
      (setq rx (concat rx "\\|\r")))
    (when str
      (replace-regexp-in-string rx " " str))))

(defun lsp-pyright--parse-dot-env (root &optional envvar)
  "Set environment variable (default PYTHONPATH) from .env file if this file exists in the project root."
  (let* ((envvar (or envvar "PYTHONPATH"))
         (file (f-join (file-name-as-directory root) ".env"))
         (rx (concat "^[:blank:]*" envvar "[:blank:]*=[:blank:]*"))
         val)
    (when (and (f-exists? file) (f-file? file) (f-readable? file))
      (with-temp-buffer
        (insert-file-contents file)
        (keep-lines rx (point-min) (point-max))
        (when (string-match (concat rx "\\(.*\\)") (buffer-string))
          (setq val (match-string 1 (buffer-string)))
          (unless (string-empty-p val)
            (setenv envvar val)))))))

;; this gets called when we do lsp-describe-thing-at-point
;; see lsp-methods.el. As always, remove Microsoft's unwanted entities :(
(setq lsp-render-markdown-markup-content #'lsp-pyright--filter-nbsp)

;; lsp-ui-doc--extract gets called when hover docs are requested
;; as always, we have to remove Microsoft's unnecessary &nbsp; entities
(advice-add 'lsp-ui-doc--extract
            :filter-return #'lsp-pyright--filter-nbsp)

;; lsp-ui-sideline--format-info gets called when lsp-ui wants to show
;; hover info in the sideline again &nbsp; has to be removed
(advice-add 'lsp-ui-sideline--format-info
            :filter-return #'lsp-pyright--filter-nbsp)

(defun lsp-python-ms--pyright-handle-completion (resp)
  (cond
   ((ht? resp) (ht-set resp "isIncomplete" (ht-get resp "isIncomplete" :json-false))))
  resp)

(dolist (mode lsp-pyright-extra-major-modes)
  (add-to-list 'lsp-language-id-configuration `(,mode . "python")))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection (lambda () lsp-pyright-cmd))
  :major-modes (append '(python-mode) lsp-pyright-extra-major-modes)
  :server-id 'mspyright
  :priority 2
  :initialization-options 'lsp-pyright--extra-init-params
  :response-handlers (lsp-ht ("textDocument/completion"
                              'lsp-python-ms--pyright-handle-completion))
  :notification-handlers (lsp-ht ("pyright/reportProgress" 'ignore)
                                 ("pyright/beginProgress" 'ignore)
                                 ("pyright/endProgress" 'ignore))
  :initialized-fn (lambda (workspace)
                    (with-lsp-workspace workspace
                      (lsp--set-configuration (lsp-configuration-section "python"))))
  :download-server-fn (lambda (client callback error-callback _update?)
                        (when lsp-pyright-auto-install-typeshed
                          (lsp-pyright--install-typeshed client callback error-callback)))))

(provide 'lsp-pyright)
