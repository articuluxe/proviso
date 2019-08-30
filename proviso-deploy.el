;;; proviso-deploy.el --- Deploy artifacts to locations
;; Copyright (C) 2018-2019  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Wednesday, September 12, 2018
;; Version: 1.0
;; Modified Time-stamp: <2019-08-30 09:00:21 dharms>
;; Modified by: Dan Harms
;; Keywords: tools proviso projects
;; URL: https://github.com/articuluxe/proviso.git
;; Package-Requires: ((emacs "24.4"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Deploy things to places.
;;

;;; Code:
(require 'proviso-core)
(require 'xfer)
(require 'proviso-gui)
(require 'seq)
(require 'dash)
(require 'ivy)
(require 'diff)
(require 'ediff-diff)
(require 'cl-lib)

(defvar-local proviso-deploy-buffer-name nil
  "Buffer name for `proviso-deploy' mode.")

(defconst proviso-deploy-buffer-name-prefix
  "*%s-deploy*"
  "Buffer prefix string for `proviso-deploy'.
This will be formatted with the project name.")

(defconst proviso-deploy-subdir ".deployments/"
  "Subdirectory within scratch-dir to store deployment files.")

(defun proviso-deploy--execute (source dest &optional sync)
  "Execute a deployment from SOURCE to DEST.
Optional SYNC is non-nil to prevent a background process from
being spawned to execute the file transfer."
  (let ((src (substitute-env-vars source t))
        (dst (substitute-env-vars dest t)))
    (message "Deploying %s to %s..." src dst)
    (if sync
        (xfer-transfer-file src dst)
      (xfer-transfer-file-async src dst))))

(defun proviso-deploy-one (spec subid &optional synchronous)
  "Execute a deployment represented by SPEC.
SUBID should reference an actual sub-deployment (see
:real-sources).  Usually the deployment occurs asynchronously; if
optional SYNCHRONOUS is non-nil, another process will not be
spawned."
  (let ((type (plist-get spec :type))
        cmd src dst sources)
    (cond ((eq type 'command)
           (setq cmd (substitute-env-vars
                      (plist-get spec :command) t))
           (shell-command cmd))
          ((eq type 'deploy)
           (setq sources (plist-get spec :real-sources))
           (if (eq subid t)
               (dolist (source sources)
                 (proviso-deploy--execute
                  (cdr source)
                  (plist-get spec :destination)
                  synchronous))
             (proviso-deploy--execute
              (alist-get subid sources) ;TODO subid is 1, not 0
              (plist-get spec :destination)
              synchronous))))))

(defun proviso-deploy-all (specs)
  "Execute all deployments contained in SPECS."
  (mapc (lambda (spec)
          ;; we always run async, no need to spawn again
          (proviso-deploy-one spec t t))
        specs))

(defun proviso-deploy-create (source dest &optional id)
  "Add a deployment from SOURCE to DEST.
ID is an optional id."
  (interactive "FSource: \nFDestination: ")
  (set-text-properties 0 (- (length source) 1) nil source)
  (set-text-properties 0 (- (length dest) 1) nil dest)
  (if id
      (list :source source :destination dest :type 'deploy :id id)
    (list :source source :destination dest :type 'deploy)))

(defun proviso-deploy-create-cmd (cmd &optional id)
  "Add a deployment command CMD.
ID is an optional id."
  (interactive "sCommand: ")
  (if id
      (list :command cmd :type 'command :id id)
    (list :command cmd :type 'command)))

(defun proviso-deploy-create-env (cmd &optional id)
  "Add an environment directive CMD.
ID is an optional id."
  (interactive "sEnvironment export: ")
  (if id
      (list :env cmd :type 'env :id id)
    (list :env cmd :type 'env)))

(defun proviso-deploy-choose-deploy (specs &optional prompt)
  "Let user select a deployment from SPECS.
PROMPT is an optional prompt."
  (when specs
    (let* ((home (getenv "HOME"))
           (prompt (or prompt "Choose deployment: "))
           (max 0) len
           type cmd src dst
           (lst (mapcar (lambda (spec)
                          (setq type (plist-get spec :type))
                          (cond ((eq type 'command)
                                 (setq cmd (plist-get spec :command))
                                 (cons (substitute-env-vars cmd t)
                                       (plist-get spec :id)))
                                ((eq type 'env)
                                 (setq cmd (plist-get spec :env))
                                 (cons (substitute-env-vars cmd t)
                                       (plist-get spec :id)))
                                ((eq type 'deploy)
                                 (setq src (substitute-env-vars (plist-get spec :source) t))
                                 (setq dst (substitute-env-vars (plist-get spec :destination) t))
                                 (cons
                                  (if (string-empty-p home)
                                      (cons src dst)
                                    (cons
                                     (replace-regexp-in-string home "~" src)
                                     (replace-regexp-in-string home "~" dst)))
                                  (plist-get spec :id)))))
                        specs)))
      (dolist (elt lst)
        (when (consp (car elt))
          (setq len (string-width (caar elt)))
          (setq max (max len max))))
      (catch 'exit
        (ivy-read prompt
                  (mapcar
                   (lambda (elt)
                     (cons
                      (cond ((consp (car elt))
                             (format
                              (concat "%-"
                                      (format "%d" max)
                                      "s -> %s")
                              (car (car elt))
                              (cdr (car elt))))
                            ((stringp (car elt))
                             (substitute-env-vars (car elt) t)))
                      (cdr elt)))
                   lst)
                  :action (lambda (x)
                            (throw 'exit (cdr x)))
                  :caller 'proviso-deploy-choose-deploy
                  )))))

(defun proviso-deploy--write-to-current-buffer (specs)
  "Write deployment specification SPECS to current buffer."
  (let (deploys envs type)
    (dolist (spec specs)
      (setq type (plist-get spec :type))
      (cond ((eq type 'command)
             (setq deploys t))
            ((eq type 'deploy)
             (setq deploys t))
            ((eq type 'env)
             (setq envs t))))
    (insert "(")
    (when envs
      (insert "(env . (\n")
      (dolist (spec specs)
        (let ((type (plist-get spec :type))
              cmd)
          (when (eq type 'env)
            (setq cmd (plist-get spec :env))
            (prin1 cmd (current-buffer))
            (insert "\n"))))
      (insert "))"))
    (when deploys
      (when envs
        (insert "\n"))
      (insert "(deploy . (\n")
      (dolist (spec specs)
        (let ((type (plist-get spec :type))
              cmd src dst)
          (cond ((eq type 'command)
                 (setq cmd (plist-get spec :command))
                 (prin1 cmd (current-buffer))
                 (insert "\n"))
                ((eq type 'deploy)
                 (setq src (plist-get spec :source))
                 (setq dst (plist-get spec :destination))
                 (prin1 (cons src dst) (current-buffer))
                 (insert "\n")))))
      (insert "))"))
    (insert ")\n")))

(defun proviso-deploy-write-to-string (specs)
  "Return a string containing a deployment specification for SPECS."
  (with-temp-buffer
    (proviso-deploy--write-to-current-buffer specs)
    (buffer-string)))

(defun proviso-deploy--read-elt (elt &optional type)
  "Read an element ELT of type TYPE."
  (cond ((consp elt)
         (list :source (car elt)
               :destination (cdr elt)
               :type 'deploy))
        ((stringp elt)
         (if (eq type 'env)
             (list :env elt :type 'env)
           (list :command elt :type (or type 'command))))
        (t nil)))

(defun proviso-deploy--read-from-str (str &optional proj)
  "Read deployments from STR, destined for PROJ.
If PROJ is not supplied, no `:id' parameter will be present."
  (let (specs obj)
    (dolist (spec (car (read-from-string str)))
      (cond ((and (consp spec)
                  (eq (car spec) 'env))
             (dolist (elt (cdr spec))
               (when (setq obj (proviso-deploy--read-elt elt 'env))
                 (when proj
                   (setq obj
                         (append
                          `(:id ,(proviso-deploy-get-next-id proj))
                          obj)))
                 (add-to-list 'specs obj t))))
            ((and (consp spec)
                  (eq (car spec) 'deploy))
             (dolist (elt (cdr spec))
               (when (setq obj (proviso-deploy--read-elt elt))
                 (when proj
                   (setq obj
                         (append
                          `(:id ,(proviso-deploy-get-next-id proj))
                          obj)))
                 (add-to-list 'specs obj t))))
            (t
             (and
              (setq obj (proviso-deploy--read-elt spec 'command))
              (add-to-list 'specs obj t)))))
    specs))

(defun proviso-deploy-read-from-file (proj filename)
  "Read a deployment specification from FILENAME, intended for PROJ."
  (with-temp-buffer
    (insert-file-contents-literally filename)
    (proviso-deploy--read-from-str (buffer-string) proj)))

;;;###autoload
(defun proviso-deploy-save-file (&optional arg)
  "Save current deployments to file.
If ARG is non-nil, another project can be chosen."
  (interactive "P")
  (let* ((proj (if arg (proviso-choose-project)
                 (proviso-current-project))))
    (if proj
        (proviso-deploy--save-file proj)
      (user-error "No project"))))

(defun proviso-deploy-save-file-current-project ()
  "Save deployments from current project to file."
  (interactive)
  (let ((proj proviso-local-proj))
    (if proj
        (proviso-deploy--save-file proj)
      (user-error "No current project"))))

(defun proviso-deploy--save-file (proj)
  "Save deployments from PROJ to a file.
This is an internal helper function."
  (let ((scratch (proviso-get proj :scratch-dir))
        (store (proviso-get proj :deploy-file))
        (defaultname (concat
                      (proviso-get proj :project-name)
                      ".deploy"))
        (lst (proviso-get proj :deployments)))
    (unless store
      (make-directory (concat scratch proviso-deploy-subdir) t)
      (setq store
            (read-file-name "Save deployments to: "
                            (concat scratch proviso-deploy-subdir)
                            nil nil defaultname))
    (proviso-put proj :deploy-file store)
    (async-start
     `(lambda ()
        (setq inhibit-message t)
        (with-temp-buffer
          (insert ,(proviso-deploy-write-to-string lst))
          (write-file ,store))))))

(defun proviso-deploy-save-file-as-current-project ()
  "Save deployments from current project to new file."
  (interactive)
  (let ((proj proviso-local-proj))
    (if proj
        (proviso-deploy--save-file-as proj)
      (user-error "No project"))))

;;;###autoload
(defun proviso-deploy-save-file-as (&optional arg)
  "Save current deployments to a new file.
If ARG is non-nil, another project can be chosen."
  (interactive "P")
  (let ((proj (if arg (proviso-choose-project)
                (proviso-current-project))))
    (if proj
        (proviso-deploy--save-file-as proj)
      (user-error "No project"))))

(defun proviso-deploy--save-file-as (proj)
  "Save deployments from PROJ to a new file."
  (let ((scratch (proviso-get proj :scratch-dir))
        (store (proviso-get proj :deploy-file))
        (defaultname (concat
                      (proviso-get proj :project-name)
                      ".deploy"))
        (lst (proviso-get proj :deployments))
        file)
    (make-directory (concat scratch proviso-deploy-subdir) t)
    (setq file
          (read-file-name "Save deployments to: "
                          (concat scratch proviso-deploy-subdir)
                          nil nil defaultname))
    (if file
        (progn
          (proviso-put proj :deploy-file file)
          (async-start
           `(lambda ()
              (setq inhibit-message t)
              (with-temp-buffer
                (insert ,(proviso-deploy-write-to-string lst))
                (write-file ,file)))))
      (user-error "No file selected, not saving"))))

(defun proviso-deploy--file-predicate (file)
  "Return non-nil if FILE is a suitable deployment file."
  (or (file-directory-p file)
      (string-match-p "\\.deploy$" file)))

;;;###autoload
(defun proviso-deploy-open-file (&optional arg)
  "Load a deployment from file.
If ARG is non-nil, another project can be chosen."
  (interactive "P")
  (let* ((proj (if arg (proviso-choose-project)
                 (proviso-current-project)))
         (scratch (proviso-get proj :scratch-dir))
         specs file)
    (make-directory (concat scratch proviso-deploy-subdir) t)
    (setq file
          (read-file-name "Load deployment file: "
                          (concat scratch proviso-deploy-subdir)
                          nil t nil #'proviso-deploy--file-predicate))
    (if (and file
             (setq specs (proviso-deploy-read-from-file proj file)))
        (progn
          (proviso-put proj :deployments specs)
          (proviso-put proj :deploy-file file)))))

(defun proviso-deploy-get-next-id (proj)
  "Get the next :deploy-id from PROJ."
  (let ((id (or (proviso-get proj :deploy-id) 0)))
    (prog1
        (incf id)
      (proviso-put proj :deploy-id id))))

(defun proviso-deploy--get-deploy-by-id (specs which)
  "Fetch a deployment from SPECS by WHICH.
WHICH may be an id, of a cons cell (ID . SUBID), where SUBID
identifies a sub-deployment."
  (let ((id (if (consp which) (car which) which)))
    (catch 'found
      (dolist (spec specs)
        (when (eq id (plist-get spec :id))
          (throw 'found spec))))))

(defun proviso-deploy-get-deploy-by-id (proj which)
  "Fetch a deployment belonging to PROJ by WHICH.
WHICH could be an id, or a cons cell (ID . SUBID) identifying a
sub-deployment."
  (let ((specs (proviso-get proj :deployments)))
    (proviso-deploy--get-deploy-by-id specs which)))

(defun proviso-deploy-get-real-source-by-id (spec id)
  "Fetch the real source from SPEC by ID."
  (let ((srcs (plist-get spec :real-sources)))
    (alist-get id srcs)))

(defun proviso-deploy--process-env (proj)
  "Process environment variable directives from PROJ."
  (let ((specs (proviso-get proj :deployments)))
    (dolist (spec specs)
      (when (eq (plist-get spec :type) 'env)
        (push (plist-get spec :env) process-environment)))))

;;;###autoload
(defun proviso-deploy-add-deploy (&optional arg)
  "Add a deployment.
If ARG is non-nil, another project can be chosen."
  (interactive "P")
  (let ((proj (if arg (proviso-choose-project)
                (proviso-current-project))))
    (proviso-deploy--add-deploy proj)))

(defun proviso-deploy-add-deploy-current-project ()
  "Add a deployment to the current project."
  (let ((proj proviso-local-proj))
    (if proj
        (proviso-deploy--add-deploy proj)
      (user-error "No current project"))))

(defun proviso-deploy--add-deploy (proj)
  "Add a deployment to project PROJ."
  (let* ((specs (proviso-get proj :deployments))
         (spec (call-interactively
                #'proviso-deploy-create)))
    (if spec
        (progn
          (setq spec
                (append `(:id ,(proviso-deploy-get-next-id proj))
                        spec))
          (if specs
              (add-to-list 'specs spec t)
            (setq specs (list spec)))
          (proviso-put proj :deployments specs))
      (user-error "No deployment added"))))

;;;###autoload
(defun proviso-deploy-add-deploy-cmd (&optional arg)
  "Add a deployment command.
If ARG is non-nil, another project can be chosen."
  (interactive "P")
  (let* ((proj (if arg (proviso-choose-project)
                 (proviso-current-project))))
    (proviso-deploy--add-deploy-cmd proj)))

(defun proviso-deploy-add-deploy-cmd-current-project ()
  "Add a deployment command to the current project."
  (let ((proj proviso-local-proj))
    (if proj
        (proviso-deploy--add-deploy-cmd proj)
      (user-error "No current project"))))

(defun proviso-deploy--add-deploy-cmd (proj)
  "Add a deployment command to PROJ."
  (let* ((specs (proviso-get proj :deployments))
         (spec (call-interactively
                #'proviso-deploy-create-cmd)))
    (if spec
        (progn
          (setq spec
                (append `(:id ,(proviso-deploy-get-next-id proj))
                        spec))
          (if specs
              (add-to-list 'specs spec t)
            (setq specs (list spec)))
          (proviso-put proj :deployments specs))
      (user-error "No deployment command added"))))

;;;###autoload
(defun proviso-deploy-add-deploy-env (&optional arg)
  "Add an environment directive.
If ARG is non-nil, another project can be chosen."
  (interactive "P")
  (let* ((proj (if arg (proviso-choose-project)
                 (proviso-current-project))))
    (proviso-deploy--add-deploy-env proj)))

(defun proviso-deploy-add-deploy-env-current-project ()
  "Add an environment directive to the current project."
  (let ((proj proviso-local-proj))
    (if proj
        (proviso-deploy--add-deploy-env proj)
      (user-error "No current project"))))

(defun proviso-deploy--add-deploy-env (proj)
  "Add an environment directive to PROJ."
  (let* ((specs (proviso-get proj :deployments))
         (spec (call-interactively
                #'proviso-deploy-create-env)))
    (if spec
        (progn
          (setq spec
                (append `(:id ,(proviso-deploy-get-next-id proj))
                        spec))
          (if specs
              (add-to-list 'specs spec t)
            (setq specs (list spec)))
          (proviso-put proj :deployments specs))
      (user-error "No deployment environment directive added"))))

;;;###autoload
(defun proviso-deploy-run-deploy (&optional arg)
  "Run a deployment.
If ARG is non-nil, another project can be chosen."
  (interactive "P")
  (let* ((proj (if arg (proviso-choose-project)
                 (proviso-current-project)))
         (specs (proviso-get proj :deployments))
         id spec)
    (if specs
        (if (and (setq id (proviso-deploy-choose-deploy
                           specs
                           "Run deployment: "))
                 (setq spec
                       (proviso-deploy--get-deploy-by-id specs id)))
            (proviso-deploy--run-deploy
             spec
             (if (consp id) (cdr id) id))
          (user-error "No deployment chosen"))
      (user-error "No deployments"))))

(defun proviso-deploy--run-deploy-by-id (proj which)
  "Run a deployment from PROJ identified by WHICH.
WHICH may be a solitary id, which identifies a deployment, or a
cons cell (ID . TOKEN), where TOKEN is either a sub-deployment id
or the symbol t, which matches all sub-deployments."
  (let ((spec (proviso-deploy-get-deploy-by-id proj which)))
    (proviso-put proj :last-deploy which)
    (proviso-deploy--run-deploy spec
                                (if (consp which)
                                    (cdr which)
                                  nil))))

(defun proviso-deploy--run-deploy (spec &optional subid)
  "Run deployment SPEC.
Optional SUBID might describe a sub-deployment."
  (proviso-deploy-one spec subid))

;;;###autoload
(defun proviso-deploy-run-all-deploys (&optional arg)
  "Run all deployments.
If ARG is non-nil, another project can be chosen."
  (interactive "P")
  (let ((proj (if arg (proviso-choose-project)
                (proviso-current-project))))
    (if proj
        (proviso-deploy--run-all-deploys proj)
      (user-error "No project"))))

(defun proviso-deploy--run-all-deploys (proj)
  "Run all deployments from PROJ."
  (let* ((lst (proviso-get proj :deployments)))
    (async-start
     `(lambda ()
        (setq inhibit-message t)
        ,(async-inject-variables "load-path")
        (require 'proviso-deploy)
        (proviso-deploy-all (quote ,lst))))))

(defun proviso-deploy-run-all-deploys-current-project ()
  "Run all deployments in current project."
  (let ((proj proviso-local-proj))
    (if proj
        (proviso-deploy--run-all-deploys proj)
      (user-error "No current project"))))

;;;###autoload
(defun proviso-deploy-run-last (&optional arg)
  "Rerun the last deployment, if any.
If ARG is non-nil, another project can be chosen."
  (interactive "P")
  (let ((proj (if arg (proviso-choose-project)
                (proviso-current-project))))
    (if proj
        (proviso-deploy--run-last proj)
      (user-error "No project"))))

(defun proviso-deploy--run-last (proj)
  "Run last deployment, if any, from project PROJ."
  (let* ((last (proviso-get proj :last-deploy))
         (spec (proviso-deploy-get-deploy-by-id
                proj
                (if (consp last) (car last) last))))
    (if spec
        (proviso-deploy-one spec (if (consp last)
                                     (cdr last)
                                   last))
      (user-error "No prior deployment"))))

(defun proviso-deploy-run-last-current-project ()
  "Run the last deployment in current project, if any."
  (let ((proj proviso-local-proj))
    (if proj
        (proviso-deploy--run-last proj)
      (user-error "No current project"))))

;;;###autoload
(defun proviso-deploy-revert-file (&optional arg)
  "Revert a deployment.
If ARG is non-nil, another project can be chosen."
  (interactive "P")
  (let* ((proj (if arg (proviso-choose-project)
                 (proviso-current-project)))
         (store (proviso-get proj :deploy-file))
         (file (or store
                   (concat (proviso-get proj :project-name)
                           ".deploy")))
         (lst (proviso-deploy-read-from-file proj file)))
    (if lst
        (proviso-put proj :deployments lst)
      (user-error "No deployments read in from %s" file))))

;;;###autoload
(defun proviso-deploy-delete-deploy (&optional arg)
  "Select a deployment for deletion.
If ARG is non-nil, another project can be chosen."
  (interactive "P")
  (let* ((proj (if arg (proviso-choose-project)
                 (proviso-current-project)))
         (specs (proviso-get proj :deployments))
         id spec)
    (if specs
        (if (and (setq id (proviso-deploy-choose-deploy
                           specs "Delete deployment: "))
                 (setq spec
                       (proviso-deploy--get-deploy-by-id specs id)))
            (proviso-put
             proj :deployments
             (delete spec
                     (proviso-get proj :deployments)))
          (user-error "No deployment chosen"))
      (user-error "No deployments"))))

;;;###autoload
(defun proviso-deploy-delete-all-deploy (&optional arg)
  "Delete all active deployments.
If ARG is non-nil, another project can be chosen."
  (interactive "P")
  (let ((proj (if arg (proviso-choose-project)
                (proviso-current-project))))
    (if proj
        (proviso-deploy--delete-all proj)
      (user-error "No project"))))

(defun proviso-deploy--delete-all (proj)
  "Delete all deployments from project PROJ."
  (proviso-put proj :deployments nil))

(defun proviso-deploy-delete-all-current-project ()
  "Delete all deployments from current project."
  (let ((proj proviso-local-proj))
    (if proj
        (proviso-deploy--delete-all proj)
      (user-error "No current project"))))

;;;###autoload
(defun proviso-deploy-check-file (&optional arg)
  "Check a deployed file for alterations.
If ARG is non-nil, another project can be chosen."
  (interactive "P")
  (let* ((proj (if arg (proviso-choose-project)
                 (proviso-current-project)))
         (specs (proviso-get proj :deployments))
         id spec)
    (if specs
        (if (and (setq id (proviso-deploy-choose-deploy
                           specs
                           "Check deployment: "))
                 (setq spec
                       (proviso-deploy--get-deploy-by-id specs id)))
            (proviso-deploy--check-file-spec spec (cdr id))
          (user-error "No deployment chosen"))
      (user-error "No deployments"))))

(defun proviso-deploy--check-file-spec (spec &optional subid)
  "Check the deployment SPEC.
SUBID is an optional identifier for a sub-deployment."
  (if (eq (plist-get spec :type) 'deploy)
      (let* ((srcs (plist-get spec :real-sources))
             (src (if subid (alist-get subid srcs) (cdr (car srcs))))
             (dst (substitute-env-vars (plist-get spec :destination) t)))
        (setq src (substitute-env-vars src t))
        (and dst
             (file-directory-p dst)
             (setq dst (expand-file-name
                        (file-name-nondirectory src) dst)))
        (if (and (file-exists-p src)
                 (file-exists-p dst))
            (if (ediff-same-file-contents src dst)
                (message "Files are identical.")
              (let
                  ((choices '(?d ?e ?q))
                   (prompt
                    "Files are different; run [d]iff, [e]diff or [q]uit: ")
                   ch)
                (while (null ch)
                  (setq ch (read-char-choice prompt choices)))
                (cond ((eq ch ?d)
                       (diff src dst))
                      ((eq ch ?e)
                       (ediff src dst))
                      (t
                       (message "Diff aborted.")))))
          (user-error "One or more files do not exist")))
    (user-error "No files to compare")))

;;;###autoload
(defun proviso-deploy-diff-file (&optional arg)
  "Run diff against a deployed file.
If ARG is non-nil, another project can be chosen."
  (interactive "P")
  (let* ((proj (if arg (proviso-choose-project)
                 (proviso-current-project)))
         (specs (proviso-get proj :deployments))
         id spec)
    (if specs
        (if (and (setq id (proviso-deploy-choose-deploy
                           specs
                           "Diff deployment: "))
                 (setq spec
                       (proviso-deploy--get-deploy-by-id specs id)))
            (proviso-deploy--diff-file-spec spec (cdr id))
          (user-error "No deployment chosen"))
      (user-error "No deployments"))))

(defun proviso-deploy--diff-file-spec (spec &optional subid)
  "Diff the deployment SPEC.
SUBID is an optional identifier for a sub-deployment."
  (if (eq (plist-get spec :type) 'deploy)
      (let* ((srcs (plist-get spec :real-sources))
             (src (if subid (alist-get subid srcs) (cdr (car srcs))))
             (dst (substitute-env-vars (plist-get spec :destination) t)))
        (setq src (substitute-env-vars src t))
        (and dst
             (file-directory-p dst)
             (setq dst (expand-file-name
                        (file-name-nondirectory src) dst)))
        (if (and (file-exists-p src)
                 (file-exists-p dst))
            (diff src dst)
          (user-error "One or more files do not exist")))
    (user-error "No files to compare")))

;;;###autoload
(defun proviso-deploy-ediff-file (&optional arg)
  "Run ediff against a deployed file.
If ARG is non-nil, another project can be chosen."
  (interactive "P")
  (let* ((proj (if arg (proviso-choose-project)
                 (proviso-current-project)))
         (specs (proviso-get proj :deployments))
         id spec)
    (if specs
        (if (and (setq id (proviso-deploy-choose-deploy
                           specs
                           "Ediff deployment: "))
                 (setq spec
                       (proviso-deploy--get-deploy-by-id specs id)))
            (proviso-deploy--ediff-file-spec spec (cdr id))
          (user-error "No deployment chosen"))
      (user-error "No deployments"))))

(defun proviso-deploy--ediff-file-spec (spec &optional subid)
  "Ediff the deployment SPEC.
SUBID is an optional identifier for a sub-deployment."
  (if (eq (plist-get spec :type) 'deploy)
      (let* ((srcs (plist-get spec :real-sources))
             (src (if subid (alist-get subid srcs) (cdr (car srcs))))
             (dst (substitute-env-vars (plist-get spec :destination) t)))
        (setq src (substitute-env-vars src t))
        (and dst
             (file-directory-p dst)
             (setq dst (expand-file-name
                        (file-name-nondirectory src) dst)))
        (if (and (file-exists-p src)
                 (file-exists-p dst))
            (ediff-files src dst)
          (user-error "One or more files do not exist")))
    (user-error "No files to compare")))

;;;###autoload
(defun proviso-deploy-edit-deploy (&optional arg)
  "Edit a deployment command.
If ARG is non-nil, another project can be chosen."
  (interactive "P")
  (let* ((proj (if arg (proviso-choose-project)
                 (proviso-current-project)))
         (specs (proviso-get proj :deployments))
         id spec)
    (if specs
        (if (and (setq id (proviso-deploy-choose-deploy
                           specs
                           "Edit deployment: "))
                 (setq spec
                       (proviso-deploy--get-deploy-by-id specs id)))
            (proviso-deploy--edit-deploy-spec proj id)
          (user-error "No deployment chosen"))
      (user-error "No deployments"))))

(defun proviso-deploy--edit-deploy-spec (proj id)
  "Edit the deployment with ID, belonging to project PROJ."
  (let* ((specs (proviso-get proj :deployments))
         (spec (proviso-deploy-get-deploy-by-id proj id))
         (type (plist-get spec :type))
         cmd src dst
         (n (seq-position specs id
                          (lambda (lst elt)
                            (eq elt (plist-get lst :id))))))
    (cond ((eq type 'command)
           (setq cmd (plist-get spec :command))
           (setcar (nthcdr n specs)
                   (proviso-deploy-create-cmd
                    (read-string "New command: "
                                 cmd) id)))
          ((eq type 'env)
           (setq cmd (plist-get spec :env))
           (setcar (nthcdr n specs)
                   (proviso-deploy-create-env
                    (read-string "New environment directive: "
                                 cmd) id)))
          ((eq type 'deploy)
           (setq src (plist-get spec :source))
           (setq dst (plist-get spec :destination))
           (setq src
                 (if (string-match-p "\\$" src)
                     (read-string "New source: " src)
                   (read-file-name
                    "New source: "
                    (file-name-directory src)
                    nil nil
                    (file-name-nondirectory src))))
           (setq dst
                 (if (string-match-p "\\$" dst)
                     (read-string "New destination: " dst)
                   (read-file-name
                    "New destination: "
                    (file-name-directory dst)
                    nil nil
                    (file-name-nondirectory dst))))
           (setcar (nthcdr n specs)
                   (proviso-deploy-create src dst id))))))

;;;###autoload
(defun proviso-deploy-find-file (&optional arg)
  "Edit the deployed file.
If ARG is non-nil, another project can be chosen."
  (interactive "P")
  (let* ((proj (if arg (proviso-choose-project)
                 (proviso-current-project)))
         (specs (proviso-get proj :deployments))
         id spec)
    (if specs
        (if (and (setq id (proviso-deploy-choose-deploy
                           specs
                           "Find deployed file: "))
                 (setq spec (proviso-deploy--get-deploy-by-id specs id)))
            (proviso-deploy--find-file-spec spec (cdr id))
          (user-error "No deployment chosen"))
      (user-error "No deployments"))))

(defun proviso-deploy--find-file-spec (spec &optional subid)
  "Visit the deployment SPEC.
SUBID is an optional identifer for a sub-deployment."
  (if (eq (plist-get spec :type) 'deploy)
      (let* ((srcs (plist-get spec :real-sources))
             (src (if subid (alist-get subid srcs) (cdr (car srcs))))
             (dst (substitute-env-vars (plist-get spec :destination) t)))
        (setq src (substitute-env-vars src t))
        (if dst
            (progn
              (when (file-directory-p dst)
                (setq dst (expand-file-name
                           (file-name-nondirectory src) dst)))
              (if (file-exists-p dst)
                  (find-file dst)
                (user-error "File '%s' does not exist" dst)))
          (user-error "No deployed file to edit")))
    (user-error "Not a deployment")))

;;;###autoload
(defun proviso-deploy-find-file-other-window (&optional arg)
  "Visit the deployed file in another window.
If ARG is non-nil, another project can be chosen."
  (interactive "P")
  (let* ((proj (if arg (proviso-choose-project)
                 (proviso-current-project)))
         (specs (proviso-get proj :deployments))
         id spec)
    (if specs
        (if (and (setq id (proviso-deploy-choose-deploy
                           specs
                           "Find deployed file in other window: "))
                 (setq spec (proviso-deploy--get-deploy-by-id specs id)))
            (if (eq (plist-get spec :type) 'deploy)
                (let ((src (substitute-env-vars (plist-get spec :source) t))
                      (dst (substitute-env-vars (plist-get spec :destination) t)))
                  (if dst
                      (progn
                        (when (file-directory-p dst)
                          (setq dst (expand-file-name
                                     (file-name-nondirectory src) dst)))
                        (if (file-exists-p dst)
                            (find-file-other-window dst)
                          (user-error "File '%s' does not exist" dst)))
                    (user-error "No deployed file to edit")))
              (user-error "Not a deployment"))
          (user-error "No deployment chosen"))
      (user-error "No deployments"))))

(defun proviso-deploy-revert-buffer ()
  "Reverts (recreates) the deployment buffer."
  (interactive)
  (proviso-deploy-create-buffer (proviso-current-project)))

(defvar proviso-deploy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" #'proviso-deploy-revert-buffer)
    map)
  "Keymap for `proviso-deploy-mode'.")

(define-derived-mode proviso-deploy-mode special-mode
  "Deploy"
  "Major mode for deploying things to places.
\<proviso-deploy-mode-map>
"
  (setq buffer-read-only t)
  (setq truncate-lines t))

;;;###autoload
(defun proviso-deploy-show (&optional arg)
  "Show a deploy buffer.
Optional argument ARG allows choosing a project."
  (interactive "P")
  (let ((proj (if arg (proviso-choose-project)
                (proviso-current-project))))
    (if proj
        (progn
          (proviso-deploy-create-buffer proj)
          (pop-to-buffer proviso-deploy-buffer-name))
      (user-error "No project"))))

(defun proviso-deploy-move-deployment-up ()
  "Move the selected deployment up in the list, if possible."
  (if-let* ((cell (proviso-gui-find-current-cell))
            (id (cdr (assq 'parent-id cell)))
            (proj proviso-local-proj)
            (specs (proviso-get proj :deployments))
            (n (seq-position specs id
                             (lambda (lst elt)
                               (eq elt (plist-get lst :id)))))
            (p (if (> n 0) (1- n) n)))
      (progn
        (unless (eq n p)
          (setcar (nthcdr p specs)
                  (prog1
                      (nth n specs)
                    (setcar (nthcdr n specs)
                            (nth p specs))))))))

(defun proviso-deploy-move-deployment-down ()
  "Move the selected deployment down in the list, if possible."
  (if-let* ((cell (proviso-gui-find-current-cell))
            (id (cdr (assq 'parent-id cell)))
            (proj proviso-local-proj)
            (specs (proviso-get proj :deployments))
            (n (seq-position specs id
                             (lambda (lst elt)
                               (eq elt (plist-get lst :id)))))
            (o (if (< n (1- (length specs))) (1+ n) n)))
      (progn
        (unless (eq n o)
          (setcar (nthcdr o specs)
                  (prog1
                      (nth n specs)
                    (setcar (nthcdr n specs)
                            (nth o specs))))))))

(defun proviso-deploy-create-buffer (proj)
  "Create a deployment buffer for project PROJ."
  (interactive)
  (setq proviso-deploy-buffer-name
        (format proviso-deploy-buffer-name-prefix proj))
  (let ((buffer (get-buffer-create proviso-deploy-buffer-name))
        (width (string-width "Destination"))
        lst)
    (proviso-gui-init-buffer buffer proviso-deploy-mode-map)
    (with-current-buffer buffer
      (setq-local proviso-local-proj proj)
      (kill-local-variable 'process-environment)
      (proviso-deploy-mode)
      (make-local-variable 'process-environment)
      (proviso-deploy--process-env proj))
    (proviso-gui-add-global-cb
     buffer
     '(("s" "Save" proviso-deploy-save-file-current-project file)
       ("S" "Save as" proviso-deploy-save-file-as-current-project file)
       ("o" "Open file" proviso-deploy-open-file buffer)
       ;; TODO add Import file
       ("R" "Run all" proviso-deploy-run-all-deploys-current-project deployment)
       ("." "Run last" proviso-deploy-run-last-current-project deployment)
       ("X" "Delete all" proviso-deploy-delete-all-current-project buffer)
       ("+" "Add deployment" proviso-deploy-add-deploy-current-project buffer new)
       ("=" "Add command" proviso-deploy-add-deploy-cmd-current-project buffer new)
       ("-" "Add env" proviso-deploy-add-deploy-env-current-project buffer new)
       ("\M-p" "Move up" proviso-deploy-move-deployment-up buffer id)
       ("\M-n" "Move down" proviso-deploy-move-deployment-down buffer id)
       ))
    (setq width
          (proviso-gui-add-to-buffer
           buffer
           '((:heading "Project"
                       :category project
                       :content (lambda ()
                                  (propertize
                                   (proviso-get proviso-local-proj :project-name)
                                   'face 'highlight))
                       :section post)
             (:heading "File"
                       :category file
                       :content (lambda ()
                                  (let ((file (proviso-get proviso-local-proj :deploy-file)))
                                    (cond ((and file (file-exists-p file))
                                           (propertize
                                            (replace-regexp-in-string (getenv "HOME") "~" file)
                                            'face '(bold)))
                                          (file
                                           (propertize
                                            (replace-regexp-in-string (getenv "HOME") "~" file)
                                            'face '(shadow)))
                                          (t
                                           (propertize "None" 'face '(shadow)))))))
             ) width))                  ;TODO add a find file
    (dolist (spec (proviso-get proj :deployments))
      (let ((type (plist-get spec :type))
            command source dest deployments subid)
        (lexical-let ((id (plist-get spec :id)))
          (cond ((eq type 'command)
                 (setq command (plist-get spec :command))
                 (add-to-list 'lst
                              (list
                               :heading "Command"
                               :category 'command
                               :parent-id id
                               :content (lambda ()
                                          (let* ((spec (proviso-deploy-get-deploy-by-id proviso-local-proj id))
                                                 (cmd (plist-get spec :command)))
                                            (substitute-env-vars cmd t)))
                               :bindings `(("r" "Run"
                                            (lambda ()
                                              (proviso-deploy--run-deploy-by-id proviso-local-proj ,id)))
                                           ("t" "Edit"
                                            (lambda ()
                                              (proviso-deploy--edit-deploy-spec proviso-local-proj ,id))))
                               :section 'pre) t))
                ((eq type 'env)
                 (setq command (plist-get spec :env))
                 (add-to-list 'lst
                              (list
                               :heading "Environment"
                               :category 'buffer
                               :parent-id id
                               :content (lambda ()
                                          (let* ((spec (proviso-deploy-get-deploy-by-id proviso-local-proj id))
                                                 (cmd (plist-get spec :env)))
                                            (substitute-env-vars cmd t)))
                               :bindings `(("t" "Edit"
                                            (lambda ()
                                              (proviso-deploy--edit-deploy-spec proviso-local-proj ,id))))
                               :section 'pre) t))
                ((eq type 'deploy)
                 (setq source (plist-get spec :source))
                 (setq dest (plist-get spec :destination))
                 (cond ((string-match-p "[$^.*]" source)
                        (setq deployments (directory-files
                                           (file-name-directory source)
                                           t
                                           (file-name-nondirectory source))))
                       ((file-directory-p source)
                        (setq deployments (directory-files
                                           source t)))
                       (t (push source deployments)))
                 (let* ((indices (number-sequence 0 (1- (length deployments))))
                        (real-sources (mapcar (lambda (x)
                                                (cons x (nth x deployments)))
                                              indices)))
                   (plist-put spec :real-sources real-sources)
                   (dolist (elt real-sources)
                     (lexical-let ((subid (car elt)))
                       (add-to-list 'lst
                                    (list
                                     :heading "Source"
                                     :category 'deployment
                                     :parent-id id
                                     :content (lambda ()
                                                (let* ((spec (proviso-deploy-get-deploy-by-id proviso-local-proj
                                                                                              (cons id subid)))
                                                       (realsrc (proviso-deploy-get-real-source-by-id spec subid))
                                                       (src (substitute-env-vars realsrc t))
                                                       (home (getenv "HOME")))
                                                  (propertize
                                                   (if home
                                                       (replace-regexp-in-string home "~" src)
                                                     src)
                                                   'help-echo realsrc)))
                                     :bindings `(("r" "Run"
                                                  (lambda ()
                                                    (proviso-deploy--run-deploy-by-id proviso-local-proj (cons ,id ,subid))))
                                                 ("c" "Check"
                                                  (lambda ()
                                                    (let ((spec (proviso-deploy-get-deploy-by-id proviso-local-proj ,id)))
                                                      (proviso-deploy--check-file-spec spec ,subid))))
                                                 ("d" "Diff"
                                                  (lambda ()
                                                    (let ((spec (proviso-deploy-get-deploy-by-id proviso-local-proj ,id)))
                                                      (proviso-deploy--diff-file-spec spec ,subid))))
                                                 ("e" "Ediff"
                                                  (lambda ()
                                                    (let ((spec (proviso-deploy-get-deploy-by-id proviso-local-proj ,id)))
                                                      (proviso-deploy--ediff-file-spec spec ,subid))))
                                                 ("t" "Edit"
                                                  (lambda ()
                                                    (proviso-deploy--edit-deploy-spec proviso-local-proj ,id)))
                                                 )
                                     :section 'pre) t)
                       (add-to-list 'lst
                                    (list
                                     :category 'deployment
                                     :parent-id id
                                     :content (lambda ()
                                                (let* ((spec (proviso-deploy-get-deploy-by-id proviso-local-proj
                                                                                              (cons id subid)))
                                                       (realsrc (proviso-deploy-get-real-source-by-id spec subid))
                                                       (src (substitute-env-vars realsrc t))
                                                       (attr (file-attributes src)))
                                                  (propertize
                                                   (if (file-exists-p src)
                                                       (concat
                                                        (format-time-string
                                                         "%F %T"
                                                         (file-attribute-modification-time attr))
                                                        (format "%10s"
                                                                (proviso-deploy-human-readable-filesize
                                                                 (file-attribute-size attr)))
                                                        )
                                                     "---------- --:--:--        --")
                                                   'face '(shadow))))
                                     :bindings `(("r" "Run"
                                                  (lambda ()
                                                    (proviso-deploy--run-deploy-by-id proviso-local-proj (cons ,id ,subid))))
                                                 ("c" "Check"
                                                  (lambda ()
                                                    (let ((spec (proviso-deploy-get-deploy-by-id proviso-local-proj ,id)))
                                                      (proviso-deploy--check-file-spec spec ,subid))))
                                                 ("d" "Diff"
                                                  (lambda ()
                                                    (let ((spec (proviso-deploy-get-deploy-by-id proviso-local-proj ,id)))
                                                      (proviso-deploy--diff-file-spec spec ,subid))))
                                                 ("e" "Ediff"
                                                  (lambda ()
                                                    (let ((spec (proviso-deploy-get-deploy-by-id proviso-local-proj ,id)))
                                                      (proviso-deploy--ediff-file-spec spec ,subid))))
                                                 ("t" "Edit"
                                                  (lambda ()
                                                    (proviso-deploy--edit-deploy-spec proviso-local-proj ,id)))
                                                 )
                                     )
                                    t)
                       (add-to-list 'lst
                                    (list
                                     :heading "Destination"
                                     :category 'deployment
                                     :parent-id id
                                     :content (lambda ()
                                                (let* ((spec (proviso-deploy-get-deploy-by-id proviso-local-proj (cons id subid)))
                                                       (realsrc (proviso-deploy-get-real-source-by-id spec subid))
                                                       (src (substitute-env-vars realsrc t))
                                                       (dst (substitute-env-vars (plist-get spec :destination) t)))
                                                  (when (file-directory-p dst)
                                                    (setq dst (expand-file-name
                                                               (file-name-nondirectory src) dst)))
                                                  (propertize
                                                   (replace-regexp-in-string (getenv "HOME") "~" dst)
                                                   'help-echo (plist-get spec :destination))))
                                     :bindings `(("r" "Run"
                                                  (lambda ()
                                                    (proviso-deploy--run-deploy-by-id proviso-local-proj (cons ,id ,subid))))
                                                 ("c" "Check"
                                                  (lambda ()
                                                    (let ((spec (proviso-deploy-get-deploy-by-id proviso-local-proj ,id)))
                                                      (proviso-deploy--check-file-spec spec ,subid))))
                                                 ("d" "Diff"
                                                  (lambda ()
                                                    (let ((spec (proviso-deploy-get-deploy-by-id proviso-local-proj ,id)))
                                                      (proviso-deploy--diff-file-spec spec ,subid))))
                                                 ("e" "Ediff"
                                                  (lambda ()
                                                    (let ((spec (proviso-deploy-get-deploy-by-id proviso-local-proj ,id)))
                                                      (proviso-deploy--ediff-file-spec spec ,subid))))
                                                 ("t" "Edit"
                                                  (lambda ()
                                                    (proviso-deploy--edit-deploy-spec proviso-local-proj ,id)))
                                                 ("f" "find file"
                                                  (lambda ()
                                                    (let ((spec (proviso-deploy-get-deploy-by-id proviso-local-proj ,id)))
                                                      (proviso-deploy--find-file-spec spec ,subid))))
                                                 )) t)
                       (add-to-list 'lst
                                    (list
                                     :category 'deployment
                                     :parent-id id
                                     :content (lambda ()
                                                (let* ((spec (proviso-deploy-get-deploy-by-id proviso-local-proj (cons id subid)))
                                                       (realsrc (proviso-deploy-get-real-source-by-id spec subid))
                                                       (src (substitute-env-vars realsrc t))
                                                       (dst (substitute-env-vars (plist-get spec :destination) t))
                                                       attr)
                                                  (when (file-directory-p dst)
                                                    (setq dst (expand-file-name
                                                               (file-name-nondirectory src) dst)))
                                                  (setq attr (file-attributes dst))
                                                  (propertize
                                                   (if (file-exists-p dst)
                                                       (concat
                                                        (format-time-string
                                                         "%F %T"
                                                         (file-attribute-modification-time attr))
                                                        (format "%10s"
                                                                (proviso-deploy-human-readable-filesize
                                                                 (file-attribute-size attr))))
                                                     "---------- --:--:--        --")
                                                   'face '(shadow))))
                                     :bindings `(("r" "Run"
                                                  (lambda ()
                                                    (proviso-deploy--run-deploy-by-id proviso-local-proj (cons ,id ,subid))))
                                                 ("c" "Check"
                                                  (lambda ()
                                                    (let ((spec (proviso-deploy-get-deploy-by-id proviso-local-proj ,id)))
                                                      (proviso-deploy--check-file-spec spec ,subid))))
                                                 ("d" "Diff"
                                                  (lambda ()
                                                    (let ((spec (proviso-deploy-get-deploy-by-id proviso-local-proj ,id)))
                                                      (proviso-deploy--diff-file-spec spec ,subid))))
                                                 ("e" "Ediff"
                                                  (lambda ()
                                                    (let ((spec (proviso-deploy-get-deploy-by-id proviso-local-proj ,id)))
                                                      (proviso-deploy--ediff-file-spec spec ,subid))))
                                                 ("t" "Edit"
                                                  (lambda ()
                                                    (proviso-deploy--edit-deploy-spec proviso-local-proj ,id)))
                                                 ("f" "find file"
                                                  (lambda ()
                                                    (let ((spec (proviso-deploy-get-deploy-by-id proviso-local-proj ,id)))
                                                      (proviso-deploy--find-file-spec spec ,subid))))
                                                 )) t)
                       ))))))))
    (setq width (proviso-gui-add-to-buffer buffer lst width))
    (proviso-gui-finalize-buffer buffer)
    ))

(defconst proviso-deploy-filesize-prefixes [" " "K" "M" "G" ]
  "Prefix notations for different magnitudes of file sizes.")

(defun proviso-deploy-human-readable-filesize (size)
  "Return SIZE as human-readable string.
From `http://mbork.pl/2018-03-26_Human-readable_filesizes'."
  (let* ((order (1- (max 1 (ceiling (log (max size 1) 1024)))))
         (prefix (elt proviso-deploy-filesize-prefixes
                      (min order (length proviso-deploy-filesize-prefixes))))
         (size-in-unit (/ size (expt 1024.0 order)))
         (precision (max 3 (+ 2 (floor (log (max size-in-unit 1) 10)))))
         (str (format (format "%%.%dg% %sB" precision)
                      size-in-unit prefix)))
    str))

(provide 'proviso-deploy)
;;; proviso-deploy.el ends here
