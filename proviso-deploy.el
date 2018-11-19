;;; proviso-deploy.el --- deploy artifacts to locations
;; Copyright (C) 2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Wednesday, September 12, 2018
;; Version: 1.0
;; Modified Time-stamp: <2018-11-19 09:04:49 dharms>
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
(require 'proviso-transfer)
(require 'proviso-gui)
(require 'seq)
(require 'dash)
(require 'ivy)
(require 'diff)
(require 'ediff-diff)

(defvar-local proviso-deploy-buffer-name nil
  "Buffer name for `proviso-deploy' mode.")

(defconst proviso-deploy-buffer-name-prefix
  "*%s-deploy*"
  "Buffer prefix string for `proviso-deploy'.
This will be formatted with the project name.")

(defun proviso-deploy-one (spec)
  "Execute a deployment represented by SPEC."
  (let ((cmd (plist-get spec :command))
        (src (plist-get spec :source))
        (dst (plist-get spec :destination)))
    (if cmd
        (shell-command cmd)
      (message "Deploying %s to %s..." src dst)
      (proviso-transfer-file-async src dst))))

(defun proviso-deploy-all (specs)
  "Execute all deployments contained in SPECS."
  (dolist (spec specs) #'proviso-deploy-one))

(defun proviso-deploy-create (source dest)
  "Add a deployment from SOURCE to DEST."
  (interactive "FSource: \nFDestination: ")
  (list :source source :destination dest))

(defun proviso-deploy-create-cmd (cmd)
  "Add a deployment command CMD."
  (interactive "sCommand: ")
  (list :command cmd))

(defun proviso-deploy-choose-deploy (specs &optional prompt)
  "Let user select a deployment from SPECS.
PROMPT is an optional prompt."
  (when specs
    (let* ((home (getenv "HOME"))
           (prompt (or prompt "Choose deployment: "))
           lst
           (max 0) len
           cmd src dst)
      (setq lst (mapcar (lambda (spec)
                          (setq cmd (plist-get spec :command))
                          (setq src (plist-get spec :source))
                          (setq dst (plist-get spec :destination))
                          (cond (cmd
                                 (cons cmd spec))
                                ((and src dst)
                                 (cons
                                  (cons
                                   (replace-regexp-in-string home "~" src)
                                   (replace-regexp-in-string home "~" dst))
                                  spec))))
                        specs))
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
                             (car elt)))
                      (cdr elt)))
                   lst)
                  :action (lambda (x)
                            (throw 'exit (cdr x)))
                  :caller 'proviso-deploy-choose-deploy
                  )))))

(defun proviso-deploy--write-to-current-buffer (specs)
  "Write deployment specification SPECS to current buffer."
  (insert "((deploy . (\n")
  (dolist (spec specs)
    (let ((cmd (plist-get spec :command))
          (src (plist-get spec :source))
          (dst (plist-get spec :destination)))
      (cond (cmd
             (prin1 cmd (current-buffer)))
            (t
             (prin1 (cons src dst) (current-buffer))))
      (insert "\n")))
  (insert ")))\n"))

(defun proviso-deploy-write-to-file (filename specs)
  "Save a deployment specification SPECS to FILENAME."
  (with-temp-buffer
    (proviso-deploy--write-to-current-buffer specs)
    (write-file filename)))

(defun proviso-deploy--read-elt (elt)
  "Read an element ELT."
  (cond ((consp elt)
         (list :source (car elt)
               :destination (cdr elt)))
        ((stringp elt)
         (list :command elt))
        (t nil)))

(defun proviso-deploy--read-from-str (str)
  "Read deployments from STR."
  (let (specs obj)
    (dolist (spec (car (read-from-string str)))
      (cond ((and (consp spec)
                  (eq (car spec) 'deploy))
             (dolist (elt (cdr spec))
               (and
                (setq obj (proviso-deploy--read-elt elt))
                (add-to-list 'specs obj t))))
            (t
             (and
              (setq obj (proviso-deploy--read-elt spec))
              (add-to-list 'specs obj t)))))
    specs))

(defun proviso-deploy-read-from-file (filename)
  "Read a deployment specification from FILENAME."
  (with-temp-buffer
    (insert-file-contents-literally filename)
    (proviso-deploy--read-from-str
     (buffer-string))))

;;;###autoload
(defun proviso-deploy-save-file (&optional arg)
  "Save current deployments to file.
If ARG is non-nil, another project can be chosen."
  (interactive "P")
  (let* ((proj (if arg (proviso-choose-project)
                 (proviso-current-project)))
         (remote (proviso-get proj :remote-prefix))
         (root (proviso-get proj :root-dir))
         (store (proviso-get proj :deploy-file))
         (defaultfile (concat (or (proviso-get proj :project-name)
                                  "default")
                              ".deploy"))
         (lst (proviso-get proj :deployments)))
    (unless store
      (setq store
            (read-file-name "Save deployments to: "
                            (concat remote root)
                            nil nil defaultfile))
      (proviso-put proj :deploy-file store))
    (proviso-deploy-write-to-file store lst)))

;;;###autoload
(defun proviso-deploy-save-file-as (&optional arg)
  "Save current deployments to a new file.
If ARG is non-nil, another project can be chosen."
  (interactive "P")
  (let* ((proj (if arg (proviso-choose-project)
                 (proviso-current-project)))
         (remote (proviso-get proj :remote-prefix))
         (root (proviso-get proj :root-dir))
         (store (proviso-get proj :deploy-file))
         (defaultfile (or store
                          (concat (or (proviso-get proj :project-name)
                                      "default")
                                  ".deploy")))
         (lst (proviso-get proj :deployments))
         file)
    (setq file
          (read-file-name "Save deployments to: "
                          (concat remote root)
                          nil nil defaultfile))
    (if file
        (progn
          (proviso-put proj :deploy-file file)
          (proviso-deploy-write-to-file file lst))
      (user-error "No file selected, not saving"))))

(defun proviso-deploy--file-predicate (file)
  "Return non-nil if FILE is a suitable deployment file."
  (string-match-p "\\.deploy$" file))

;;;###autoload
(defun proviso-deploy-open-file (&optional arg)
  "Load a deployment from file.
If ARG is non-nil, another project can be chosen."
  (interactive "P")
  (let* ((proj (if arg (proviso-choose-project)
                 (proviso-current-project)))
         (remote (proviso-get proj :remote-prefix))
         (root (proviso-get proj :root-dir))
         specs file)
    (setq file
          (read-file-name "Load deployment file: "
                          (concat remote root)
                          nil t nil #'proviso-deploy--file-predicate))
    (if (and file
             (setq specs (proviso-deploy-read-from-file file)))
        (progn
          (proviso-put proj :deployments specs)
          (proviso-put proj :deploy-file file)))))

;;;###autoload
(defun proviso-deploy-add-deploy (&optional arg)
  "Add a deployment.
If ARG is non-nil, another project can be chosen."
  (interactive "P")
  (let* ((proj (if arg (proviso-choose-project)
                 (proviso-current-project)))
         (specs (proviso-get proj :deployments))
         (spec (call-interactively
                'proviso-deploy-create)))
    (if spec
        (progn
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
                 (proviso-current-project)))
         (specs (proviso-get proj :deployments))
         (spec (call-interactively
                'proviso-deploy-create-cmd)))
    (if spec
        (progn
          (if specs
              (add-to-list 'specs spec t)
            (setq specs (list spec)))
          (proviso-put proj :deployments specs))
      (user-error "No deployment command added"))))

;;;###autoload
(defun proviso-deploy-run-deploy (&optional arg)
  "Run a deployment.
If ARG is non-nil, another project can be chosen."
  (interactive "P")
  (let* ((proj (if arg (proviso-choose-project)
                 (proviso-current-project)))
         (specs (proviso-get proj :deployments))
         spec)
    (if specs
        (if (setq spec
                  (proviso-deploy-choose-deploy
                   specs
                   "Run deployment: "))
            (progn
              (proviso-deploy-one spec)
              (proviso-put proj :last-deploy spec))
          (user-error "No deployment chosen"))
      (user-error "No deployments"))))

;;;###autoload
(defun proviso-deploy-run-all-deploys (&optional arg)
  "Run all deployments.
If ARG is non-nil, another project can be chosen."
  (interactive "P")
  (let* ((proj (if arg (proviso-choose-project)
                 (proviso-current-project)))
         (lst (proviso-get proj :deployments)))
    (proviso-deploy-all lst)))

;;;###autoload
(defun proviso-deploy-run-last (&optional arg)
  "Rerun the last deployment, if any.
If ARG is non-nil, another project can be chosen."
  (interactive "P")
  (let* ((proj (if arg (proviso-choose-project)
                 (proviso-current-project)))
         (spec (proviso-get proj :last-deploy)))
    (if spec
        (proviso-deploy-one spec)
      (user-error "No prior deployment"))))

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
         (lst (proviso-deploy-read-from-file file)))
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
         spec)
    (if specs
        (if (setq spec
                  (proviso-deploy-choose-deploy
                   specs
                   "Delete deployment: "))
            (proviso-put
             proj :deployments
             (delete spec
                     (proviso-get proj :deployments)))
          (user-error "No deployment chosen"))
      (user-error "No deployments"))))

;;;###autoload
(defun proviso-deploy-check-file (&optional arg)
  "Check a deployed file for alterations.
If ARG is non-nil, another project can be chosen."
  (interactive "P")
  (let* ((proj (if arg (proviso-choose-project)
                 (proviso-current-project)))
         (specs (proviso-get proj :deployments))
         spec
         src dst)
    (if specs
        (if (setq spec
                  (proviso-deploy-choose-deploy
                   specs
                   "Check deployment: "))
            (progn
              (setq src (plist-get spec :source))
              (setq dst (plist-get spec :destination))
              (when (file-directory-p dst)
                (setq dst (expand-file-name
                           (file-name-nondirectory src) dst)))
              (if (and src dst)
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
                    (user-error "One or more files do not exist"))
                (user-error "No files to compare")))
          (user-error "No deployment chosen"))
      (user-error "No deployments"))))

;;;###autoload
(defun proviso-deploy-diff-file (&optional arg)
  "Run diff against a deployed file.
If ARG is non-nil, another project can be chosen."
  (interactive "P")
  (let* ((proj (if arg (proviso-choose-project)
                 (proviso-current-project)))
         (specs (proviso-get proj :deployments))
         spec src dst)
    (if specs
        (if (setq spec
                  (proviso-deploy-choose-deploy
                   specs
                   "Diff deployment: "))
            (progn
              (setq src (plist-get spec :source))
              (setq dst (plist-get spec :destination))
              (when (file-directory-p dst)
                (setq dst (expand-file-name
                           (file-name-nondirectory src) dst)))
              (if (and src dst)
                  (if (and (file-exists-p src)
                           (file-exists-p dst))
                      (diff src dst)
                    (user-error "One or more files do not exist"))
                (user-error "No files to compare")))
          (user-error "No deployment chosen"))
      (user-error "No deployments"))))

;;;###autoload
(defun proviso-deploy-ediff-file (&optional arg)
  "Run ediff against a deployed file.
If ARG is non-nil, another project can be chosen."
  (interactive "P")
  (let* ((proj (if arg (proviso-choose-project)
                 (proviso-current-project)))
         (specs (proviso-get proj :deployments))
         spec src dst)
    (if specs
        (if (setq spec
                  (proviso-deploy-choose-deploy
                   specs
                   "Ediff deployment: "))
            (progn
              (setq src (plist-get spec :source))
              (setq dst (plist-get spec :destination))
              (when (file-directory-p dst)
                (setq dst (expand-file-name
                           (file-name-nondirectory src) dst)))
              (if (and src dst)
                  (if (and (file-exists-p src)
                           (file-exists-p dst))
                      (ediff-files src dst)
                    (user-error "One or more files do not exist"))
                (user-error "No files to compare")))
          (user-error "No deployment chosen"))
      (user-error "No deployments"))))

;;;###autoload
(defun proviso-deploy-edit-deploy (&optional arg)
  "Edit a deployment command.
If ARG is non-nil, another project can be chosen."
  (interactive "P")
  (let* ((proj (if arg (proviso-choose-project)
                 (proviso-current-project)))
         (specs (proviso-get proj :deployments))
         spec src dst cmd)
    (if specs
        (if (setq spec
                  (proviso-deploy-choose-deploy
                   specs
                   "Edit deployment: "))
            (let ((cmd (plist-get spec :command))
                  (src (plist-get spec :source))
                  (dst (plist-get spec :destination)))
              (cond (cmd
                     (setcar (member spec specs)
                             (read-string "New command: "
                                          cmd)))
                    ((and src dst)
                     (setcar (member spec specs)
                             (proviso-deploy-create
                              (read-file-name
                               "Source: "
                               (file-name-directory src)
                               nil nil
                               (file-name-nondirectory src))
                              (read-file-name
                               "Destination: "
                               (file-name-directory dst)
                               nil nil
                               (file-name-nondirectory dst))
                              )))))
          (user-error "No deployment chosen"))
      (user-error "No deployments"))))

;;;###autoload
(defun proviso-deploy-find-file (&optional arg)
  "Edit the deployed file.
If ARG is non-nil, another project can be chosen."
  (interactive "P")
  (let* ((proj (if arg (proviso-choose-project)
                 (proviso-current-project)))
         (specs (proviso-get proj :deployments))
         spec file)
    (if specs
        (if (setq spec
                  (proviso-deploy-choose-deploy
                   specs
                   "Find deployed file: "))
            (let ((file (plist-get spec :destination)))
              (if file
                  (if (file-exists-p file)
                      (find-file file)
                    (user-error "File '%s' does not exist" file))
                (user-error "No remote file to edit")))
          (user-error "No deployment chosen"))
      (user-error "No deployments"))))

;;;###autoload
(defun proviso-deploy-find-file-other-window (&optional arg)
  "Edit the deployed file in another window.
If ARG is non-nil, another project can be chosen."
  (interactive "P")
  (let* ((proj (if arg (proviso-choose-project)
                 (proviso-current-project)))
         (specs (proviso-get proj :deployments))
         spec file)
    (if specs
        (if (setq spec
                  (proviso-deploy-choose-deploy
                   specs
                   "Find deployed file in other window: "))
            (if (and (setq file (plist-get spec :destination))
                     (file-exists-p file))
                (find-file-other-window file)
              (user-error "File '%s' does not exist" file))
          (user-error "No deployment chosen"))
      (user-error "No deployments"))))

(defvar proviso-deploy-mode-map
  (let ((map (make-sparse-keymap)))
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
    (proviso-deploy-create-buffer proj)
    (pop-to-buffer proviso-deploy-buffer-name)))

(defun proviso-deploy-create-buffer (proj)
  "Create a deployment buffer for project PROJ."
  (interactive)
  (setq proviso-deploy-buffer-name
        (format proviso-deploy-buffer-name-prefix proj))
  (let ((buffer (get-buffer-create proviso-deploy-buffer-name))
        (width (string-width "Destination"))
        (home (getenv "HOME"))
        lst)
    (proviso-gui-init-buffer buffer proviso-deploy-mode-map)
    (with-current-buffer buffer
      (setq-local proviso-local-proj proj)
      (proviso-deploy-mode))
    (setq width
          (proviso-gui-add-to-buffer
           buffer
           '((:heading "Project"
                       :content (lambda ()
                                  (propertize
                                   (proviso-get proviso-local-proj :project-name)
                                   'face 'highlight))
                       :section post)
             (:heading "File"
                       :content (lambda ()
                                  (let ((file (proviso-get proviso-local-proj :deploy-file)))
                                    (cond ((and file (file-exists-p file))
                                           (propertize
                                            (replace-regexp-in-string home "~" file)
                                            'face '(bold)))
                                          (file
                                           (propertize
                                            (replace-regexp-in-string home "~" file)
                                            'face '(shadow)))
                                          (t
                                           (propertize "None" 'face '(shadow)))))))
             ) width))
    (dolist (spec (proviso-get proj :deployments))
      (lexical-let ((cmd (plist-get spec :command))
                    (src (plist-get spec :source))
                    (dst (plist-get spec :destination)))
        (cond (cmd
               (add-to-list 'lst
                            (list
                             :heading "Command"
                             :content (lambda () cmd)) t))
              ((and src dst)
               (when (file-directory-p dst)
                 (setq dst (expand-file-name
                            (file-name-nondirectory src) dst)))
               (add-to-list 'lst
                            (list
                             :heading "Source"
                             :content (lambda ()
                                        (replace-regexp-in-string home "~" src))
                             :section 'pre) t)
               (add-to-list 'lst
                            (list
                             :content (lambda ()
                                        (propertize
                                         (format-time-string
                                          "%F %r"
                                          (file-attribute-modification-time
                                           (file-attributes
                                            src)))
                                         'face '(shadow))))
                            t)
               (add-to-list 'lst
                            (list
                             :heading "Destination"
                             :content (lambda ()
                                        (replace-regexp-in-string home "~" dst)))
                            t)
               (add-to-list 'lst
                            (list
                             :content (lambda ()
                                        (propertize
                                         (if (file-exists-p dst)
                                             (format-time-string
                                              "%F %r"
                                              (file-attribute-modification-time
                                               (file-attributes
                                                dst)))
                                           "---------- --:--:-- --")
                                         'face '(shadow))))
                            t)
               ))))
    (setq width (proviso-gui-add-to-buffer buffer lst width))
    (proviso-gui-finalize-buffer buffer)
    ))

(provide 'proviso-deploy)
;;; proviso-deploy.el ends here
