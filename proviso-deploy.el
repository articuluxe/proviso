;;; proviso-deploy.el --- deploy artifacts to locations
;; Copyright (C) 2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Wednesday, September 12, 2018
;; Version: 1.0
;; Modified Time-stamp: <2018-09-26 09:06:43 dharms>
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

(defvar-local proviso-deploy-buffer-name nil
  "Buffer name for `proviso-deploy' mode.")

(defconst proviso-deploy-buffer-name-prefix
  "*%s-deploy*"
  "Buffer prefix string for `proviso-deploy'.
This will be formatted with the project name.")

(defun proviso-deploy--stringize-deployment (spec)
  "Create a string based on SPEC suitable for display."
  (list
   (list :heading "Source"
         :content (lambda ()
                    (plist-get spec :source)))
   (list :heading "Destination"
         :content (lambda ()
                    (plist-get spec :destination)))
   ))

(defun proviso-deploy--stringize-deployments (specs)
  "Create a string representation of all deployments in SPECS.
This will be used to display them to the user."
  (-mapcat #'proviso-deploy--stringize-deployment
           specs))

(defun proviso-deploy-one (spec)
  "Execute a deployment represented by SPEC."
  (let ((src (plist-get spec :source))
        (dst (plist-get spec :destination)))
    (message "Transferring %s to %s..." src dst)
    (proviso-transfer-file-async src dst)))

(defun proviso-deploy-all (specs)
  "Execute all deployments contained in SPECS."
  (dolist (spec specs) #'proviso-deploy-one))

(defun proviso-deploy-create (source dest)
  "Add a deployment from SOURCE to DEST."
  (interactive "FSource: \nFDestination: ")
  (list :source source :destination dest))

(defun proviso-deploy-choose-deploy (specs)
  "Let user select a deployment from SPECS."
  (let ((lst
         (mapcar (lambda (spec)
                   (cons
                    (format "%s --> %s"
                            (plist-get spec :source)
                            (plist-get spec :destination))
                    spec))
                 specs)))
    (catch 'exit
      (ivy-read "Choose deployment: "
                lst
                :action (lambda (x)
                          (throw 'exit (cdr x)))
                :caller 'proviso-deploy-choose-deploy
                ))))

(defun proviso-deploy-write-to-file (filename specs)
  "Save a deployment specification SPECS to FILENAME."
  (with-temp-buffer
    (prin1
     (mapcar
      (lambda (spec)
        (cons
         (plist-get spec :source)
         (plist-get spec :destination)))
      specs)
     (current-buffer))
    (insert "\n")
    (write-file filename)))

(defun proviso-deploy--read-from-str (str)
  "Read deployments from STR."
  (let (specs)
    (dolist (spec (car (read-from-string str)))
      (add-to-list 'specs
                   (list :source
                         (car spec)
                         :destination
                         (cdr spec)) t))
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
         (lst (proviso-get proj :deployments))
         (spec (call-interactively
                'proviso-deploy-create)))
    (if spec
        (progn
          (if lst
              (add-to-list 'lst spec t)
            (setq lst (list spec)))
          (proviso-put proj :deployments lst))
      (message "No deployment added."))))

;;;###autoload
(defun proviso-deploy-run-deploy (&optional arg)
  "Run a deployment.
If ARG is non-nil, another project can be chosen."
  (interactive "P")
  (let* ((proj (if arg (proviso-choose-project)
                 (proviso-current-project)))
         (lst (proviso-get proj :deployments))
         spec)
    (if lst
        (if (setq spec (proviso-deploy-choose-deploy lst))
            (proviso-deploy-one spec)
          (message "No deployment chosen."))
      (message "No deployments found."))))

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
(defun proviso-deploy-run-last ()
  "Rerun the last deployment."
  ;; TODO
  )

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
      (message "No deployments read in from %s." file))))

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
  (let ((buffer (get-buffer-create proviso-deploy-buffer-name)))
    (proviso-gui-init-buffer buffer proviso-deploy-mode-map)
    (with-current-buffer buffer
      (setq-local proviso-local-proj proj)
      (proviso-deploy-mode))
    (proviso-gui-add-to-buffer
     buffer
     '((:heading "Project"
                 :content
                 (lambda ()
                   (propertize
                    (proviso-get proviso-local-proj :project-name)
                    'face 'highlight)))))
    (proviso-gui-add-to-buffer
     buffer
     (proviso-deploy--stringize-deployments))
    (proviso-gui-finalize-buffer buffer)
    ))

(provide 'proviso-deploy)
;;; proviso-deploy.el ends here
