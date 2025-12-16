;;; proviso.el --- Manage projects
;; Copyright (C) 2016-2019, 2021-2023, 2025  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Thursday, November  3, 2016
;; Version: 1.0
;; Modified Time-stamp: <2025-12-16 14:10:17 dharms>
;; Modified by: Dan Harms
;; Keywords: tools profiles project
;; URL: https://github.com/articuluxe/proviso.git
;; Package-Requires: ((emacs "25.1"))

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
;; This project is based in part on profiles.el by Sylvain Bougerel, from
;; 2009.

;;

;;; Code:

(require 'proviso-core)
(require 'proviso-regexp)
(require 'proviso-ag)
(require 'proviso-rg)
(require 'proviso-fd)
(require 'proviso-xref)
(require 'proviso-clang)
(require 'proviso-finder)
(require 'proviso-dashboard)
(require 'proviso-deploy)
(require 'proviso-fulledit)
(require 'proviso-compile)
(require 'proviso-gud)
(require 'proviso-dired)
(require 'proviso-display)
(require 'proviso-docker)
(require 'proviso-tags)
(require 'proviso-bookmarks)
(require 'proviso-gentags)
(require 'proviso-registers)
(require 'proviso-grep)
(require 'proviso-include-files)
(require 'proviso-search)

(eval-when-compile
  (require 'cl-lib))
(require 'switch-buffer-functions)

(defun proviso-init (proj)
  "Load a project PROJ."
  (condition-case err
      (run-hook-with-args 'proviso-hook-on-project-init proj)
    ('proviso-error-non-fatal
     (proviso-put proj :inited nil)
     (message "Stopped loading project \"%s\" (%s)"
              (symbol-name proj) (cdr err)))
    ((proviso-error-aborted proviso-error)
     (ignore-errors
       (proviso-put proj :inited nil))
     (error (cdr err)))))

(defvar proviso--load-file-errors '()
  "List of errors encountered while loading a project file.")

(defun proviso--validate-init-errors (proj)
  "Validate and handle any errors that occurred during loading of project PROJ."
  (setq proviso--ignore-load-errors nil)
  (dolist (err proviso--load-file-errors)
    (proviso--query-error proj err)))

(add-hook 'proviso-hook-on-project-init #'proviso--validate-init-errors)

(defun proviso--log-project-inited (proj)
  "Log a project PROJ upon initialization."
  (let ((name (symbol-name proj)))
    (message (concat
              "Loaded project "
              (propertize (proviso-get proj :project-name)
                          'face '(bold))
              " at "
              (propertize (proviso-get proj :root-dir)
                          'face '(italic))))))

(defun proviso--loaded (proj)
  "A project PROJ has been loaded.
This may or may not be for the first time."
  (when proj
    (unless (proviso-get proj :inited)
      (proviso-put proj :inited t)
      (run-hook-with-args 'proviso-hook-on-project-pre-init proj)
      (proviso-load-environment-file-from-project proj)
      (condition-case err
          (proviso--safe-funcall proj :initfun proj)
        ('error (push err proviso--load-file-errors)))
      (proviso-init proj)
      (run-hook-with-args 'proviso-hook-on-project-post-init proj)
      (proviso--log-project-inited proj))
    (proviso--on-proj-loaded proj)))

(defun proviso-on-file-opened ()
  "Called when a file is opened."
  (if-let ((proj (or proviso-local-proj proviso-curr-proj)))
      (run-hook-with-args 'proviso-hook-on-file-opened
                          proj major-mode)))

(add-hook 'find-file-hook 'proviso-on-file-opened)

(defun proviso--on-proj-loaded (proj)
  "Called when a project PROJ is made active."
  (when proj
    (unless (eq proj proviso-curr-proj)
      (let ((proviso-old-proj proviso-curr-proj))
        (setq proviso-curr-proj proj)
        (run-hook-with-args 'proviso-hook-on-project-active proj proviso-old-proj)
        ))))

(defun proviso-refresh-current-project ()
  "Make sure the current buffer's project is up-to-date."
  (interactive)
  (proviso--on-proj-loaded proviso-local-proj))

(defun proviso-find-file-no-project (file)
  "Open FILE without examining for `'proviso' functionality."
  (interactive "fFind file without project: ")
  (let ((proviso-inspect-files-p nil))
    (find-file file)))

(defun proviso-switch-buffer-defun (prev curr)
  "Called on buffer change events, with PREV and CURR the buffers that changed."
  (when (local-variable-p 'proviso-local-proj curr)
    (proviso--on-proj-loaded
     (buffer-local-value 'proviso-local-proj curr))))

(add-hook 'switch-buffer-functions 'proviso-switch-buffer-defun)

(defun proviso--eval-file (filename)
  "Evalute the settings contained inside FILENAME."
  (setq proviso--load-file-errors nil)
  (let (alist str)
    (and filename
         (not (file-directory-p filename))
         (string-match-p (car proviso-project-signifiers) filename)
         (with-temp-buffer
           (insert-file-contents-literally filename)
           (condition-case err
               (progn
                 (setq str (string-trim (buffer-string)))
                 (unless (string-empty-p str)
                   (setq alist (car (read-from-string (buffer-string))))))
             ('error (push err proviso--load-file-errors)))))
    (if (listp alist)
        alist
      (push (format "Malformed file %s: not a plist" filename)
            proviso--load-file-errors)
      nil)))

(defun proviso--load-file (filename)
  "Load the settings contained within FILENAME."
  (setq proviso--load-file-errors nil)
  (condition-case err
      ;; only load *.proviso files
      (and filename
           (not (file-directory-p filename))
           (string-match-p (car proviso-project-signifiers) filename)
           (load-file filename))
    ('error (push err proviso--load-file-errors))))

(advice-add 'find-file-noselect-1 :before 'proviso--file-opened-advice)

(defun proviso-disable ()
  "Disable the project manipulation features of `proviso'."
  (interactive)
  (message "Proviso disabled")
  (advice-remove 'find-file-noselect-1 'proviso--file-opened-advice))

;;;###autoload
(defun proviso-enable ()
  "Enable the project manipulation features of `proviso'."
  (interactive)
  (message "Proviso enabled")
  (advice-add 'find-file-noselect-1 :before 'proviso--file-opened-advice))

(defun proviso--file-opened-advice (buf filename nowarn rawfile truename number)
  "Advice to initialize a project, if necessary, for BUF, visiting FILENAME.
NOWARN, RAWFILE, TRUENAME and NUMBER are not used by the advice."
  (proviso--file-opened buf filename))

(defun proviso--file-opened (buffer filename)
  "Initialize a project, if necessary, for BUFFER, visiting FILENAME."
  (when proviso-inspect-files-p
  (with-current-buffer buffer
    (make-local-variable 'proviso-local-proj)
    (put 'proviso-local-proj 'permanent-local t)
    (let* ((dir (file-name-directory (expand-file-name filename)))
           (remote-prefix (file-remote-p dir))
           (remote-host (file-remote-p dir 'host))
           basename fullname scratch props new-proj)
      ;; try to find some project for this file
      (seq-let [root-file root-dir] (proviso--find-root dir t)
        (unless root-dir (setq root-dir dir))
        ;; first check for a provisional project
        (seq-let [provisional-path provisional-project provisional-name]
            (proviso-find-provisional-project root-dir)
          (if (and provisional-project (file-directory-p provisional-path))
              (let (proj other-props)
                (when remote-host
                  (setq provisional-path (file-remote-p provisional-path 'localname)))
                (setq root-dir (file-name-as-directory provisional-path))
                (setq basename provisional-project)
                (setq proj (intern-soft basename proviso-provisional-obarray))
                (setq fullname
                      (proviso-create-project-uid basename root-dir remote-host))
                (setq new-proj (proviso-add-active-project-path root-dir fullname remote-host))
                (if new-proj
                    (progn
                      (setq props (if proj (copy-tree (symbol-plist proj)) nil))
                      (when (and root-file
                                 (setq other-props (proviso--eval-file root-file)))
                        (setq props (append props other-props))
                        (message "Adding properties from project file %s to provisional project '%s'"
                                 (abbreviate-file-name root-file)
                                 basename))
                      (setq basename provisional-name) ;basename used for display name here on
                      (unless (setq proviso-local-proj
                                    (proviso-define-active-project fullname props))
                        (user-error "Unable to set project '%s' from provisional '%s' for %s"
                                    fullname basename (abbreviate-file-name filename))))
                  (unless (setq proviso-local-proj (proviso-proj-p fullname))
                    (user-error
                     "Error finding existing project '%s' matching provisional project '%s' for %s"
                     fullname provisional-name (abbreviate-file-name filename)))))
            ;; no provisional project, look for a project file
            (if root-file
                (progn
                  (when remote-host
                    (setq root-dir (file-remote-p root-dir 'localname)))
                  (setq props (proviso--eval-file root-file))
                  ;; project name defaults to filename, unless overridden
                  (unless (setq basename (plist-get props :project-name))
                    (setq basename (proviso-compute-basename-from-file root-file)))
                  (setq fullname (proviso-create-project-uid basename root-dir remote-host))
                  (setq new-proj (proviso-add-active-project-path root-dir fullname remote-host))
                  (if new-proj
                      (unless (setq proviso-local-proj
                                    (proviso-define-active-project fullname props))
                        (user-error "Unable to set project '%s' from %s for %s"
                                    fullname
                                    (abbreviate-file-name root-file)
                                    (abbreviate-file-name filename)))
                    (unless (setq proviso-local-proj (proviso-proj-p fullname))
                      (user-error "Error finding existing project '%s' from %s for %s"
                                  fullname
                                  (abbreviate-file-name root-file)
                                  (abbreviate-file-name filename)))))
              ;; otherwise no project file either
              )))
        (when (and proviso-local-proj new-proj)
          (proviso-put proviso-local-proj :root-dir root-dir)
          (proviso-put proviso-local-proj :project-name basename)
          (proviso-put proviso-local-proj :project-uid fullname)
          (when remote-host
            (proviso-put proviso-local-proj :remote-host remote-host))
          (when remote-prefix
            (proviso-put proviso-local-proj :remote-prefix remote-prefix))
          (setq scratch (concat remote-prefix root-dir))
          (if (file-writable-p scratch)
              (proviso-put proviso-local-proj :scratch-dir scratch)
            (proviso-put proviso-local-proj :scratch-dir
                         (proviso--compute-scratch-dir root-dir remote-host remote-prefix)))
          (if remote-host
              (proviso-put proviso-local-proj :local-scratch-dir
                           (proviso--compute-scratch-dir root-dir remote-host remote-prefix t))))
        )
      (proviso--loaded proviso-local-proj)))))

(provide 'proviso)
;;; proviso.el ends here
