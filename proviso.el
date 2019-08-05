;;; proviso.el --- Manage projects
;; Copyright (C) 2016-2019  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Thursday, November  3, 2016
;; Version: 1.0
;; Modified Time-stamp: <2019-08-05 08:36:04 dharms>
;; Modified by: Dan Harms
;; Keywords: tools profiles project
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
;; This project is based in part on profiles.el by Sylvain Bougerel, from
;; 2009.

;;

;;; Code:

(require 'proviso-core)
(require 'proviso-regexp)
(require 'proviso-ag)
(require 'proviso-rg)
(require 'proviso-xref)
(require 'proviso-clang-format)
(require 'proviso-finder)
(require 'proviso-dashboard)
(require 'proviso-deploy)
(require 'proviso-fulledit)
(require 'proviso-compile)
(require 'proviso-gud)
(require 'proviso-dired)
(require 'proviso-display)
(require 'proviso-tags)
(require 'proviso-gentags)
;; bookmarks must come before registers so that registers runs first
(require 'proviso-bookmarks)
(require 'proviso-registers)
(require 'proviso-grep)
(require 'proviso-include-files)

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
      (condition-case err
          (proviso--safe-funcall proj :initfun proj)
        ('error (push err proviso--load-file-errors)))
      (proviso-init proj)
      (run-hook-with-args 'proviso-hook-on-project-post-init proj)
      (proviso--log-project-inited proj))
    (proviso--on-proj-loaded proj)))

(defun proviso-on-file-opened ()
  "Called when a file is opened."
  (when proviso-local-proj
    (run-hook-with-args 'proviso-hook-on-file-opened
                        proviso-local-proj major-mode)))

(add-hook 'find-file-hook 'proviso-on-file-opened)

(defun proviso--on-proj-loaded (proj)
  "Called when a project PROJ is made active."
  (unless (eq proj proviso-curr-proj)
    (let ((proviso-old-proj proviso-curr-proj))
      (setq proviso-curr-proj proj)
      (run-hook-with-args 'proviso-hook-on-project-active proj proviso-old-proj)
      )))

(defun proviso-refresh-current-project ()
  "Make sure the current buffer's project is up-to-date."
  (interactive)
  (proviso--on-proj-loaded proviso-local-proj))

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

(defun proviso--file-opened-advice (buf filename nowarn rawfile truename number)
  "Advice to initialize a project, if necessary, for BUF, visiting FILENAME.
NOWARN, RAWFILE, TRUENAME and NUMBER are not used by the advice."
  (proviso--file-opened buf filename))

(defun proviso--file-opened (buffer filename)
  "Initialize a project, if necessary, for BUFFER, visiting FILENAME."
  (with-current-buffer buffer
    (make-local-variable 'proviso-local-proj)
    (put 'proviso-local-proj 'permanent-local t)
    (let ((dir (file-name-directory (expand-file-name filename)))
          basename fullname)
      (seq-let [remote-host remote-localname remote-prefix]
          (proviso--compute-remote-props dir)
        (if (setq fullname (proviso-find-active-project dir remote-host))
            ;; active project already exists
            (setq proviso-local-proj (intern-soft fullname proviso-obarray))
          ;; no current project; so look for new project
          (seq-let [root-file root-dir] (proviso--find-root dir t)
            (if remote-host
                (setq root-dir remote-localname)
              (unless root-dir (setq root-dir dir)))
            (if root-file               ;found a project file
                (let ((props (proviso--eval-file root-file)))
                  ;; project name defaults to filename, unless overridden
                  (unless (setq basename (plist-get props :project-name))
                    (setq basename (proviso-compute-basename-from-file root-file)))
                  (setq fullname (proviso-create-project-uid basename root-dir remote-host))
                  (proviso-add-active-project-path root-dir fullname remote-host)
                  (setq proviso-local-proj
                        (proviso-define-active-project fullname props)))
              ;; else no project file; check proviso-path-alist
              (let ((cell (proviso-find-path-alist root-dir))
                    props)
                (if cell
                    (progn
                      (string-match (car cell) root-dir)
                      (setq root-dir (substring root-dir 0 (match-end 0)))
                      (setq basename (cdr cell))
                      (setq props (intern-soft basename proviso-provisional-obarray))
                      (setq fullname
                            (proviso-create-project-uid basename root-dir remote-host))
                      (proviso-add-active-project-path root-dir fullname remote-host)
                      (setq proviso-local-proj
                            (proviso-define-active-project fullname
                                                           (if props
                                                               (symbol-plist props)
                                                             nil))))
                  ;; else no project here
                  )))
            (when proviso-local-proj
              (proviso-put proviso-local-proj :root-dir root-dir)
              (proviso-put proviso-local-proj :project-name basename)
              (proviso-put proviso-local-proj :project-uid fullname)
              (when remote-host
                (proviso-put proviso-local-proj :remote-host remote-host))
              (when remote-prefix
                (proviso-put proviso-local-proj :remote-prefix remote-prefix))
              (proviso-put proviso-local-proj :root-stem
                           (proviso--compute-stem proviso-local-proj)))
            )                               ;done loading new project
          (proviso--loaded proviso-local-proj)
          )))))

(provide 'proviso)
;;; proviso.el ends here
