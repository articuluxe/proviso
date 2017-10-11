;;; proviso.el --- manage projects
;; Copyright (C) 2016-2017  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Thursday, November  3, 2016
;; Version: 1.0
;; Modified Time-stamp: <2017-10-11 17:49:49 dharms>
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
(require 'proviso-fulledit)
(require 'proviso-compile)
(require 'proviso-dired)
(require 'proviso-display)
(require 'proviso-tags)
(require 'proviso-gentags)
;; bookmarks must come before registers so that registers runs first
(require 'proviso-bookmarks)
(require 'proviso-sml)
(require 'proviso-registers)
(require 'proviso-grep)
(require 'proviso-include-files)

(eval-when-compile
  (require 'cl))
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

(add-hook 'proviso-hook-on-project-init 'proviso--validate-init-errors)

(defun proviso--log-project-inited (proj)
  "Log a project PROJ upon initialization."
  (let ((name (symbol-name proj)))
    (message "Loaded project %s (%s) at %s"
             (proviso-get proj :project-name)
             name
             (proviso-get proj :root-dir))))

(defun proviso--loaded (proj)
  "A project PROJ has been loaded.
This may or may not be for the first time."
  (when proj
    (unless (proviso-get proj :inited)
      (proviso-put proj :inited t)
      (run-hook-with-args 'proviso-hook-on-project-pre-init proj)
      (proviso--safe-funcall proj :initfun proj)
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

(defun proviso--load-file (filename)
  "Load the settings contained within FILENAME."
  (let ((expr (concat (format "Proviso: Error while loading %S" filename) " %S")))
    (setq proviso--load-file-errors nil)
    (condition-case err
        (load-file filename)
      ('error (push err proviso--load-file-errors)))))

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
    (setq proviso-local-proj
          (intern-soft (proviso-find-path-alist
                        (expand-file-name filename))
                       proviso-obarray))
    (let* ((root (proviso--find-root (file-name-directory filename) t))
           (root-file (car root))
           (root-dir (cdr root))
           (remote-props (proviso--compute-remote-props root-dir))
           remote-host remote-localname remote-prefix basename)
      (when remote-props
        (setq remote-host (car remote-props))
        (setq remote-localname (cadr remote-props))
        (setq remote-prefix (caddr remote-props)))
      (when (and root root-file root-dir
                 (string-match
                  (concat "\\." proviso--ext "$")
                  root-file)
                 (or (not proviso-local-proj)
                     (not (string-equal root-dir
                                        (proviso-get proviso-local-proj :root-dir)))))
        ;; a new project, not yet inited
        (setq proviso--last-proj-defined nil)
        (proviso--load-file root-file)
        ;; project name defaults to filename, unless overridden
        (setq basename (proviso-get proviso--last-proj-defined :project-name))
        (unless basename
          (setq basename (proviso-compute-basename-from-file root-file)))
        ;; todo: check for uniqueness; alter if necessary
        ;; (while (proviso-name-p basename)
        (unless proviso--last-proj-defined
          (proviso-define-project basename))
        (when remote-props
          (setq root-dir remote-localname))
        (push (cons root-dir basename) proviso-path-alist)
        (setq proviso-local-proj
              (intern-soft (proviso-find-path-alist
                            (expand-file-name filename))
                           proviso-obarray))
        (when proviso-local-proj
          (unless (proviso-get proviso-local-proj :root-dir)
            (proviso-put proviso-local-proj :root-dir root-dir))
          ;; change to absolute if necessary: in case the project listed
          ;; root-dir as relative
          (when (f-relative? (proviso-get proviso-local-proj :root-dir))
            (proviso-put proviso-local-proj :project-name
                         (f-long (proviso-get proviso-local-proj :root-dir))))
          (unless (proviso-get proviso-local-proj :project-name)
            (proviso-put proviso-local-proj :project-name basename))
          (unless (proviso-get proviso-local-proj :remote-host)
            (proviso-put proviso-local-proj :remote-host remote-host))
          (unless (proviso-get proviso-local-proj :remote-prefix)
            (proviso-put proviso-local-proj :remote-prefix remote-prefix))
          (unless (proviso-get proviso-local-proj :root-stem)
            (proviso-put proviso-local-proj :root-stem
                         (proviso--compute-stem proviso-local-proj))))
        )
      (proviso--loaded proviso-local-proj)
      )))

(provide 'proviso)
;;; proviso.el ends here
