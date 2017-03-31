;;; proviso.el --- manage projects
;; Copyright (C) 2016-2017  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Thursday, November  3, 2016
;; Version: 1.0
;; Modified Time-stamp: <2017-03-31 17:33:52 dharms>
;; Modified by: Dan Harms
;; Keywords: profiles project

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
(require 'proviso-include-files)
(require 'proviso-tags)
(require 'proviso-sml)

(eval-when-compile
  (require 'cl))
(require 'switch-buffer-functions)

(defun proviso-init (proj)
  "Load a project PROJ."
  (condition-case err
      (run-hook-with-args 'proviso-on-project-init proj)
      ;; (progn
      ;;   (proviso--validate-include-files proj)
      ;;   (proviso--set-include-files proj)
      ;;   )
    ('proviso-error-non-fatal
     (proviso-put proj :inited nil)
     (message "Stopped loading proj \"%s\" (%s)"
              (symbol-name proj) (cdr err)))
    ((proviso-error-aborted proviso-error)
     (ignore-errors
       (proviso-put proj :inited nil))
     (error (cdr err)))))


(defun proviso--log-project-inited (proj)
  "Log a project PROJ upon initialization."
  (let ((name (symbol-name proj)))
    (unless (string-equal name "default")
      (message "Loaded project %s (project %s) at %s"
               name
               (proviso-get proj :project-name)
               (proviso-get proj :root-dir)))))

(defun proviso--inited (proj)
  "Initialize a project PROJ."
  )

(defun proviso--loaded (proj)
  "A project PROJ has been loaded.
This may or may not be for the first time."
  (unless (proviso-get proj :inited)
    (proviso-put proj :inited t)
    (run-hook-with-args 'proviso-on-project-pre-init proj)
    (proviso--safe-funcall proj :initfun proj)
    (proviso-init proj)
    (run-hook-with-args 'proviso-on-project-post-init proj)
    (proviso--log-project-inited proj)
    )
  (unless (eq proj proviso-curr-proj)
    (let ((proviso-old-proj proviso-curr-proj))
      (setq proviso-curr-proj proj)
      (run-hook-with-args 'proviso-on-project-active proj proviso-old-proj)
      )))

;; (add-hook 'switch-buffer-functions
;;           (lambda (prev curr)
;;             (when (local-variable-p 'proviso-local-proj curr)
;;               (with-current-buffer curr ;todo: is there a better way?
;;                 (setq proviso-curr-proj proviso-local-proj)))))

(defun proviso--load-file (filename)
  "Load the settings contained within FILENAME."
  (load-file filename))

(advice-add 'find-file-noselect-1 :before 'proviso--file-opened-advice)

(defun proviso--file-opened-advice (buf filename nowarn rawfile truename number)
  "Advice that helps to initialize a project, if necessary, for BUF, visiting FILENAME."
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
          (setq basename (proviso--compute-basename-from-file root-file)))
        ;; todo: check for uniqueness; alter if necessary
        ;; (while (proviso-name-p basename)
        (unless proviso--last-proj-defined
          (proviso-define basename))
        (when remote-props
          (setq root-dir remote-localname))
        (push (cons root-dir basename) proviso-path-alist)
        (setq proviso-local-proj
              (intern-soft (proviso-find-path-alist
                            (expand-file-name filename))
                           proviso-obarray))
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
                       (proviso--compute-stem proviso-local-proj)))
        )
      (proviso--loaded proviso-local-proj)
      )))

(provide 'proviso)
;;; proviso.el ends here
