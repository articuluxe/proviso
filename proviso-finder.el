;;; proviso-finder.el --- utilities for selecting files in a project
;; Copyright (C) 2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Tuesday, April 24, 2018
;; Version: 1.0
;; Modified Time-stamp: <2018-05-08 17:21:45 dharms>
;; Modified by: Dan Harms
;; Keywords: tools unix proviso project clang-format
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
;; Utilities for searching files and directories belonging to the current
;; project.
;;

;;; Code:
(require 'proviso-core)
(require 'proviso-fulledit)
(require 'counsel)
(require 'async)

(defun proviso-finder-gather-files-interactive (proj &optional all-files)
  "Gather files in project PROJ.
If ALL-FILES is nil, only the first source directory will be
searched."
  (let ((remote (proviso-get proj :remote-prefix))
        (root (or (proviso-get proj :root-dir) default-directory))
        (lst (proviso-get proj :proj-alist)))
    (proviso-finder-gather-files remote root lst all-files)))

(defun proviso-finder-gather-files (remote root lst &optional all-files async)
  "Gather files at REMOTE under ROOT, according to LST (see `:proj-alist').
If ALL-FILES is nil, only the first source directory will be searched.
If ASYNC is non-nil, the search is occurring asynchronously."
  (let ((reporter (unless async (make-progress-reporter "Gathering files...")))
        result entry dir files msg)
    (if (seq-empty-p lst)
        (push (list root) lst))
    (setq msg (format "athering %sfiles %sunder %s"
                      (if all-files "all " "")
                      (if async "asynchronously " "")
                      (concat remote root)))
    (message "G%s" msg)
    (unwind-protect
        (catch 'done
          (dolist (element lst)
            (setq entry (plist-get element :dir))
            (setq dir
                  (concat
                   remote
                   (when (or (null entry) (not (file-name-absolute-p entry)))
                     root)
                   entry))
            (setq files (proviso-fulledit-gather-all-files dir reporter t))
            (setq result (nconc
                          result
                          (mapcar
                           (lambda (file)
                             ;; return a cons cell (display . absolute)
                             (let ((stem (concat remote root)))
                               (cons
                                (string-remove-prefix
                                 (or remote "")
                                 (if (file-in-directory-p file stem)
                                     (file-relative-name file stem)
                                   file))
                                file)))
                           (sort files 'string-lessp))))
            (unless all-files (throw 'done result))))
      (when reporter (progress-reporter-done reporter))
      (message "Done g%s (%d files)" msg (length result)))
    result))

(defun proviso-finder-gather-dirs-interactive (proj &optional all-dirs)
  "Gather directories in project PROJ.
If ALL-DIRS is nil, only the first source directory will be
searched."
  (let ((remote (proviso-get proj :remote-prefix))
        (root (or (proviso-get proj :root-dir) default-directory))
        (lst (proviso-get proj :proj-alist)))
    (proviso-finder-gather-dirs remote root lst all-dirs)))

(defun proviso-finder-gather-dirs (remote root lst &optional all-dirs async)
  "Gather directories at REMOTE under ROOT, according to LST.
See `:proj-alist' for more details.  If ALL-DIRS is nil, only the
first source directory will be searched.  If ASYNC is non-nil,
the search is occurring asynchronously."
  (let ((reporter (unless async (make-progress-reporter "Gathering directories...")))
        result entry dir dirs msg)
    (if (seq-empty-p lst)
        (push (list root) lst))
    (setq msg (format "athering %sdirs %sunder %s"
                     (if all-dirs "all " "")
                     (if async "asynchronously " "")
                     (concat remote root)))
    (message "G%s" msg)
    (unwind-protect
        (catch 'done
          (dolist (element lst)
            (setq entry (plist-get element :dir))
            (setq dir
                  (concat
                   remote
                   (when (or (null entry) (not (file-name-absolute-p entry)))
                     root)
                   entry))
            (setq dirs (proviso-fulledit-gather-all-dirs dir reporter t))
            (setq result (nconc
                          result
                          (mapcar
                           (lambda (dir)
                             ;; return a cons cell (display . absolute)
                             (let ((stem (concat remote root)))
                               (cons
                                (string-remove-prefix
                                 (or remote "")
                                 (if (file-in-directory-p dir stem)
                                     (file-relative-name dir stem)
                                   dir))
                                dir)))
                           (sort dirs 'string-lessp))))
            (unless all-dirs (throw 'done result))))
      (when reporter (progress-reporter-done reporter))
      (message "Done g%s (%d dirs)" msg (length result)))
    result))

;;;###autoload
(defun proviso-finder-find-file (&optional arg)
  "Find file in current project.  ARG customizes behavior."
  (interactive "P")
  (let* ((proj (proviso-current-project))
         (symbol (if arg :project-files-all :project-files))
         (future (if arg :project-files-all-future :project-files-future))
         (files (proviso-get proj symbol))
         (prompt (concat "Find file "
                         (if proj
                             (concat "in project \""
                                     (proviso-get proj :project-name)
                                     "\"")
                           "under current directory"))))
    (and (not files)
         (setq files (async-get (proviso-get proj future)))
         proj
         (proviso-put proj symbol files))
    (ivy-set-prompt 'proviso-finder-find-file counsel-prompt-function)
    (ivy-read prompt files
              :action #'proviso-finder-open-file-action
              :caller #'proviso-finder-find-file)))

(defun proviso-finder-open-file-action (x)
  "Find file X in current project."
  (with-ivy-window
    (let* ((file (cdr x))
           (default-directory (file-name-directory file)))
      (find-file file))))

;;;###autoload
(defun proviso-finder-open-dir (&optional arg)
  "Find directory in current project.  ARG customizes behavior."
  (interactive "P")
  (let* ((proj (proviso-current-project))
         (symbol (if arg :project-dirs-all :project-dirs))
         (future (if arg :project-dirs-all-future :project-dirs-future))
         (dirs (proviso-get proj symbol))
         (prompt (concat "Open directory "
                         (if proj
                             (concat "in project \""
                                     (proviso-get proj :project-name)
                                     "\"")
                           "under current directory"))))
    (and (not dirs)
         (setq dirs (async-get (proviso-get proj future)))
         proj
         (proviso-put proj symbol dirs))
    (ivy-set-prompt 'proviso-finder-open-dir counsel-prompt-function)
    (ivy-read prompt dirs
              :action #'proviso-finder-open-dir-action
              :caller #'proviso-finder-open-dir)))

(defun proviso-finder-open-dir-action (x)
  "Open dired on directory X in current project."
  (with-ivy-window
    (let* ((dir (cdr x)))
      (dired dir))))

(defun proviso-finder--load-files (proj)
  "Start an async process to gather files contained in PROJ."
  (let ((remote (proviso-get proj :remote-prefix))
        (root (or (proviso-get proj :root-dir) default-directory))
        (lst (proviso-get proj :proj-alist)))
    (proviso-put proj :project-files-future
                 (async-start
                  `(lambda ()
                     ,(async-inject-variables "load-path")
                     (require 'proviso)
                     (setq deferred:debug t)
                     (setq async-debug t)
                     ;; (setq debug-on-error t)
                     (proviso-finder-gather-files ,remote ,root (quote ,lst) nil t))))
    (proviso-put proj :project-files-all-future
                 (async-start
                  `(lambda ()
                     ,(async-inject-variables "load-path")
                     (setq deferred:debug t)
                     (setq async-debug t)
                     ;; (setq debug-on-error t)
                     (require 'proviso)
                     (proviso-finder-gather-files ,remote ,root (quote ,lst) t t))))
    (proviso-put proj :project-dirs-future
                 (async-start
                  `(lambda ()
                     ,(async-inject-variables "load-path")
                     (require 'proviso)
                     (setq deferred:debug t)
                     (setq async-debug t)
                     (proviso-finder-gather-dirs ,remote ,root (quote ,lst) nil t))))
    (proviso-put proj :project-dirs-all-future
                 (async-start
                  `(lambda ()
                     ,(async-inject-variables "load-path")
                     (require 'proviso)
                     (setq deferred:debug t)
                     (setq async-debug t)
                     (proviso-finder-gather-dirs ,remote ,root (quote ,lst) t t))))
    ))

(add-hook 'proviso-hook-on-project-init 'proviso-finder--load-files)

(provide 'proviso-finder)
;;; proviso-finder.el ends here
