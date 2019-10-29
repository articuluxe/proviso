;;; proviso-finder.el --- Utilities for selecting files in a project
;; Copyright (C) 2018-2019  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Tuesday, April 24, 2018
;; Version: 1.0
;; Modified Time-stamp: <2019-10-29 06:44:30 dan.harms>
;; Modified by: Dan Harms
;; Keywords: tools unix proviso project clang-format
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
;; Utilities for searching files and directories belonging to the current
;; project.
;;

;;; Code:
(require 'proviso-core)
(require 'proviso-fulledit)
(require 'proviso-fd)
(require 'counsel)
(require 'async)

(defcustom proviso-finder-file-cache-enabled t
  "Should the file cache be populated automatically."
  :group 'proviso-custom-group)

(defun proviso-finder-gather-files-interactive (proj)
  "Gather files in project PROJ."
  (let ((remote (proviso-get proj :remote-prefix))
        (root (or (proviso-get proj :root-dir) default-directory))
        (lst (proviso-get proj :proj-alist))
        (exclude-files (or (proviso-get proj :grep-exclude-files)
                           proviso-uninteresting-files))
        (exclude-dirs (or (proviso-get proj :grep-exclude-dirs)
                          proviso-uninteresting-dirs))
        (include-files (or (proviso-get proj :grep-include-files)
                           proviso-interesting-files)))
    (proviso-finder-gather-files remote root lst nil
                                 exclude-files exclude-dirs include-files)))

(defun proviso-finder-gather-files (remote root lst &optional async
                                           exclude-files exclude-dirs include-files)
  "Gather files at REMOTE under ROOT, according to LST (see `:proj-alist').
If ASYNC is non-nil, the search is occurring asynchronously.
EXCLUDE-FILES, EXCLUDE-DIRS and INCLUDE-FILES, if present, are
passed on to `proviso-fulledit-gather-files' or `proviso-fd-gather-files'."
  (let* ((method (if (xfer-util-find-executable "fd" root) 'fd 'std))
         (msg (format "athering all files%s %sunder %s"
                      (if (eq method 'fd) " using fd" "")
                      (if async "asynchronously " "")
                      (concat remote root)))
         (reporter (unless async (make-progress-reporter (concat "G" msg "..."))))
         result entry dir files)
    (if (seq-empty-p lst)
        (push (list root) lst))
    (unless async (message "G%s" msg))
    (unwind-protect
        (catch 'done
          (dolist (element lst)
            (setq entry (proviso-substitute-env-vars (plist-get element :dir)))
            (setq dir
                  (concat
                   remote
                   (when (or (null entry) (not (file-name-absolute-p entry)))
                     root)
                   entry))
            (setq files
                  (if (eq method 'fd)
                      (proviso-fd-gather-files
                       dir nil exclude-files
                       exclude-dirs include-files)
                    (proviso-fulledit-gather-files
                     dir exclude-files
                     exclude-dirs include-files
                     reporter t)))
            (setq result (nconc result (sort files 'string-lessp)))))
      (when reporter (progress-reporter-done reporter))
      (unless async
        (message "Done g%s (%d files)" msg (length result))))
    (proviso-finder-adjust-paths result remote root)))

(defun proviso-finder-gather-dirs-interactive (proj)
  "Gather directories in project PROJ."
  (let ((remote (proviso-get proj :remote-prefix))
        (root (or (proviso-get proj :root-dir) default-directory))
        (lst (proviso-get proj :proj-alist))
        (exclude-dirs (or (proviso-get proj :grep-exclude-dirs)
                          proviso-uninteresting-dirs)))
    (proviso-finder-gather-dirs remote root lst nil exclude-dirs)))

(defun proviso-finder-gather-dirs (remote root lst &optional async exclude-dirs)
  "Gather directories at REMOTE under ROOT, according to LST.
See `:proj-alist' for more details.  If ASYNC is non-nil,
the search is occurring asynchronously.  EXCLUDE-DIRS provides an
optional exclusion list."
  (let* ((msg (format "athering all dirs %sunder %s"
                      (if async "asynchronously " "")
                      (concat remote root)))
         (reporter (unless async (make-progress-reporter (concat "G" msg "..."))))
         result entry dir dirs)
    (if (seq-empty-p lst)
        (push (list root) lst))
    (unless async (message "G%s" msg))
    (unwind-protect
        (catch 'done
          (dolist (element lst)
            (setq entry (proviso-substitute-env-vars (plist-get element :dir)))
            (setq dir
                  (concat
                   remote
                   (when (or (null entry) (not (file-name-absolute-p entry)))
                     root)
                   entry))
            (setq dirs (proviso-fulledit-gather-dirs dir exclude-dirs reporter t))
            (setq result (nconc result (sort dirs 'string-lessp)))))
      (when reporter (progress-reporter-done reporter))
      (unless async
        (message "Done g%s (%d dirs)" msg (length result))))
    (proviso-finder-adjust-paths result remote root)))

;;;###autoload
(defun proviso-finder-recompute-cache ()
  "Recompute the file and directory cache for the current project."
  (interactive)
  (let ((proj (proviso-current-project)))
    (when proj
      (proviso-finder--load-files proj))))

(defun proviso-finder-adjust-paths (lst remote root)
  "Adjust paths relative in LST, a list of files or directories.
REMOTE is a possibly empty remote prefix, ROOT is a root directory.
The result is the same list, with each element transformed into a
cons (NAME . ORIG), where ORIG is the original file or
directory (absolute), and NAME is that name made relative to ROOT."
  (let ((stem (concat remote root)))
    (mapcar (lambda (name)
              (cons
               (string-remove-prefix
                (or remote "")
                (if (file-in-directory-p name stem)
                    (file-relative-name name stem)
                  name))
               name))
            lst)))

;;;###autoload
(defun proviso-find-file-all (&optional arg)
  "Allow user to select a file across all projects.
ARG is currently unused."
  (interactive "P")
  (let (lst)
    (mapatoms (lambda (atom) (push atom lst)) proviso-obarray)
    (ivy-read "Find file in projects: "
              (proviso-find-file--projects lst)
              :action (lambda (x) (find-file x))
              :caller #'proviso-find-file-all)))

;;;###autoload
(defun proviso-find-file-all-other-window (&optional arg)
  "Allow user to select a file in other window across all projects.
ARG is currently unused."
  (interactive "P")
  (let (lst)
    (mapatoms (lambda (atom) (push atom lst)) proviso-obarray)
    (ivy-read "Find file in other window in projects: "
              (proviso-find-file--projects lst)
              :action (lambda (x) (find-file-other-window x))
              :caller #'proviso-find-file-all-other-window)))

(defun proviso-find-file--projects (projects)
  "Return a list of all files contained in PROJECTS."
  (mapcan (lambda (proj)
            (mapcar 'cdr (proviso-get proj :project-files)))
          projects))

;;;###autoload
(defun proviso-finder-find-file (&optional arg)
  "Find file in current project's primary source directory.
If ARG is non-nil, another project can be chosen."
  (interactive "P")
  (let ((proj (if arg (proviso-choose-project)
                (proviso-current-project))))
    (proviso-finder--find-file proj nil)))

;;;###autoload
(defun proviso-finder-find-file-other-window (&optional arg)
  "Find file in other window among project's primary source directory.
If ARG is non-nil, another project can be chosen; otherwise the
current project is used."
  (interactive "P")
  (let ((proj (if arg (proviso-choose-project)
                (proviso-current-project))))
    (proviso-finder--find-file proj 'other)))

(defun proviso-finder--find-file (proj other-window)
  "Allow choosing a file to open in project PROJ.
OTHER-WINDOW means to open the file in the other window."
  (let* ((remote (proviso-get proj :remote-prefix))
         (root (or (proviso-get proj :root-dir) default-directory))
         (files (proviso-get proj :project-files))
         (desc (if proj (concat "in project \""
                                (proviso-get proj :project-name)
                                "\"")
                 "under current directory"))
         (prompt (concat "Find file " desc ": ")))
    (when (not files)
      (if proj
          (progn
            (unless (async-ready (proviso-get proj :project-files-future))
              (message "Still gathering files..."))
            (when (setq files (async-get (proviso-get proj :project-files-future)))
              (proviso-put proj :project-files files)))
        (setq files (proviso-finder-gather-files (file-remote-p default-directory)
                                                 default-directory nil nil
                                                 proviso-uninteresting-files
                                                 proviso-uninteresting-dirs
                                                 proviso-interesting-files))))
    (when (seq-empty-p files)
      (user-error "No files to open %s" desc))
    (ivy-read prompt files
              :action (if other-window
                          #'proviso-finder-open-file-other-window-action
                        #'proviso-finder-open-file-action)
              :caller #'proviso-finder--find-file)))

(defun proviso-finder-open-file-action (x)
  "Find file X in current project."
  (with-ivy-window
    (let* ((file (cdr x))
           (default-directory (file-name-directory file)))
      (find-file file))))

(defun proviso-finder-open-file-other-window-action (x)
  "Find file X in current project in other window."
  (with-ivy-window
    (let* ((file (cdr x))
           (default-directory (file-name-directory file)))
      (find-file-other-window file))))

(ivy-add-actions 'proviso-finder--find-file
                 '(("j" proviso-finder-open-file-other-window-action "other window")))

;;;###autoload
(defun proviso-finder-open-dir (&optional arg)
  "Find directory in current project.
If ARG is non-nil, another project can be chosen."
  (interactive "P")
  (let ((proj (if arg (proviso-choose-project)
                (proviso-current-project))))
    (proviso-finder--find-dir proj nil)))

;;;###autoload
(defun proviso-finder-open-dir-other-window (&optional arg)
  "Find directory in current project in the other window.
If ARG is non-nil, another project can be chosen."
  (interactive "P")
  (let ((proj (if arg (proviso-choose-project)
                (proviso-current-project))))
    (proviso-finder--find-dir proj 'other)))

(defun proviso-finder--find-dir (proj other-window)
  "Allow choosing a directory to open in project PROJ.
OTHER-WINDOW means to open the file in the other window."
  (let* ((remote (proviso-get proj :remote-prefix))
         (root (or (proviso-get proj :root-dir) default-directory))
         (symbol :project-dirs)
         (future :project-dirs-future)
         (dirs (proviso-get proj symbol))
         (desc (if proj (concat "in project \""
                                (proviso-get proj :project-name)
                                "\"")
                 "under current directory"))
         (prompt (concat "Open directory " desc ": ")))
    (when (not dirs)
      (if proj
          (progn
            (unless (async-ready (proviso-get proj future))
              (message "Still gathering directories..."))
            (when (setq dirs (async-get (proviso-get proj future)))
              (proviso-put proj symbol dirs)))
        (setq dirs (proviso-finder-gather-dirs (file-remote-p default-directory)
                                               default-directory nil nil
                                               proviso-uninteresting-dirs))))
    (when (seq-empty-p dirs)
      (user-error "No directories to open %s" desc))
    (ivy-read prompt dirs
              :action (if other-window
                          #'proviso-finder-open-dir-other-window-action
                        #'proviso-finder-open-dir-action)
              :caller #'proviso-finder--find-dir)))

(defun proviso-finder-open-dir-action (x)
  "Open dired on directory X in current project."
  (with-ivy-window
    (let* ((dir (cdr x)))
      (dired dir))))

(defun proviso-finder-open-dir-other-window-action (x)
  "Open dired on directory X in current project in other window."
  (with-ivy-window
    (let* ((dir (cdr x)))
      (dired-other-window dir))))

(ivy-add-actions 'proviso-finder--find-dir
                 '(("j" proviso-finder-open-dir-other-window-action "other window")))

(defun proviso-finder--file-cache-enabled (proj)
  "Check whether the file cache is enabled for project PROJ."
  (let ((setting (proviso-get proj :file-cache)))
    (cond ((eq setting 'enabled) t)
          ((eq setting 'disabled) nil)
          (t proviso-finder-file-cache-enabled))))

(defun proviso-finder--load-files (proj)
  "Start an async process to gather files contained in PROJ."
  (let ((remote (proviso-get proj :remote-prefix))
        (root (or (proviso-get proj :root-dir) default-directory))
        (lst (proviso-get proj :proj-alist))
        (exclude-files (or (proviso-get proj :grep-exclude-files)
                           proviso-uninteresting-files))
        (exclude-dirs (or (proviso-get proj :grep-exclude-dirs)
                          proviso-uninteresting-dirs))
        (include-files (or (proviso-get proj :grep-include-files)
                           proviso-interesting-files)))
    (when (proviso-finder--file-cache-enabled proj)
      (proviso-put proj :project-files-future
                   (async-start
                    `(lambda ()
                       (setq inhibit-message t)
                       ,(async-inject-variables "load-path\\|shell-file-name")
                       (require 'proviso-finder)
                       (proviso-finder-gather-files ,remote ,root (quote ,lst) t
                                                    (quote ,exclude-files)
                                                    (quote ,exclude-dirs)
                                                    (quote ,include-files)))))
      (proviso-put proj :project-dirs-future
                   (async-start
                    `(lambda ()
                       (setq inhibit-message t)
                       ,(async-inject-variables "load-path\\|shell-file-name")
                       (require 'proviso-finder)
                       (proviso-finder-gather-dirs ,remote ,root (quote ,lst) t
                                                   (quote ,exclude-dirs))))))))

(add-hook 'proviso-hook-on-project-init 'proviso-finder--load-files)

(provide 'proviso-finder)
;;; proviso-finder.el ends here
