;;; proviso-finder.el --- utilities for selecting files in a project
;; Copyright (C) 2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Tuesday, April 24, 2018
;; Version: 1.0
;; Modified Time-stamp: <2018-04-26 21:19:01 dharms>
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
(require 'f)
(require 'counsel)

(defun proviso-finder-gather-files (&optional arg)
  "Gather files in current project, according to ARG."
  (let* ((proj (proviso-current-project))
         (remote (proviso-get proj :remote-prefix))
         (root (or (proviso-get proj :root-dir) default-directory))
         (lst (proviso-get proj :proj-alist))
         (reporter (make-progress-reporter "Gathering files..."))
         result entry dir files)
    (unwind-protect
        (catch 'done
          (dolist (element lst)
            (setq entry (plist-get element :dir))
            (setq dir
                  (concat
                   remote
                   (when (or (null entry) (f-relative? entry)) root)
                   entry))
            (setq files (proviso-fulledit-gather-all-files dir reporter t))
            (setq result (nconc
                          result
                          (mapcar
                           (lambda (file)
                             (cons (if (f-relative? entry)
                                       (file-relative-name file root)
                                     file)
                                   ;; todo: remove remote prefix
                                   file))
                           (sort files 'string-lessp))))
            (unless arg (throw 'done result))))
      (progress-reporter-done reporter))
    result))

(defun proviso-finder-gather-dirs (&optional arg)
  "Gather directories in current project, according to ARG."
  (let* ((proj (proviso-current-project))
         (remote (proviso-get proj :remote-prefix))
         (root (or (proviso-get proj :root-dir) default-directory))
         (lst (proviso-get proj :proj-alist))
         (reporter (make-progress-reporter "Gathering directories..."))
         result entry dir dirs)
    (unwind-protect
        (catch 'done
          (dolist (element lst)
            (setq entry (plist-get element :dir))
            (setq dir
                  (concat
                   remote
                   (when (or (null entry) (f-relative? entry)) root)
                   entry))
            (setq dirs (proviso-fulledit-gather-all-dirs dir reporter t))
            (setq result (nconc
                          result
                          (mapcar
                           (lambda (dir)
                             (cons (if (f-relative? entry)
                                       (file-relative-name dir root)
                                     dir)
                                   ;; todo: remove remote prefix
                                   dir))
                           (sort dirs 'string-lessp))))
            (unless arg (throw 'done result))))
      (progress-reporter-done reporter))
    result))

;;;###autoload
(defun proviso-finder-find-file (&optional arg)
  "Find file in current project.  ARG customizes behavior."
  (interactive "P")
  (let ((files (proviso-finder-gather-files arg)))
    (ivy-set-prompt 'proviso-finder-find-file counsel-prompt-function)
    (ivy-read "Find file in project" files
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
  (let ((dirs (proviso-finder-gather-dirs arg)))
    (ivy-set-prompt 'proviso-finder-open-dir counsel-prompt-function)
    (ivy-read "Open directory in project" dirs
              :action #'proviso-finder-open-dir-action
              :caller #'proviso-finder-open-dir)))

(defun proviso-finder-open-dir-action (x)
  "Open dired on directory X in current project."
  (with-ivy-window
    (let* ((dir (cdr x)))
      (dired dir))))

(provide 'proviso-finder)
;;; proviso-finder.el ends here
