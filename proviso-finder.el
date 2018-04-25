;;; proviso-finder.el --- utilities for selecting files in a project
;; Copyright (C) 2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Tuesday, April 24, 2018
;; Version: 1.0
;; Modified Time-stamp: <2018-04-25 07:50:47 dharms>
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

(defun proviso-finder-gather-files ()
  "Gather files in current project."
  (let* ((root (or (proviso-current-project-root) default-directory))
         (reporter (make-progress-reporter "Gathering files..."))
         (files (proviso-fulledit-gather-all-files root reporter t)))
    (progress-reporter-done reporter)
    (mapcar (lambda (file)
              (cons (file-relative-name file root)
                    file))
            files)))

(defun proviso-finder-gather-dirs ()
  "Gather directories in current project."
  (let* ((root (or (proviso-current-project-root) default-directory))
         (reporter (make-progress-reporter "Gathering directories..."))
         (dirs (proviso-fulledit-gather-all-dirs root reporter t)))
    (progress-reporter-done reporter)
    (mapcar (lambda (dir)
              (cons (file-relative-name dir root)
                    dir))
            dirs)))

;;;###autoload
(defun proviso-finder-find-file ()
  "Find file in current project."
  (interactive)
  (ivy-set-prompt 'proviso-finder-find-file counsel-prompt-function)
  (let ((files (proviso-finder-gather-files)))
    (ivy-read "Find file in project"
              (sort files (lambda (x y)
                            (string-lessp (car x) (car y))))
              :action #'proviso-finder-open-file-action
              :caller #'proviso-finder-find-file)))

(defun proviso-finder-open-file-action (x)
  "Find file X in current project."
  (with-ivy-window
    (let* ((file (cdr x))
           (default-directory (file-name-directory file)))
      (find-file file))))

;;;###autoload
(defun proviso-finder-open-dir ()
  "Find directory in current project."
  (interactive)
  (ivy-set-prompt 'proviso-finder-open-dir counsel-prompt-function)
  (let ((dirs (proviso-finder-gather-dirs)))
    (ivy-read "Open directory in project"
              (sort dirs (lambda (x y)
                           (string-lessp (car x) (car y))))
              :action #'proviso-finder-open-dir-action
              :caller #'proviso-finder-open-dir)))

(defun proviso-finder-open-dir-action (x)
  "Open dired on directory X in current project."
  (with-ivy-window
    (let* ((dir (cdr x)))
      (dired dir))))


(provide 'proviso-finder)
;;; proviso-finder.el ends here
