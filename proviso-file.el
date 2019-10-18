;;; proviso-file.el --- File utilities for proviso
;; Copyright (C) 2019  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Thursday, October 17, 2019
;; Version: 1.0
;; Modified Time-stamp: <2019-10-18 08:46:32 dharms>
;; Modified by: Dan Harms
;; Keywords: tools projects proviso
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
;; File utilities.
;;

;;; Code:
(require 'proviso-fd)
(require 'seq)
(require 'ivy)
(require 'xfer-util)

(defun proviso-file-choose-remove-action (x)
  "Remove file X."
  (let ((file (cdr x)))
    (delete-file file)))

(defun proviso-file-choose (dir prompt action &optional regex)
  "Perform ACTION of file chosen via PROMPT beneath DIR."
  (let* ((pattern (or regex "."))
        (files
         (cond ((xfer-util-find-executable "fd" dir)
                (proviso-fd-gather-files dir pattern))
               (t
                (directory-files-recursively dir pattern)))))
    (if (seq-empty-p files)
        (user-error "No files found")
      (ivy-read prompt
                (proviso-finder-adjust-paths
                 files
                 (file-remote-p dir)
                 (file-remote-p dir 'localname))
                :action action
                :caller #'proviso-file-choose))))

;;;###autoload
(defun proviso-file-remove-git-lock ()
  "Allow user to choose to delete any git lock files under DIR."
  (interactive)
  (let ((dir (or (proviso-current-project-root) default-directory)))
    (proviso-file-choose
     (read-directory-name "Search for lock file under: " dir)
     "Remove file: "
     #'proviso-file-choose-remove-action
     "index\\.lock$")))

;;;###autoload
(defun proviso-file-choose-file-delete (dir regex)
  "Allow user to choose a file to delete matching REGEX under DIR."
  (interactive "DSearch for files under: \nsMatching regex: ")
  (proviso-file-choose dir "Remove file: "
                       #'proviso-file-choose-remove-action
                       regex))

(provide 'proviso-file)
;;; proviso-file.el ends here
