;;; proviso-dired.el --- proviso dired utilities
;; Copyright (C) 2017  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Wednesday, June 28, 2017
;; Version: 1.0
;; Modified Time-stamp: <2017-06-28 08:36:35 dharms>
;; Modified by: Dan Harms
;; Keywords: proviso project dired

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

;;

;;; Code:
(require 'proviso-core)
(require 'ivy)

(defun proviso-gather-dired-dirs (proj)
  "Gather all dired targets for project PROJ."
  (let ((remote (proviso-get proj :remote-prefix))
        (root (proviso-get proj :root-dir))
        (blddirs (proviso-get proj :build-subdirs))
        (srcdirs (proviso-get proj :include-files))
        lst entry dir)
    (dolist (element blddirs)
      (setq entry (plist-get element :dir))
      (setq dir (if (and entry (file-name-absolute-p entry))
                    entry (concat root entry)))
      (add-to-list 'lst (cons dir (concat remote dir))))
    (dolist (element srcdirs)
      (add-to-list 'lst (cons element (concat remote element))))
    (add-to-list 'lst (cons root (concat remote root)))
    lst))

;;;###autoload
(defun proviso-open-dired ()
  "Open a dired buffer in some directory according to the current project."
  (interactive)
  (let ((cands (proviso-gather-dired-dirs (proviso-current-project)))
        result)
    (ivy-read "Open dired: " cands
              :caller 'proviso-open-dired
              :action (lambda (x)
                        (let ((file (directory-file-name (cdr x))))
                          (if (file-readable-p file)
                              (dired file)
                            (error "%s does not exist!" file)))))))

(provide 'proviso-dired)
;;; proviso-dired.el ends here
