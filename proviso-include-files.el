;;; proviso-include-files.el --- setup proviso project include files
;; Copyright (C) 2017  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Thursday, March 30, 2017
;; Version: 1.0
;; Modified Time-stamp: <2017-03-31 17:40:38 dharms>
;; Modified by: Dan Harms
;; Keywords: proviso project include files

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

(defun proviso--validate-include-files (proj)
  "Validate the set of include files of project PROJ."
  (let ((remote (proviso-get proj :remote-prefix))
        (root (proviso-get proj :root-dir))
        (lst (proviso-get proj :proj-alist))
        entry path)
    (setq proviso--ignore-load-errors nil)
    (setq lst
          (seq-filter (lambda (elt)
                        (setq entry (plist-get elt :dir))
                        (setq path
                              (concat
                               remote
                               (concat
                                (when (or (zerop (length entry))
                                          (f-relative? entry))
                                  root)
                                entry)))
                        (cond ((null entry) nil)
                              ((f-exists? path) path)
                              (proviso--ignore-load-errors nil)
                              (t
                               (proviso--query-error
                                proj
                                (format "%s does not exist!" path)))))
                      lst))
    (proviso-put proj :proj-alist lst)))

(defun proviso--set-include-files (proj)
  "Set include files according to PROJ's project definition."
  (let ((remote (proviso-get proj :remote-prefix))
        (root (proviso-get proj :root-dir))
        (lst (proviso-get proj :proj-alist))
        elt entry includes ff-includes)
    ;; in case there are no entries, add a default one (will resolve to root-dir)
    (when (zerop (length lst))
      (push (list) lst))
    (dolist (element lst)
      (setq entry (plist-get element :dir))
      (setq elt (concat (when (or (null entry) (f-relative? entry)) root) entry))
      (push elt includes)
      (push (concat remote elt) ff-includes))
    (proviso-put proj :include-files includes)
    ;; ff-search-directories doesn't want a trailing slash
    (proviso-put proj :include-ff-files (mapcar 'directory-file-name ff-includes))
    ))

(add-hook 'proviso-on-project-init 'proviso--set-include-files)
;; add the validation last so it runs first
(add-hook 'proviso-on-project-init 'proviso--validate-include-files)

(provide 'proviso-include-files)
;;; proviso-include-files.el ends here
