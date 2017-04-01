;;; proviso-grep.el --- setup proviso grep
;; Copyright (C) 2017  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Saturday, April  1, 2017
;; Version: 1.0
;; Modified Time-stamp: <2017-04-01 14:05:57 dharms>
;; Modified by: Dan Harms
;; Keywords: proviso project grep

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

(defun proviso--set-grep-dirs (proj)
  "Set grep directories according to PROJ's project definition."
  (let ((root (proviso-get proj :root-dir))
        (lst (proviso-get proj :proj-alist))
        elt entry dirs)
    (dolist (element lst)
      (setq entry (plist-get element :dir))
      (setq elt (if (and entry (file-name-absolute-p entry))
                    entry
                  (concat root entry)))
      (push elt dirs))
    (proviso-put proj :grep-dirs (delete-dups (append dirs `(,root))))
    ))

(add-hook 'proviso-on-project-init 'proviso--set-grep-dirs)

(provide 'proviso-grep)
;;; proviso-grep.el ends here
