;;; proviso-tags.el --- add tags functionality to profiles
;; Copyright (C) 2017  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Thursday, January  5, 2017
;; Version: 1.0
;; Modified Time-stamp: <2017-02-16 08:15:41 dharms>
;; Modified by: Dan Harms
;; Keywords: proviso tags

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

(require 'tramp)

(defun proviso-tags-compute-remote-subdir-stem (prof)
  "Compute remote profile PROF's stem.
Format is useful for uniquely naming the local TAGS directory."
  (concat
   (replace-regexp-in-string
    "/\\|\\\\" "!"
    (prof-get prof :remote-host) t t)
   "!"
   (replace-regexp-in-string
    "/\\|\\\\" "!"
    (prof-get prof :root-stem))))

(defun proviso-tags-compute-tags-dir (prof dir)
  "Compute where a profile PROF's local TAGS should live.
DIR gives the root directory."
  (let ((base (or (getenv "EMACS_TAGS_DIR") "~"))
        (sub (or (prof-get prof :tags-subdir) ".tags/"))
        dest)
    (unless dir (setq dir default-directory))
    (unless (tramp-tramp-file-p dir)
      ;; in the local case, set the base according to the project
      (setq base dir))
    (setq dest (concat (file-name-as-directory base) sub))
    (if (tramp-tramp-file-p dir)
        (concat dest (file-name-as-directory
                      (proviso-tags-compute-remote-subdir-stem prof)))
      dest)))

(defun proviso-tags-on-init (prof)
  "Initialize tags functionality when profile PROF is initialized."
  (let ((root (prof-get prof :root-dir)))
    (and root (not (prof-get prof :tags-dir))
         (prof-put prof :tags-dir
                   (proviso-tags-compute-tags-dir
                    prof
                    (concat (prof-get prof :remote-prefix)
                            root))))))

(add-hook 'prof-on-proviso-pre-init 'proviso-tags-on-init)

(provide 'proviso-tags)
;;; proviso-tags.el ends here
