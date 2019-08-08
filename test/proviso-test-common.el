;;; proviso-test-common.el --- common test utilities for proviso
;; Copyright (C) 2017-2019  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Friday, March 31, 2017
;; Version: 1.0
;; Modified Time-stamp: <2019-08-08 08:09:15 dharms>
;; Modified by: Dan Harms
;; Keywords: tools proviso test

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
;; Proviso tests: common functionality.
;;

;;; Code:

;; transfer dependencies from argv into load-path
(let ((lst (cdr argv))
      add elt)
  (setq argv nil)
  (while lst
    (setq elt (car lst))
    (if add
        (progn
          (push elt load-path)
          (setq add nil))
      (unless
          (setq add (string= elt "-L"))
        (push elt argv)))
      (setq lst (cdr lst))))
(push (concat (file-name-directory load-file-name) "/..") load-path)
(push (file-name-directory load-file-name) load-path)

(require 'ert)
(setq debug-on-error t)
(setq bmkp-last-as-first-bookmark-file nil)

;; project-specific code begins here
(require 'proviso-core)
(setq proviso-finder-file-cache-enabled nil)

;; helper defun
(defun proviso-test-reset-all ()
  "Reset all profile-related data structures to nil."
  (setq proviso-obarray (make-vector 7 0))
  (setq proviso-provisional-obarray (make-vector 10 0))
  (setq proviso-path-alist nil)
  (setq proviso-proj-alist nil)
  (setq proviso-curr-proj nil)
  (setq proviso-local-proj (default-value 'proviso-local-proj))
  (setq proviso-projects (ht-create 'equal))
  )



;;; proviso-test-common.el ends here
