;;; proviso-test-common.el --- common test utilities for proviso
;; Copyright (C) 2017-2020  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Friday, March 31, 2017
;; Version: 1.0
;; Modified Time-stamp: <2020-01-08 09:00:18 Dan.Harms>
;; Modified by: Dan.Harms
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

(require 'ert)
(defvar load-name load-file-name)
(defvar absolute-root-dir
  (cond ((eq system-type 'windows-nt)
         (expand-file-name "c:\\Users"))
        (t "/home"))
  "An absolute path name near the root of the current host.")
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
