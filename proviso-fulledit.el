;;; proviso-fulledit.el --- full-edit for proviso
;; Copyright (C) 2017  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Wednesday, September 20, 2017
;; Version: 1.0
;; Modified Time-stamp: <2017-09-20 17:04:47 dharms>
;; Modified by: Dan Harms
;; Keywords: project proviso
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
;; This code loads all interesting files into the running instance of Emacs.
;;

;;; Code:
(require 'seq)

(defvar proviso-fulledit-accept-patterns
  '( "\\.cpp$" "\\.cc$" "\\.cxx$" "\\.c$" "\\.C$"
     "\\.h$" "\\.hh$" "\\.hpp$" "\\.hxx$" "\\.H$"
     "\\.sh$" "\\.py$" "\\.sql$" "\\.java$" "\\.in$"
     "\\.proto$" "\\.el$" "\\.cs$"
     "^CMakeLists.txt$" "\\.cmake$"
     "^Makefile$" "^makefile$"
     )
  "List of regexps which `proviso-fulledit' will open.")
(defvar proviso-fulledit-reject-patterns
  '( "\\.exe$" "\\.pdb$" "\\.obj$"
     )
  "List of regexps which `proviso-fulledit' will ignore.")

(defun proviso-fulledit-test-list-for-string (lst input)
  "Return non-nil if there exists in LST a match for string INPUT.
LST is a list of regexes."
  (catch 'found
    (dolist (curr lst)
      (if (string-match curr input)
          (throw 'found t)))
    nil))

(defun proviso-fulledit-gather-all-files (dir reporter &optional symbolic)
  "Gather a list of filenames recursively below directory DIR.
REPORTER is a progress reporter.  SYMBOLIC should be non-nil to
allow the presence of symlinks in the results.  Results are
filtered via `proviso-fulledit-accept-patterns' and
`proviso-fulledit-reject-patterns'."
  (let* ((all-results
          (directory-files
           dir t directory-files-no-dot-files-regexp t))
         (files (seq-remove 'file-directory-p all-results))
         (dirs (seq-filter 'file-directory-p all-results))
         (result '()))
    (unless symbolic
      (setq files (seq-remove 'file-symlink-p files))
      (setq dirs (seq-remove 'file-symlink-p dirs)))
    (dolist (file files)
      (and
       (proviso-fulledit-test-list-for-string
        proviso-fulledit-accept-patterns
        (file-name-nondirectory file))
       (not (proviso-fulledit-test-list-for-string
             proviso-fulledit-reject-patterns
             (file-name-nondirectory file)))
       (setq result (cons file result))
       (progress-reporter-update reporter)
       ))
    (dolist (dir dirs)
      (setq
       result
       (nconc
        result
        (proviso-fulledit-gather-all-files dir reporter symbolic))))
    result
    ))

(defun proviso-fulledit-open-file-list (files)
  "Find (open) each of a list of filenames FILES."
  (let* ((i 0)
         (len (length files))
         (reporter (make-progress-reporter "Opening files..." 0 len)))
    (dolist (file files)
      (find-file-noselect file)
      (setq i (+ i 1))
      (progress-reporter-update reporter i))
    (progress-reporter-done reporter)))

(defun proviso-fulledit (root &optional arg)
  "Find (open) all files recursively below directory ROOT.
With optional prefix argument ARG, will follow symbolic targets."
  (interactive
   `(,(read-directory-name "Full-Edit Directory: " nil nil t)
     ,current-prefix-arg))
  (if root
      (let* ((reporter (make-progress-reporter "Gathering files..."))
             (files (proviso-fulledit-gather-all-files (expand-file-name root)
                                                       reporter arg)))
        (progress-reporter-done reporter)
        (proviso-fulledit-open-file-list files)
        )
    (message "No directory given")))

(provide 'proviso-fulledit)
;;; proviso-fulledit.el ends here
