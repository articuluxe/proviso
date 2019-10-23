;;; proviso-fd.el --- Support for fd in proviso
;; Copyright (C) 2019  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Friday, October 11, 2019
;; Version: 1.0
;; Modified Time-stamp: <2019-10-23 15:43:54 dan.harms>
;; Modified by: Dan Harms
;; Keywords: tools unix proviso project fd
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
;; Utilities enabling support for fd.
;;

;;; Code:
(require 'proviso-defines)
(require 'proviso-regexp)
(require 'seq)

(defcustom proviso-fd-args "-HIa"
  "Standard arguments to give to fd."
  :group 'proviso-custom-group)

(defun proviso-fd--create-inclusion-str (lst)
  "Create a substring for `fd' to match files from LST."
  (if (seq-empty-p lst)
      "."
    (concat "'("
            (mapconcat (lambda (elt)
                         (proviso-regexp-glob-to-regex elt))
                       lst "|")
            ")'")))

(defun proviso-fd--create-file-exclusion (lst)
  "Create a substring for `fd' to ignore files from LST."
  (mapconcat (lambda (elt)
               (concat "-E " elt)) lst " "))

(defun proviso-fd--create-dir-exclusion (lst)
  "Create a substring for `fd' to ignore dirs from LST."
  (mapconcat (lambda (elt)
               (concat "-E " elt)) lst " "))

(defun proviso-fd-gather-files-interactive (dir &optional reporter symbolic)
  "Gather a list of filenames recursively below directory DIR.
REPORTER is an optional progress reporter.  SYMBOLIC should be
non-nil to allow the presence of symlinks in the results.
Results are filtered via `proviso-interesting-files',
`proviso-uninteresting-files' and `proviso-uninteresting-dirs'."
  (let* ((proj (proviso-current-project))
         (exclude-files (or (proviso-get proj :grep-exclude-files)
                            proviso-uninteresting-files))
         (exclude-dirs (or (proviso-get proj :grep-exclude-dirs)
                           proviso-uninteresting-dirs))
         (include-files (or (proviso-get proj :grep-include-files)
                            proviso-interesting-files)))
    (proviso-fd-gather-files dir exclude-files exclude-dirs
                             include-files
                             symbolic)))

(defun proviso-fd-gather-files (dir &optional pattern
                                    exclude-files
                                    exclude-dirs
                                    include-files
                                    symbolic
                                    )
  "Return files matching PATTERN recursively beneath DIR.
EXCLUDE-FILES, EXCLUDE-DIRS and INCLUDE-FILES provide filters to
exclude and include results, respectively.  SYMBOLIC should be
non-nil to allow the presence of symlinks in the results."
  (let ((buffer (get-buffer-create " *proviso-fd*"))
        (cmd (concat "fd "
                     (if symbolic (concat proviso-fd-args "L")
                       proviso-fd-args)
                     (when exclude-files
                       (concat " "
                               (proviso-fd--create-file-exclusion exclude-files)))
                     (when exclude-dirs
                       (concat " "
                               (proviso-fd--create-dir-exclusion exclude-dirs)))
                     " "
                     (proviso-fd--create-inclusion-str
                      (if pattern (list pattern) include-files))
                     " "
                     dir))
        (default-directory dir)
        status)
    (with-current-buffer buffer
      (erase-buffer)
      (setq status (call-process-shell-command cmd nil t))
      (if (eq status 0)
          (split-string (buffer-string) "\n" t)
        (if (string-match "[fd error]: \\(.+\\)" (buffer-string))
            (user-error "Error: %s" (match-string 1)))
        nil))))

(provide 'proviso-fd)
;;; proviso-fd.el ends here
