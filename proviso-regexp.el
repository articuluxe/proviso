;;; proviso-regexp.el --- proviso regex manipulations
;; Copyright (C) 2017  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Wednesday, November  8, 2017
;; Version: 1.0
;; Modified Time-stamp: <2017-11-10 08:01:15 dharms>
;; Modified by: Dan Harms
;; Keywords: tools project proviso
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
;; Provides utilities to manage regular expressions in proviso.  The principle
;; use case is to convert from shell glob patterns into regular expressions.
;; We do not use `eshell-glob-regex' because its results use constructs
;; specific to Emacs, whereas we would prefer PCRE for maximum compatibility.

;;; Code:

(defun proviso-regexp-glob-to-regex (glob &optional no-anchor-begin
                                          no-anchor-end)
  "Convert a shell glob pattern GLOB to a regular expression.
NO-ANCHOR-BEGIN, if non-nil, means that no regex anchor character
`^' should be placed at the beginning of the result, even if
there were no wildcard at the beginning of the input.
NO-ANCHOR-END, if non-nil, means that no regex anchor character
`$' should be placed at the end of the result, even if there were
no wildcard at the end of the input.  Without these constraints,
the algorithm attempts to anchor the regex at its bounds in order
to match the absence of wildcards in the shell glob pattern that
is being replicated."
  (let ((result "")
        (anchor-begin (not (or (string-empty-p glob)
                               (string-match "^\\*" glob))))
        (anchor-end (not (or (string-empty-p glob)
                             (string-match "\\*$" glob)))))
    (dolist (ch (append glob nil) result)
      (setq
       result
       (concat
        result
        (cond ((eq ch ?*)
               ".*")
              ((eq ch ?.)
               "\\.")
              (t
               (char-to-string ch))))))
    (concat
     (and (not no-anchor-begin) anchor-begin "^")
     result
     (and (not no-anchor-end) anchor-end "$"))))

(provide 'proviso-regexp)
;;; proviso-regexp.el ends here
