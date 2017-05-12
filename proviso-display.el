;;; proviso-display.el --- display information about proviso
;; Copyright (C) 2017  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Tuesday, May  9, 2017
;; Version: 1.0
;; Modified Time-stamp: <2017-05-11 17:32:16 dharms>
;; Modified by: Dan Harms
;; Keywords: proviso project display

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

(defface proviso-face-heading '((((background dark)) (:foreground "Yellow"))
                                (t (:foreground "Blue")))
  "Face used to highlight headings.")

(defun proviso-display-print-project (proj)
  "Return a string containing a textual representation of PROJ."
  (with-output-to-string (pp (proviso-get-plist proviso-curr-proj))))

(defun proviso-display--get-project-names ()
  "Return a list containing the current project names in `proviso-obarray'."
  (let (lst)
    (mapatoms (lambda (atom)
                (push (symbol-name atom) lst)) proviso-obarray)
    lst))

;;;###autoload
(defun proviso-display-echo-project-names ()
  "Echo the project names contained in `proviso-obarray'."
  (interactive)
  (message "%s" (proviso-display--get-project-names)))

;;;###autoload
(defun proviso-display-projects (&optional arg)
  "Display information about current projects."
  (interactive "P")
  (if arg
      (let ((one-win (one-window-p)))
        (pop-to-buffer (get-buffer-create "*Project List*"))
        (when one-win (delete-other-windows)))
    (set-buffer (get-buffer-create "*Project List*")))
  (let ((inhibit-read-only t)
        (title "All Projects")
        elt name proj)
    (erase-buffer)
    (insert (format "%s\n%s\n" title (make-string (length title) ?-)))
    ;; (add-text-properties (point-min) (point) 'font-lock-face 'proviso-face-heading)
    (goto-char (point-min))
    (dolist (elt proviso-path-alist)
      (setq name (cdr elt))
      (setq proj (intern-soft name proviso-obarray))
      (insert "%s  --  %s" name (car elt))
      ;; (put-text-property
      )))

(provide 'proviso-display)
;;; proviso-display.el ends here
