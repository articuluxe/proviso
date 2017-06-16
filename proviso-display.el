;;; proviso-display.el --- display information about proviso
;; Copyright (C) 2017  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Tuesday, May  9, 2017
;; Version: 1.0
;; Modified Time-stamp: <2017-06-15 17:43:08 dharms>
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
(require 'ivy)

(defface proviso-face-heading '((((background dark)) (:foreground "Yellow"))
                                (t (:foreground "Blue")))
  "Face used to highlight headings.")

;;;###autoload
(defun proviso-display-choose-project ()
  "Allow the user to choose a project among those currently defined."
  (interactive)
  (let (lst)
    (mapatoms (lambda (atom)
                (push (symbol-name atom) lst)) proviso-obarray)
    (if (seq-empty-p lst)
        (error "No projects defined")
      (ivy-read "Project: " lst
                :caller 'proviso-display-choose-project
                ))))

;;;###autoload
(defun proviso-display-print-project ()
  "Print properties of a selected project."
  (interactive)
  (let ((proj (proviso-display-choose-project)))
    (when proj
      (with-output-to-temp-buffer
          (format "*Proviso-project: %s*" proj)
        (proviso-display--print-project proj)))))

(defun proviso-display--print-project (proj)
  "Return a string containing a textual representation of PROJ."
  (pp (proviso-get-plist proj)))

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
  (let (projs (proviso-display--get-project-names))
    (if projs
        (message "%s" projs)
      (error "No projects"))))

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
      )
    (proviso-display-mode)
    ))

(defcustom proviso-prefix-key
  "\C-cp"
  "Prefix key for `proviso'.")

(defvar proviso-display-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map proviso-prefix-key #'proviso-display-mode)
    map)
  "Keymap for `proviso-display-mode'.")

(defun proviso-display-mode ()
  "Provide a mode to view information about proviso projects."
  (kill-all-local-variables)
  (use-local-map proviso-display-mode-map)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq major-mode 'proviso-display-mode)
  (setq mode-name "Proviso")
  (run-mode-hooks 'proviso-display-mode-hook))

(provide 'proviso-display)
;;; proviso-display.el ends here
