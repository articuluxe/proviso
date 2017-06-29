;;; proviso-display.el --- display information about proviso
;; Copyright (C) 2017  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Tuesday, May  9, 2017
;; Version: 1.0
;; Modified Time-stamp: <2017-06-29 11:27:23 dharms>
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
(require 'tabulated-list)

(defface proviso-face-heading '((((background dark)) (:foreground "Yellow"))
                                (t (:foreground "Blue")))
  "Face used to highlight headings.")

;;;###autoload
(defun proviso-display-print-project ()
  "Print properties of a selected project."
  (interactive)
  (let ((proj (proviso-choose-project)))
    (when proj
      (with-output-to-temp-buffer (format "*Proviso-project: %s*" proj)
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
  (let ((projs (proviso-display--get-project-names)))
    (if projs
        (message "%s" projs)
      (error "No projects"))))

(defvar proviso-display-buffer-name "*proviso-projects*"
  "Name of the buffer describing proviso projects.")

(defun proviso-display-entries ()
  "Generate entry list for tabulated-list."
  (let (lst)
    (mapatoms (lambda (atom)
                (push (symbol-name atom) lst)) proviso-obarray)
    (mapcar (lambda (elt)
              (let ((root (proviso-get elt :root-dir))
                    (remote (or (proviso-get elt :remote-host) "")))
                (list elt
                      (vconcat
                       (list elt
                             root
                             remote
                             )))))
            lst)))

(define-derived-mode proviso-display-mode
  tabulated-list-mode "Proviso"
  "Major mode for displaying proviso projects.
\\{proviso-display-mode-map\}"
  (setq tabulated-list-format [("Project" 15 t)
                               ("Root" 55 t)
                               ("Remote" 30 t)
                               ])
  (setq tabulated-list-sort-key (cons "Project" nil))
  (setq tabulated-list-padding 2)
  (setq tabulated-list-entries #'proviso-display-entries)
  (tabulated-list-init-header))

;;;###autoload
(defun proviso-display-projects ()
  "Display proviso projects."
  (interactive)
  (let ((buf (get-buffer-create proviso-display-buffer-name)))
    (pop-to-buffer buf)
    (proviso-display-mode)
    (tabulated-list-print)))

(define-key proviso-display-mode-map "r" #'proviso-display-open-root-dired)

(defun proviso-display-open-root-dired ()
  "Open a dired buffer at the root location of the selected project."
  (interactive)
  (let* ((name (tabulated-list-get-id))
         (proj (intern-soft name proviso-obarray))
         dir)
    (and proj
         (setq dir (directory-file-name
                    (concat
                     (proviso-get proj :remote-prefix)
                     (proviso-get proj :root-dir))))
         (file-readable-p dir)
         (dired dir))))

(provide 'proviso-display)
;;; proviso-display.el ends here
