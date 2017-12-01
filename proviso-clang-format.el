;;; proviso-clang-format.el --- utility to run clang-format
;; Copyright (C) 2017  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Friday, November 10, 2017
;; Version: 1.0
;; Modified Time-stamp: <2017-12-01 08:12:49 dharms>
;; Modified by: Dan Harms
;; Keywords: tools unix proviso project clang-format
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
;; Helper utilities to run clang-format in a project.
;;

;;; Code:
(require 'proviso-core)
(require 'clang-format)

(defvar proviso-clang-format-active-p nil
  "Non-nil if clang-format should be called.
Note there are other dependencies: clang-format must be
installed; a .clang-format file must be found, etc.")

;;;###autoload
(defun proviso-clang-format-toggle-active ()
  "Select whether clang-format is active.
See `proviso-clang-format-active-p'."
  (interactive)
  (setq proviso-clang-format-active-p
        (if proviso-clang-format-active-p
            nil t))
  (message "proviso-clang is now %sactive"
           (if proviso-clang-format-active-p "" "in")))

;;;###autoload
(defun proviso-clang-format-buffer-or-region (&optional arg)
  "Clang-format a buffer according to the current project.
ARG is used to operate on the current region."
  (interactive "P")
  (let* ((proj (proviso-current-project))
         (file (proviso-get proj :clang-format)))
    (and file
         (f-exists? file)
         (if (region-active-p)
             (clang-format-region (region-beginning) (region-end))
           (clang-format-buffer)))))

(defun proviso-clang-format-maybe-buffer ()
  "Possibly format a buffer, contingent on certain conditions.
`proviso-clang-format-active-p' must be true.
Settings file `.clang-format' must be specified, and exist."
  (when proviso-clang-format-active-p
    (proviso-clang-format-buffer-or-region)))

(defun proviso-clang-format--setup-buffer (proj mode)
  "Setup a buffer's clang-format according to the settings in PROJ.
MODE is the `major-mode'."
  (when (or (eq mode 'c-mode)
            (eq mode 'c++-mode))
    (add-hook 'before-save-hook 'proviso-clang-format-maybe-buffer nil t)))

(defun proviso-clang-format--init (proj)
  "Set up clang-format according to PROJ's project definition."
  (let ((remote (proviso-get proj :remote-prefix))
        (root (proviso-get proj :root-dir))
        (name (or (proviso-get proj :clang-format)
                  ".clang-format"))
        path)
    (setq path (if (f-relative? name)
                   (concat remote root name)
                 (concat remote name)))
    ;; if .clang-format doesn't exist in the root, also check the first
    ;; (privileged) src dir
    (unless (or (f-exists? path) (f-absolute? name))
      (let ((lst (proviso-get proj :proj-alist))
            dir)
        (when (car lst)
          (when (setq dir (plist-get (car lst) :dir))
            (setq path (concat remote root
                               (file-name-as-directory dir)
                               name))))))
    (proviso-put proj :clang-format path)))

(add-hook 'proviso-hook-on-project-init #'proviso-clang-format--init)
(add-hook 'proviso-hook-on-file-opened #'proviso-clang-format--setup-buffer)

(provide 'proviso-clang-format)
;;; proviso-clang-format.el ends here
