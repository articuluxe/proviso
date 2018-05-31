;;; proviso-dashboard.el --- a dashboard for proviso projects
;; Copyright (C) 2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Wednesday, May 16, 2018
;; Version: 1.0
;; Modified Time-stamp: <2018-05-31 08:51:46 dharms>
;; Modified by: Dan Harms
;; Keywords: tools proviso projects
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
;; A dashboard for a project overview.
;;

;;; Code:
(require 'proviso-core)

(defvar-local proviso-dashboard-markers nil
  "List of markers in dashboard buffer to navigate to.")

(defvar proviso-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "t" 'ignore)
    (define-key map "n" 'proviso-dashboard-move-next-marker)
    (define-key map "p" 'proviso-dashboard-move-prev-marker)
    map))

(defconst proviso-dashboard-buffer-name "*projects*"
  "Buffer name for `proviso-dashboard' mode.")

(defun proviso-dashboard-move-next-marker ()
  "Move to the next marker position in the dashboard buffer."
  (interactive)
  (let* ((pt (point))
         (next (seq-find (lambda (pos)
                           (> pos pt))
                         proviso-dashboard-markers)))
    (and next (goto-char next))))

(defun proviso-dashboard-move-prev-marker ()
  "Move to the previous marker position in the dashboard buffer."
  (interactive)
  (let* ((pt (point))
         (prev (seq-find (lambda (pos)
                           (< pos pt))
                         (reverse proviso-dashboard-markers))))
    (and prev (goto-char prev))))

(define-derived-mode proviso-dashboard-mode special-mode
  "Dashboard"
  "Major mode for providing an overview of a proviso project.
\\<proviso-dashboard-mode-map>
"
  (setq buffer-read-only t)
  (setq truncate-lines t)
  )

;;;###autoload
(defun proviso-dashboard-show (&optional arg)
  "Show a dashboard.
Optional ARG allows choosing a project."
  (interactive "P")
  (let ((proj (if arg (proviso-choose-project)
                (proviso-current-project))))
    (proviso-dashboard-create proj)
    (pop-to-buffer proviso-dashboard-buffer-name)))

(defun proviso-dashboard-create (proj)
  "Create a dashboard for project PROJ."
  (interactive)
  (with-current-buffer (get-buffer-create proviso-dashboard-buffer-name)
    (let ((inhibit-read-only t)
          (remote (proviso-get proj :remote-host))
          (bmk (proviso-get proj :bookmark-file))
          (clang (proviso-get proj :clang-format))
          )
      (erase-buffer)
      (proviso-dashboard-mode)
      (setq proviso-dashboard-markers nil)
      (insert "     Project: ")
      (insert (propertize (proviso-get proj :project-name)
                          'face 'highlight))
      (insert "\n        Root: ")
      (push (point-marker) proviso-dashboard-markers)
      (insert (propertize (proviso-get proj :root-dir)
                          'face '(bold)))
      (insert "\n")
      (when remote
        (insert " Remote host: ")
        (insert (propertize remote 'face '(bold)))
        (insert "\n"))
      (insert "        Tags: ")
      (push (point-marker) proviso-dashboard-markers)
      (insert (propertize (proviso-get proj :tags-dir)
                          'face '(bold)))
      (insert "\n")
      (insert "   Bookmarks: ")
      (push (point-marker) proviso-dashboard-markers)
      (insert (propertize bmk 'face
                          (if (and bmk (file-exists-p bmk))
                              '(bold)
                            '(shadow))))
      (insert "\n")
      (insert "Clang format: ")
      (push (point-marker) proviso-dashboard-markers)
      (insert (propertize clang 'face
                          (if (and clang (file-exists-p clang))
                              '(bold)
                            '(shadow))))
      (insert "\n")
      (setq proviso-dashboard-markers
            (sort proviso-dashboard-markers (lambda (lhs rhs)
                                              (< (marker-position lhs)
                                                 (marker-position rhs)))))
      )))

;;;###autoload
(defun proviso-dashboard-switch-to (proj)
  "Switch to the dashboard for projecdt PROJ in other window."
  (proviso-dashboard-create proj)
  (display-buffer proviso-dashboard-buffer-name))

(defun proviso-dashboard-refresh-buffer ()
  "Refresh the dashboard."
  (interactive)
  (kill-buffer proviso-dashboard-buffer-name)
  (proviso-dashboard-create (proviso-current-project))
  (switch-to-buffer proviso-dashboard-buffer-name))

(add-hook 'proviso-hook-on-project-post-init #'proviso-dashboard-switch-to)

(provide 'proviso-dashboard)
;;; proviso-dashboard.el ends here
