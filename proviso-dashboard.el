;;; proviso-dashboard.el --- a dashboard for proviso projects
;; Copyright (C) 2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Wednesday, May 16, 2018
;; Version: 1.0
;; Modified Time-stamp: <2018-06-01 17:19:26 dharms>
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

(defvar-local proviso-dashboard--local-map nil
  "The local map in use in `proviso-dashboard-mode'.")

(defvar proviso-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" #'proviso-dashboard-move-next-marker)
    (define-key map "p" #'proviso-dashboard-move-prev-marker)
    (define-key map "t" #'ignore)
    map))

(defvar-local proviso-dashboard-buffer-name nil
  "Buffer name for `proviso-dashboard' mode.")

(defconst proviso-dashboard-buffer-name-prefix "*%s-project*"
  "Buffer prefix string for `proviso-dashboard'.
This will be formatted with the project name.")

(defun proviso-dashboard--find-current-cell ()
  "Return the element of `proviso-dashboard-markers' near point, if any."
  (let ((beg (line-beginning-position))
        (end (line-end-position))
        pos)
    (seq-find (lambda (elt)
                (setq pos (marker-position (car elt)))
                (and (>= pos beg)
                     (<= pos end)))
              proviso-dashboard-markers)))

(defun proviso-dashboard-on-line ()
  "Examine the current line, set the current keymap if necessary."
  (let ((cell (proviso-dashboard--find-current-cell)))
    (when cell
      (set-keymap-parent (cdr cell) proviso-dashboard--local-map)
      (use-local-map (cdr cell)))))

(defun proviso-dashboard-move-next-marker ()
  "Move to the next marker position in the dashboard buffer."
  (interactive)
  (let* ((pt (point))
         (next (seq-find (lambda (cell)
                           (> (marker-position (car cell)) pt))
                         proviso-dashboard-markers)))
    (when next
      (goto-char (marker-position (car next)))
      (proviso-dashboard-on-line))))

(defun proviso-dashboard-move-prev-marker ()
  "Move to the previous marker position in the dashboard buffer."
  (interactive)
  (let* ((pt (point))
         (prev (seq-find (lambda (cell)
                           (< (marker-position (car cell)) pt))
                         (reverse proviso-dashboard-markers))))
    (when prev
      (goto-char (marker-position (car prev)))
      (proviso-dashboard-on-line))))

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

(defun proviso-dashboard-goto-root ()
  "Go to root dired of the current project."
  (interactive)
  (let ((proj proviso-local-proj))
    (when proj
      (proviso-dired-open-project proj))))

(defun proviso-dashboard-create (proj)
  "Create a dashboard for project PROJ."
  (interactive)
  (setq-local proviso-dashboard-buffer-name
              (format proviso-dashboard-buffer-name-prefix proj))
  (with-current-buffer (get-buffer-create proviso-dashboard-buffer-name)
    (let ((inhibit-read-only t)
          (remote (proviso-get proj :remote-host))
          (bmk (proviso-get proj :bookmark-file))
          (clang (proviso-get proj :clang-format))
          )
      (erase-buffer)
      (proviso-dashboard-mode)
      (setq-local proviso-local-proj proj)
      (setq-local proviso-dashboard--local-map (current-local-map))
      (setq proviso-dashboard-markers nil)
      (insert "     Project: ")
      (insert (propertize (proviso-get proj :project-name)
                          'face 'highlight))
      (insert "\n        Root: ")
      (push (cons (point-marker)
                  (let ((map (make-sparse-keymap)))
                    (define-key map "d" #'proviso-dashboard-goto-root)
                    map))
            proviso-dashboard-markers)
      (insert (propertize (proviso-get proj :root-dir)
                          'face '(bold)))
      (insert "\n")
      (when remote
        (insert " Remote host: ")
        (insert (propertize remote 'face '(bold)))
        (insert "\n"))
      (insert "        Tags: ")
      (push (cons (point-marker)
                  (let ((map (make-sparse-keymap)))
                    map))
            proviso-dashboard-markers)
      (insert (propertize (proviso-get proj :tags-dir)
                          'face '(bold)))
      (insert "\n")
      (insert "   Bookmarks: ")
      (push (cons (point-marker)
                  (let ((map (make-sparse-keymap)))
                    map))
            proviso-dashboard-markers)
      (insert (propertize bmk 'face
                          (if (and bmk (file-exists-p bmk))
                              '(bold)
                            '(shadow))))
      (insert "\n")
      (insert "Clang format: ")
      (push (cons (point-marker)
                  (let ((map (make-sparse-keymap)))
                    map))
            proviso-dashboard-markers)
      (insert (propertize clang 'face
                          (if (and clang (file-exists-p clang))
                              '(bold)
                            '(shadow))))
      (insert "\n")
      (setq proviso-dashboard-markers
            (sort proviso-dashboard-markers (lambda (lhs rhs)
                                              (< (marker-position (car lhs))
                                                 (marker-position (car rhs))))))
      (proviso-dashboard-on-line)
      )))

;;;###autoload
(defun proviso-dashboard-switch-to (proj)
  "Switch to the dashboard for projecdt PROJ in other window."
  (when proj
    (proviso-dashboard-create proj)
    (display-buffer proviso-dashboard-buffer-name)))

;; (defun proviso-dashboard-refresh-buffer ()
;;   "Refresh the dashboard."
;;   (interactive)
;;   (kill-buffer proviso-dashboard-buffer-name)
;;   (proviso-dashboard-create (proviso-current-project))
;;   (switch-to-buffer proviso-dashboard-buffer-name))

(add-hook 'proviso-hook-on-project-post-init #'proviso-dashboard-switch-to)

(provide 'proviso-dashboard)
;;; proviso-dashboard.el ends here
