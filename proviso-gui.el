;;; proviso-gui.el --- gui gramework for proviso
;; Copyright (C) 2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Thursday, August 23, 2018
;; Version: 1.0
;; Modified Time-stamp: <2018-09-12 09:18:19 dharms>
;; Modified by: Dan Harms
;; Keywords: tools proviso project
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
;; A gui framework.
;;

;;; Code:
(require 's)
(require 'seq)

(defvar-local proviso-gui-markers nil
  "List of markers in current buffer, for navigation.")

(defvar-local proviso-gui--local-map nil
  "Local map.")

(defvar proviso-gui-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" #'proviso-gui-move-next-marker)
;    (define-key map "<down>" #'proviso-gui-move-next-marker)
    (define-key map "p" #'proviso-gui-move-prev-marker)
;    (define-key map "<up>" #'proviso-gui-move-prev-marker)
    (define-key map "t" #'ignore)
    (define-key map "q" #'delete-window)
    map))

(defun proviso-gui-move-next-marker ()
  "Move to the next marker position in the dashboard buffer."
  (interactive)
  (let* ((pt (point))
         (next (seq-find (lambda (cell)
                           (> (marker-position (car cell)) pt))
                         proviso-gui-markers)))
    (when next
      (goto-char (marker-position (car next)))
      (proviso-gui-on-line))))

(defun proviso-gui-move-prev-marker ()
  "Move to the previous marker position in the dashboard buffer."
  (interactive)
  (let* ((pt (point))
         (prev (seq-find (lambda (cell)
                           (< (marker-position (car cell)) pt))
                         (reverse proviso-gui-markers))))
    (when prev
      (goto-char (marker-position (car prev)))
      (proviso-gui-on-line))))

(defun proviso-gui--find-current-cell ()
  "Return the element of `proviso-gui-markers' near point, if any."
  (let ((beg (line-beginning-position))
        (end (line-end-position))
        pos)
    (seq-find (lambda (elt)
                (setq pos (marker-position (car elt)))
                (and (>= pos beg)
                     (<= pos end)))
              proviso-gui-markers)))

(defun proviso-gui-on-line ()
  "Examine the current line, set the current keymap if necessary."
  (let ((cell (proviso-gui--find-current-cell)))
    (when cell
      (use-local-map (cdr cell)))))

(defun proviso-gui-init-buffer (buffer keymap)
  "Initialize BUFFER for gui operations, with keymap KEYMAP."
  (let ((inhibit-read-only t))
    (with-current-buffer buffer
      (setq proviso-gui-markers nil)
      (put 'proviso-gui--local-map 'permanent-local t)
      (erase-buffer)
      (setq proviso-gui--local-map (copy-keymap keymap))
      (set-keymap-parent proviso-gui-map (keymap-parent keymap))
      (set-keymap-parent proviso-gui--local-map proviso-gui-map)
      (define-key proviso-gui--local-map [remap previous-line]
        #'proviso-gui-move-prev-marker)
      (define-key proviso-gui--local-map [remap next-line]
        #'proviso-gui-move-next-marker)
      )))

(defun proviso-gui-cb (cb create marker)
  "Callback for callable CB.  Content is given by CREATE, position by MARKER."
  (interactive)
  (funcall cb)
  (let ((inhibit-read-only t)
        (pos (marker-position marker)))
    (goto-char pos)
    (delete-region pos (line-end-position))
    (insert (funcall create))
    (goto-char pos)))

(defun proviso-gui-add-to-buffer (buffer lst &optional maxwidth)
  "Add GUI elements TO BUFFER based on LST.
Returns a sorted list of markers in the buffer.
MAXWIDTH allows specifying the minimum length of the headings."
  (let ((inhibit-read-only t)
        (max-heading (or maxwidth 0))
        pred content)
    (with-current-buffer buffer
      (setq lst (seq-remove (lambda (elt)
                              (setq pred (plist-get elt :predicate))
                              (and pred (functionp pred)
                                   (not (funcall pred))))
                            lst))
      (dolist (entry lst)
        (setq max-heading (max (string-width (plist-get entry :heading))
                               max-heading)))
      (dolist (entry lst)
        (setq pred (plist-get entry :content))
        (insert (s-pad-left max-heading " " (plist-get entry :heading)))
        (insert ": ")
        (let ((map (make-sparse-keymap))
              (marker (point-marker)))
          (set-marker-insertion-type marker nil)
          (dolist (binding (plist-get entry :bindings))
            (lexical-let ((cb (cdr binding))
                          (create (plist-get entry :content))
                          (marker marker))
              (define-key map (car binding)
                (lambda() (interactive)
                  (proviso-gui-cb cb create marker)))))
          (set-keymap-parent map proviso-gui--local-map)
          (add-to-list 'proviso-gui-markers
                       (cons marker map) t)
          (insert (funcall pred) "\n"))
        )
      )))

(defun proviso-gui--sort-markers (buffer)
  "Sort the markers used in BUFFER."
  (setq proviso-gui-markers
        (sort proviso-gui-markers (lambda (lhs rhs)
                                    (< (marker-position (car lhs))
                                       (marker-position (car rhs)))))))

(defun proviso-gui-finalize-buffer (buffer)
  "Finalize the GUI settings of BUFFER."
  (with-current-buffer buffer
    (use-local-map proviso-gui--local-map)
    (proviso-gui-on-line)))

(provide 'proviso-gui)
;;; proviso-gui.el ends here
