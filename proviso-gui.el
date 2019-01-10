;;; proviso-gui.el --- gui gramework for proviso
;; Copyright (C) 2018-2019  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Thursday, August 23, 2018
;; Version: 1.0
;; Modified Time-stamp: <2019-01-10 08:12:39 dharms>
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
  "Alist of markers in current buffer, for navigation.")

(defvar-local proviso-gui--local-map nil
  "Local proviso gui key map.")

(defvar-local proviso-gui--next-id 0
  "Counter for next entry id.")

(defvar-local proviso-gui--timers nil
  "List of timers in effect.")

(defun proviso-gui-close-window ()
  "Close the gui window and kill the gui buffer.."
  (interactive)
  (kill-buffer)
  (delete-window))

(defvar proviso-gui-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" #'proviso-gui-move-next-marker)
    (define-key map "p" #'proviso-gui-move-prev-marker)
    (define-key map "t" #'ignore)
    (define-key map "q" #'proviso-gui-close-window)
    map))

(defun proviso-gui-on-buffer-kill ()
  "Hook function run when a buffer is killed."
  (dolist (timer proviso-gui--timers)
    (cancel-timer timer))
  (setq proviso-gui--timers nil))

(defun proviso-gui-on-timer (future cell)
  "Timer callback to check FUTURE, regarding CELL."
  (if (async-ready future)
      (let ((buffer (cdr (assq 'buffer cell)))
            (marker (cdr (assq 'pos cell)))
            (create (cdr (assq 'create cell))))
        (proviso-gui--draw-cell buffer marker create))
    (run-at-time 1 nil #'proviso-gui-on-timer future cell)))

(defun proviso-gui-move-next-marker ()
  "Move to the next marker position in the dashboard buffer."
  (interactive)
  (let* ((pt (point))
         (next (seq-find (lambda (cell)
                           (> (marker-position
                               (cdr (assq 'pos cell)))
                              pt))
                         proviso-gui-markers)))
    (when next
      (goto-char (marker-position (cdr (assq 'pos next))))
      (proviso-gui-on-line))))

(defun proviso-gui-move-prev-marker ()
  "Move to the previous marker position in the dashboard buffer."
  (interactive)
  (let* ((pt (point))
         (prev (seq-find (lambda (cell)
                           (< (marker-position
                               (cdr (assq 'pos cell)))
                              pt))
                         (reverse proviso-gui-markers))))
    (when prev
      (goto-char (marker-position (cdr (assq 'pos prev))))
      (proviso-gui-on-line))))

(defun proviso-gui--find-current-cell ()
  "Return the element of `proviso-gui-markers' near point, if any."
  (let ((beg (line-beginning-position))
        (end (line-end-position))
        pos)
    (seq-find (lambda (cell)
                (setq pos (marker-position
                           (cdr (assq 'pos cell))))
                (and (>= pos beg)
                     (<= pos end)))
              proviso-gui-markers)))

(defun proviso-gui-on-line ()
  "Examine the current line, set the current keymap if necessary."
  (let ((cell (proviso-gui--find-current-cell)))
    (when cell
      (use-local-map (cdr (assq 'map cell))))))

(defun proviso-gui-init-buffer (buffer keymap)
  "Initialize BUFFER for gui operations, with keymap KEYMAP."
  (let ((inhibit-read-only t))
    (with-current-buffer buffer
      (unless (memq #'proviso-gui-on-buffer-kill kill-buffer-hook)
        (add-hook 'kill-buffer-hook #'proviso-gui-on-buffer-kill nil t))
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
      (define-key proviso-gui--local-map [remap left-char]
        #'proviso-gui-move-prev-marker)
      (define-key proviso-gui--local-map [remap right-char]
        #'proviso-gui-move-next-marker)
      )))

(defun proviso-gui-get-next-entry-id (buffer)
  "Return the next id in BUFFER for identifying entries."
  (incf proviso-gui--next-id))

(defun proviso-gui-cb (cb cell)
  "Execute callback CB for gui element corresponding to CELL.
CELL is an alist of properties."
  (interactive)
  (let ((buffer (cdr (assq 'buffer cell)))
        (marker (cdr (assq 'pos cell)))
        (create (cdr (assq 'create cell)))
        (future (funcall cb)))
    (and future (processp future)
         (run-at-time 1 nil #'proviso-gui-on-timer future cell))
    (proviso-gui--draw-cell buffer marker create)))

(defun proviso-gui--draw-cell (buffer marker fun)
  "In BUFFER, recreate content at MARKER with FUN."
  (with-current-buffer buffer
    (let ((inhibit-read-only t)
          (pos (marker-position marker)))
      (goto-char pos)
      (delete-region pos (line-end-position))
      (insert (funcall fun))
      (goto-char pos))))

(defun proviso-gui-add-to-buffer (buffer lst &optional maxwidth)
  "Add GUI elements TO BUFFER based on LST.
Returns a sorted list of markers in the buffer.
MAXWIDTH allows specifying the minimum length of the headings."
  (let ((inhibit-read-only t)
        (max-heading (or maxwidth 0))
        pred heading id)
    (with-current-buffer buffer
      (setq lst (seq-remove (lambda (elt)
                              (setq pred (plist-get elt :predicate))
                              (and pred (functionp pred)
                                   (not (funcall pred))))
                            lst))
      (dolist (entry lst)
        (setq max-heading (max
                           (if (plist-get entry :heading)
                               (string-width (plist-get entry :heading))
                             0)
                           max-heading)))
      (dolist (entry lst)
        (setq heading (plist-get entry :heading))
        (setq pred (plist-get entry :content))
        (setq id (or (plist-get entry :id)
                     (proviso-gui-get-next-entry-id buffer)))
        (when (eq (plist-get entry :section) 'pre)
          (proviso-gui--insert-section-break))
        (setq heading
              (concat (s-pad-left max-heading " " heading)
                      (if heading ": " "  ")))
        (insert heading)
        (lexical-let ((map (make-sparse-keymap))
                      (marker (point-marker))
                      (create (plist-get entry :content))
                      cell)
          (set-marker-insertion-type marker nil)
          (setq cell (list
                      (cons 'pos marker)
                      (cons 'create create)
                      (cons 'buffer buffer)
                      (cons 'heading heading)))
          (dolist (binding (plist-get entry :bindings))
            (lexical-let ((cb (cdr binding)))
              (define-key map (car binding)
                (lambda() (interactive)
                  (proviso-gui-cb cb cell)))))
          (push (cons 'map map) cell)
          (set-keymap-parent map proviso-gui--local-map)
          (add-to-list 'proviso-gui-markers cell t)
          (insert (funcall pred) "\n"))
        (when (eq (plist-get entry :section) 'post)
          (proviso-gui--insert-section-break))
        )
      )
    max-heading))

(defun proviso-gui--insert-section-break ()
  "Insert a section break into current buffer at point."
  (insert "\f\n"))

(defun proviso-gui--sort-markers (buffer)
  "Sort the markers used in BUFFER."
  (setq proviso-gui-markers
        (sort proviso-gui-markers (lambda (lhs rhs)
                                    (< (marker-position
                                        (cdr (assq 'pos lhs)))
                                       (marker-position
                                        (cdr (assq 'pos rhs))))))))

(defun proviso-gui-finalize-buffer (buffer)
  "Finalize the GUI settings of BUFFER."
  (with-current-buffer buffer
    (use-local-map proviso-gui--local-map)
    (proviso-gui-on-line)))

(provide 'proviso-gui)
;;; proviso-gui.el ends here
