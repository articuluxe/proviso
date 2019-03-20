;;; proviso-gui.el --- gui gramework for proviso
;; Copyright (C) 2018-2019  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Thursday, August 23, 2018
;; Version: 1.0
;; Modified Time-stamp: <2019-03-20 08:54:40 dharms>
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
(require 'cl-lib)
(require 's)
(require 'seq)
(require 'async)

(defvar-local proviso-gui-markers nil
  "Alist of markers in current buffer, for navigation.")

(defvar-local proviso-gui--local-map nil
  "Local proviso gui key map.")

(defvar-local proviso-gui--next-id 0
  "Counter for next entry id.")

(defvar-local proviso-gui--cursor nil
  "ID of currently selected line.")

(defvar-local proviso-gui--cursor-policy nil
  "An optional policy directing how to select the cursor.
Possible values:
 - new: will select a newly-created row
 - id : will maintain selection based on 'parent-id
 - nil: first row will be selected if buffer is redrawn

Otherwise the current row is usually maintained.")

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

(defun proviso-gui-on-timer (future &optional id)
  "Timer callback to check FUTURE, regarding GUI cell ID."
  (if (async-ready future)
      (let ((cell (proviso-gui-lookup-by-id id)))
        (when cell
          (proviso-gui--draw-cell cell)))
    (run-at-time 1 nil #'proviso-gui-on-timer future id)))

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
      (proviso-gui-select-line next))
    next))

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
      (proviso-gui-select-line prev))
    prev))

(defun proviso-gui-find-current-cell ()
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

(defun proviso-gui-select-line (&optional where)
  "Examine the current line, set the current keymap if necessary.
If WHERE is non-nil, it provides the current line."
  (let ((cell (or where
                  (proviso-gui-find-current-cell))))
    (when cell
      (use-local-map (cdr (assq 'map cell)))
      (setq proviso-gui--cursor (cdr (assq 'id cell))))
    cell))

(defun proviso-gui-init-buffer (buffer keymap)
  "Initialize BUFFER for gui operations, with keymap KEYMAP."
  (let ((inhibit-read-only t))
    (with-current-buffer buffer
      (unless (memq #'proviso-gui-on-buffer-kill kill-buffer-hook)
        (add-hook 'kill-buffer-hook #'proviso-gui-on-buffer-kill nil t))
      (setq proviso-gui-markers nil)
      (put 'proviso-gui--local-map 'permanent-local t)
      (put 'proviso-gui--cursor-policy 'permanent-local t)
      (put 'proviso-gui--cursor 'permanent-local t)
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
  (with-current-buffer buffer
    (incf proviso-gui--next-id)))

(defun proviso-gui-lookup-by-category (symbol)
  "Return a list of IDs of GUI elements corresponding to category SYMBOL."
  (let ((lst
         (mapcar (lambda (cell)
                   (if (eq symbol (cdr (assq 'category cell)))
                       (cdr (assq 'id cell))
                     nil))
                 proviso-gui-markers)))
    (remove nil (remove-duplicates lst))))

(defun proviso-gui-lookup-by-id (id)
  "Return the GUI element corresponding to ID."
  (let ((cell))
    (catch 'found
      (dolist (elt proviso-gui-markers)
        (when (eq id
                  (cdr (assq 'id elt)))
          (throw 'found elt)))
      nil)))

(defun proviso-gui-lookup-by-parent-id (id)
  "Return the GUI element corresponding to parent id ID."
  (let ((cell))
    (catch 'found
      (dolist (elt proviso-gui-markers)
        (when (eq id (cdr (assq 'parent-id elt)))
          (throw 'found elt)))
      nil)))

(defun proviso-gui-cb (cb &optional where)
  "Execute callback CB, for gui element corresponding to WHERE.
CELL can be a symbol representing a category, the special symbol 'buffer,
which will redraw the entire buffer after executing the callback, an alist of
properties representing a particular cell or row, or nil."
  (interactive)
  (let ((future (funcall cb)))
    (if (eq where 'buffer)
        (progn
          (proviso-gui-cb--id future nil)
          (funcall (key-binding "g"))
          (proviso-gui--goto-cursor))
      (let ((ids (cond ((listp where)
                        (list (cdr (assq 'id where))))
                       ((not where) nil)
                       ((symbolp where)
                        (proviso-gui-lookup-by-category where)))))
        (dolist (id ids)
          (proviso-gui-cb--id future id))))))

(defun proviso-gui-cb--id (future id)
  "Redraw GUI element corresponding to ID, according to FUTURE.
FUTURE may be nil, or a process sentinel to wait upon completion."
  (let ((cell (proviso-gui-lookup-by-id id)))
    (and future (processp future)
         (run-at-time 1 nil #'proviso-gui-on-timer future id))
    (when cell
      (proviso-gui--draw-cell cell))))

(defun proviso-gui--draw-cell (cell)
  "Recreate content of CELL."
  (let ((buffer (cdr (assq 'buffer cell)))
        (marker (cdr (assq 'pos cell)))
        (create (cdr (assq 'create cell))))
    (when (and buffer marker create)
      (proviso-gui--draw-cell-internal buffer marker create)
      (proviso-gui--goto-cursor))))

(defun proviso-gui--draw-cell-internal (buffer marker fun)
  "In BUFFER, recreate content at MARKER with FUN."
  (with-current-buffer buffer
    (let ((inhibit-read-only t)
          (pos (marker-position marker)))
      (save-excursion
        (goto-char pos)
        (delete-region pos (line-end-position))
        (insert (funcall fun))))))

(defun proviso-gui--goto-cursor ()
  "Put cursor on last-selected line."
  (let ((cell (proviso-gui-lookup-by-id proviso-gui--cursor)))
    (when cell
      (goto-char (marker-position (cdr (assq 'pos cell)))))
    cell))

(defun proviso-gui-add-global-cb (buffer bindings)
  "Add global callbacks in BUFFER for BINDINGS."
  (with-current-buffer buffer
    (dolist (binding bindings)
      (lexical-let ((cb (nth 1 binding))
                    (category (nth 2 binding))
                    (policy (nth 3 binding)))
        (define-key proviso-gui--local-map
          (nth 0 binding)
          (lambda() (interactive)
            (if (eq policy 'id)
                (if-let* ((cell (proviso-gui-lookup-by-id proviso-gui--cursor))
                          (id (cdr (assq 'parent-id cell))))
                    (setq proviso-gui--cursor-policy id))
              (setq proviso-gui--cursor-policy policy))
            (proviso-gui-cb cb category)))))))

(defun proviso-gui-add-to-buffer (buffer lst &optional maxwidth)
  "Add GUI elements TO BUFFER based on LST.
Returns a sorted list of markers in the buffer.
MAXWIDTH allows specifying the minimum length of the headings."
  (let ((inhibit-read-only t)
        (max-heading (or maxwidth 0))
        heading category pred id parent-id)
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
        (setq category (plist-get entry :category))
        (setq pred (plist-get entry :content))
        (setq id (or (plist-get entry :id)
                     (proviso-gui-get-next-entry-id buffer)))
        (setq parent-id (plist-get entry :parent-id))
        (when (eq (plist-get entry :section) 'pre)
          (proviso-gui--insert-section-break))
        (setq heading
              (concat (s-pad-left max-heading " " heading)
                      (if heading ": " "  ")))
        (insert heading)
        (lexical-let ((map (make-sparse-keymap))
                      (marker (point-marker))
                      (create (plist-get entry :content))
                      (category category)
                      (id id)
                      cell)
          (set-marker-insertion-type marker nil)
          (setq cell (list
                      (cons 'id id)
                      (cons 'heading heading)
                      (cons 'pos marker)
                      (cons 'create create)
                      (cons 'buffer buffer)
                      (cons 'parent-id parent-id)
                      ))
          (when category (push (cons 'category category) cell))
          (and (eq proviso-gui--cursor-policy 'new)
               (or (not proviso-gui--cursor)
                   (> id proviso-gui--cursor))
               (setq proviso-gui--cursor id))
          (dolist (binding (plist-get entry :bindings))
            (lexical-let ((cb (nth 1 binding)))
              (define-key map (nth 0 binding)
                (lambda() (interactive)
                  (proviso-gui-cb cb (or category id))))))
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
    (let ((cell (proviso-gui--goto-cursor)))
      (if cell
          (progn
            (if-let ((integerp proviso-gui--cursor-policy)
                     (cell2 (proviso-gui-lookup-by-parent-id
                             proviso-gui--cursor-policy))
                     (id (cdr (assq 'id cell2))))
                (progn
                  (setq proviso-gui--cursor id)
                  (setq cell (proviso-gui--goto-cursor))
                  (proviso-gui-select-line))
              (proviso-gui-select-line))
            (setq proviso-gui--cursor-policy nil))
        (goto-char (point-min))
        (proviso-gui-select-line)
        (proviso-gui--goto-cursor)))))

(provide 'proviso-gui)
;;; proviso-gui.el ends here
