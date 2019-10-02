;;; proviso-search.el --- A custom search utility across projects.
;; Copyright (C) 2019  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Wednesday, September 25, 2019
;; Version: 1.0
;; Modified Time-stamp: <2019-10-03 21:03:23 dharms>
;; Modified by: Dan Harms
;; Keywords: tools unix proviso project grep
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
;; A utility to search by grep through multiple projects.
;;

;;; Code:
(require 'proviso-grep)

;;;###autoload
(defun proviso-search (&optional arg)
  "Search through multiple projects.
ARG allows customizing search parameters."
  (interactive "P")
  (let ((str (if (region-active-p)
                 (buffer-substring (region-beginning) (region-end))
               (thing-at-point 'symbol)))
        lst)
    (mapatoms (lambda (atom)
                (push atom lst))
              proviso-obarray)
    (proviso-search-projects str (nreverse lst))))

(defun proviso-search-projects (search-str projects)
  "Run a search for SEARCH-STR through projects PROJECTS."
  (let ((buffer (get-buffer-create " *proviso-search*"))
        (grep-re (first (car grep-regexp-alist)))
        (file-group (second (car grep-regexp-alist)))
        (line-group (third (car grep-regexp-alist)))
        command status hits matches)
    (setq search-str (read-string
                      "Search projects for: " search-str))
    ;; todo arguments
    (setq command (concat proviso-grep-args " "
                          (proviso-grep--sanitize-search-str search-str)))
    (with-current-buffer buffer
      (erase-buffer)
      (dolist (project projects)
        (setq status (proviso-search--project project command))
        )
      (goto-char (point-min))
      (while (re-search-forward grep-re nil t)
        (push (list (string-to-number (match-string line-group))
                    (match-string file-group)
                    (buffer-substring-no-properties (point) (line-end-position)))
              hits))
        ;; (message "drh grep found %s" hits)
      )
    (if (setq matches
              (xref--convert-hits (nreverse hits) search-str))
        (xref--show-xrefs matches nil t)
      (user-error "No results"))))

(defun proviso-search--project (proj substr)
  "Search for a substring SUBSTR in project PROJ."
  (let* ((stem (proviso-grep--create-search-cmd proj))
         (default-directory (proviso-get proj :root-dir))
         (command (concat stem substr)))
    (call-process-shell-command command nil t)))

(defun proviso-search--last (str)
  "Return the last identifier (possibly escaped) in STR."
  (let ((pos (1- (string-width str))))
    (while (not (or (eq (aref str pos) ?\s)
                    (eq pos 0)))
      (decf pos))
	(if (and (eq (aref str pos) ?\s)
			 (< pos (1- (string-width str))))
		(incf pos))
    (list (substring str pos))
    (substring str pos)))



;;   (interactive)
;;   (let* ((proviso-local-proj (car projects))
;;          (cmd (proviso-grep--create-command))
;;          (buffer (get-buffer-create "*proviso-search*"))
;;          (proc (get-buffer-process buffer))
;;          )
;;     (with-current-buffer buffer
;;       ;; TODO handle current proc
;;       ;; TODO set revert buffer func
;;       (erase-buffer)
;;       (setq buffer-read-only nil)
;;       (insert "-*- mode: grep-mode; default-directory: "
;;               (abbreviate-file-name default-directory)
;;               (format " -*-\ngrep started at %s\n\n"
;;                       (substring (current-time-string) 0 19))
;;               cmd "\n")
;;       (setq next-error-last-buffer buffer)
;;       (set-buffer-modified-p nil)
;;       ;; TODO pop up window
;;       (let* ((process-environment (append compilation-environment
;;                                           (list
;;                                            (format "INSIDE_EMACS=%s,compile"
;;                                                    emacs-version)
;;                                            "TERM=emacs-grep"
;;                                            "GREP_COLOR=01;31"
;;                                            "GREP_COLORS=mt=01;31:fn=:ln=:bn=:se=:sl=:cx=:ne"
;;                                            )
;;                                           (copy-sequence process-environment)))
;;              (proc (start-file-process-shell-command "search" buffer cmd)))
;;         (set-process-sentinel proc 'proviso-search-sentinel)
;;         (set-process-filter proc 'proviso-search-filter)
;;         ))))

;; (defun proviso-search-sentinel (proc msg)
;;   "Search process PROC has changed state according to MSG."
;;   (when (memq (process-status proc) '(exit signal))
;;     (let ((buffer (process-buffer proc)))
;;       (if (null (buffer-name buffer))
;;           (set-process-buffer proc nil)
;;         (with-current-buffer buffer
;;           t
;;           )
;;         (delete-process proc)))))

;; (defun proviso-search-filter (proc msg)
;;   "Filter method for search results from process PROC with status MSG.
;; Match highlighting escape sequences in grep results."
;;   (when (buffer-live-p (process-buffer proc))
;;     (with-current-buffer (process-buffer proc)
;;       (let ((start (marker-position (process-mark proc)))
;;             (inhibit-read-only t)
;;             beg end)
;;         (goto-char start)
;;         (insert msg)
;;         (set-marker (process-mark proc) (point))
;;         (save-excursion
;;           (forward-line 0)
;;           (setq end (point))
;;           (goto-char start)
;;           (setq beg (point))
;;           ;; todo

;;         )
;;       )
;;     )
;;   )

(provide 'proviso-search)
;;; proviso-search.el ends here
