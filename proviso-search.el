;;; proviso-search.el --- A custom search utility across projects.
;; Copyright (C) 2019  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Wednesday, September 25, 2019
;; Version: 1.0
;; Modified Time-stamp: <2019-10-09 08:56:11 dharms>
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
(require 'proviso-ag)
(require 'xref)

;;;###autoload
(defun proviso-search-grep (&optional arg)
  "Search through multiple projects using grep.
ARG allows customizing search parameters."
  (interactive "P")
  (let ((str (if (region-active-p)
                 (buffer-substring (region-beginning) (region-end))
               (thing-at-point 'symbol)))
        lst)
    (setq str (read-string "Search projects for: " str))
    ;; todo arguments
    (mapatoms (lambda (atom)
                (push atom lst))
              proviso-obarray)
    (proviso-search-projects (nreverse lst)
                             (list #'proviso-grep-create-search-cmd
                                   str
                                   proviso-grep-args))))

;;;###autoload
(defun proviso-search-ag (&optional arg)
  "Search through multiple projects using the silver searcher.
ARG allows customizing search parameters."
  (interactive "P")
  (let ((str (if (region-active-p)
                 (buffer-substring (region-beginning) (region-end))
               (thing-at-point 'symbol)))
        lst)
    (setq str (read-string "Search projects for: " str))
    ;; todo arguments
    (mapatoms (lambda (atom)
                (push atom lst))
              proviso-obarray)
    (proviso-search-projects (nreverse lst)
                             (list #'proviso-ag-create-search-cmd
                                   str
                                   proviso-ag-args))))

;;;###autoload
(defun proviso-search-rg (&optional arg)
  "Search through multiple projects using ripgrep.
ARG allows customizing search parameters."
  (interactive "P")
  (let ((str (if (region-active-p)
                 (buffer-substring (region-beginning) (region-end))
               (thing-at-point 'symbol)))
        lst)
    (setq str (read-string "Search projects for: " str))
    ;; todo arguments
    (mapatoms (lambda (atom)
                (push atom lst))
              proviso-obarray)
    (proviso-search-projects (nreverse lst)
                             (list #'proviso-rg-create-search-cmd
                                   str
                                   proviso-rg-args))))

(defun proviso-search-projects (projects spec)
  "Run a search through PROJECTS (a list of projects) for SPEC.
SPEC is a list (FORM STR ARGS), where FORM is a function called
.
"
  (let ((buffer (get-buffer-create " *proviso-search*"))
        (grep-re (first (car grep-regexp-alist)))
        (file-group (second (car grep-regexp-alist)))
        (line-group (third (car grep-regexp-alist)))
        status hits matches)
    (with-current-buffer buffer
      (erase-buffer)
      (dolist (project projects)
        (setq status (proviso-search--project project spec)))
      (goto-char (point-min))
      (while (re-search-forward grep-re nil t)
        (push (list (string-to-number (match-string line-group))
                    (match-string file-group)
                    (buffer-substring-no-properties (point) (line-end-position)))
              hits)))
    (if (setq matches
              (xref--convert-hits (nreverse hits) (nth 1 spec)))
        (xref--show-xrefs matches nil t)
      (user-error "No results"))))

(defun proviso-search--project (proj spec)
  "Search for a string in project PROJ according to SPEC.
SPEC is a list (FORM STR ARGS), with STR the search string and
ARGS the desired command line switches.  FORM is a method to call
to create the final command line, of the signature (PROJ STR
ARGS), where PROJ is the project, STR is the search string, and
ARGS are the desired command line switches."
  (let ((command (funcall (nth 0 spec) proj (nth 1 spec) (nth 2 spec)))
         (default-directory (proviso-get proj :root-dir)))
    (call-process-shell-command command nil t)))

(provide 'proviso-search)
;;; proviso-search.el ends here
