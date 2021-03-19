;;; proviso-search.el --- A custom search utility across projects
;; Copyright (C) 2019-2021  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Wednesday, September 25, 2019
;; Version: 1.0
;; Modified Time-stamp: <2021-03-19 16:40:22 dharms>
;; Modified by: Dan Harms
;; Keywords: tools unix proviso project grep
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
;; A utility to search by grep through multiple projects.
;;

;;; Code:
(require 'proviso-grep)
(require 'proviso-ag)
(require 'proviso-rg)
(require 'xref)

;;;###autoload
(defun proviso-grep-all (&optional arg)
  "Search through multiple projects using grep.
ARG allows customizing search parameters."
  (interactive "P")
  (proviso-search--driver
   arg
   #'proviso-grep-create-search-cmd
   proviso-grep-args
   "grep"))

;;;###autoload
(defun proviso-ag-all (&optional arg)
  "Search through multiple projects using the silver searcher.
ARG allows customizing search parameters."
  (interactive "P")
  (proviso-search--driver
   arg
   #'proviso-ag-create-search-cmd
   proviso-ag-args
   "ag"))

;;;###autoload
(defun proviso-rg-all (&optional arg)
  "Search through multiple projects using ripgrep.
ARG allows customizing search parameters."
  (interactive "P")
  (proviso-search--driver
   arg
   #'proviso-rg-create-search-cmd
   proviso-rg-args
   "rg"))

(defun proviso-search--driver (arg command args name)
  "Search through all projects using COMMAND with args ARGS.
ARG allows customizing the search characteristics.
NAME is a descriptive term for the search driver."
  (let ((str (if (region-active-p)
                 (buffer-substring (region-beginning) (region-end))
               (thing-at-point 'symbol)))
        lst exclude-files exclude-dirs include-files)
    (setq str (read-string
               (format "Search projects (using %s) for: " name)
               str))
    (when arg
      (setq exclude-files
            (split-string
             (read-string "Exclude files: "
                          (mapconcat 'identity
                                     proviso-uninteresting-files " "))))
      (setq exclude-dirs
            (split-string
             (read-string "Exclude dirs: "
                          (mapconcat 'identity
                                     proviso-uninteresting-dirs " "))))
      (setq include-files
            (split-string
             (read-string "Include files: "
                          (mapconcat 'identity
                                     proviso-interesting-files " "))))
      (setq args (read-string
                  (format "%s arguments: " name)
                  args)))
    (if (and str (not (string-empty-p str)))
        (progn
          (mapatoms (lambda (atom) (push atom lst)) proviso-obarray)
          (proviso-search-projects (nreverse lst)
                                   command
                                   str
                                   args
                                   exclude-files
                                   exclude-dirs
                                   include-files))
      (user-error "No search string"))))

(defun proviso-search-projects (projects func search-str args
                                         &optional exclude-files
                                         exclude-dirs include-files)
  "Run a search through PROJECTS (a list of projects) for search-str.
FUNC is a method to call to create the final command line.  It
has the signature (PROJ SEARCH-STR ARGS), where PROJ is the
project, STR is the search string, and ARGS are the desired
command line switches.  Optional arguments EXCLUDE-FILES,
EXCLUDE-DIRS and INCLUDE-FILES allow customizing the file set to
search."
  (let ((buffer (get-buffer-create " *proviso-search*"))
        (grep-re (first (car grep-regexp-alist)))
        (file-group (second (car grep-regexp-alist)))
        (line-group (third (car grep-regexp-alist)))
        status hits matches)
    (with-current-buffer buffer
      (erase-buffer)
      (dolist (project projects)
        (setq status
              (proviso-search--project project func search-str
                                       args exclude-files
                                       exclude-dirs include-files)))
      (goto-char (point-min))
      (while (re-search-forward grep-re nil t)
        (push (list (string-to-number (match-string line-group))
                    (match-string file-group)
                    (buffer-substring-no-properties (point) (line-end-position)))
              hits)))
    (if (setq matches
              (xref--convert-hits (nreverse hits) search-str))
        (if (< emacs-major-version 27)
            (xref--show-xrefs matches nil t)
          (xref--show-xrefs `(lambda () ',matches) nil))
      (user-error "No results"))))

(defun proviso-search--project (proj func search-str args
                                     &optional exclude-files
                                     exclude-dirs include-files)
  "Search for string SEARCH-STR in project PROJ using FUNC with ARGS.
FUNC is a method to call to create the final command line, of the
signature (PROJ SEARCH-STR ARGS).  Optional arguments
EXCLUDE-FILES, EXCLUDE-DIRS and INCLUDE-FILES allow customizing
the file set to search."
  (let ((command (funcall func proj search-str args exclude-files
                          exclude-dirs include-files))
        (default-directory (concat
                            (proviso-get proj :remote-prefix)
                            (proviso-get proj :root-dir))))
    (process-file-shell-command command nil t)))

(provide 'proviso-search)
;;; proviso-search.el ends here
