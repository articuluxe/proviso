;;; proviso-ag.el --- ag support for proviso
;; Copyright (C) 2017  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Thursday, November  2, 2017
;; Version: 1.0
;; Modified Time-stamp: <2017-11-10 08:08:34 dharms>
;; Modified by: Dan Harms
;; Keywords: tools unix proviso project ag silver searcher
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
;; Provides utilities enabling searching with ag.
;;

;;; Code:
(require 'proviso-core)
(require 'proviso-defines)
(require 'proviso-regexp)
(require 'grep)
(require 'subr-x)
(require 's)

(defun proviso-ag--create-file-exclusion-str (lst)
  "Create an ag subcommand to exclude files from LST."
  (mapconcat (lambda (elt)
               (concat " --ignore " elt))
             lst ""))

(defun proviso-ag--create-dir-exclusion-str (lst)
  "Create an ag subcommand to exclude dirs from LST."
  (mapconcat (lambda (elt)
               (concat " --ignore " elt))
             lst ""))

(defun proviso-ag--create-inclusion-str (lst)
  "Create an ag subcommand to match files from LST."
  (format " -G '(%s)'"
          (mapconcat (lambda (elt)
                       ;; convert shell glob patterns to regex;
                       ;; we constrain the regexes from having an
                       ;; opening anchor "^" because ag will
                       ;; try to match the entire path name.
                       (proviso-regexp-glob-to-regex elt t))
                     lst "|")))

(defun proviso-ag--create-ag-str (proj)
  "Create an ag command string according to the settings of PROJ."
  (let ((exclude-files (or (proviso-get proj :grep-exclude-files)
                           proviso-uninteresting-files))
        (exclude-dirs (or (proviso-get proj :grep-exclude-dirs)
                          proviso-uninteresting-dirs))
        (include-files (or (proviso-get proj :grep-include-files)
                           proviso-interesting-files))
        has-exclude-dirs-p has-exclude-files-p has-include-files-p)
    (setq has-exclude-files-p (not (seq-empty-p exclude-files)))
    (setq has-exclude-dirs-p (not (seq-empty-p exclude-dirs)))
    (setq has-include-files-p (not (seq-empty-p include-files)))
    (concat
     (when (or has-exclude-files-p has-exclude-dirs-p)
       (concat
        (proviso-ag--create-file-exclusion-str exclude-files)
        (proviso-ag--create-dir-exclusion-str exclude-dirs)))
     (when has-include-files-p
       (proviso-ag--create-inclusion-str include-files))
     )))

(defun proviso-ag--create-command (&optional arg)
    "Create a command suitable for ag to search for a string.
ARG allows customizing the selection of the root search directory."
    (let* ((proj (proviso-current-project))
           (root (proviso-get proj :root-dir))
           (dirs (proviso-get proj :grep-dirs))
           (cmd (or (proviso-get proj :ag-cmd) ""))
           (prompt "Search root: ")
           (search-string (if (region-active-p)
                              (buffer-substring (region-beginning) (region-end))
                            (thing-at-point 'symbol)))
           (remote (file-remote-p default-directory))
           first dir substr idx)
      (setq first (if (consp (car dirs)) (cdr (car dirs)) (car dirs)))
      (setq dir (cond ((and arg (= (prefix-numeric-value arg) 16))
                       (read-directory-name prompt default-directory nil t))
                      ((and arg (= (prefix-numeric-value arg) 4) dirs)
                       (completing-read prompt dirs))
                      ((or (null dirs) (null first) (string-empty-p first))
                       (or (proviso-current-project-root) default-directory))
                      (t first)))
      (setq dir
            (if remote
                ;; remove remote prefix
                (replace-regexp-in-string (regexp-quote remote) "" dir)
              (expand-file-name dir)))  ;not sure if this is needed for ag
      (when (string-empty-p cmd)
        (setq cmd (proviso-ag--create-ag-str proj)))
      (when (and proj (not (proviso-get proj :ag-cmd)))
        (proviso-put proj :ag-cmd cmd))
      (setq substr (concat "ag" cmd " --hidden --nogroup -fiQ "))
      (setq idx (string-width substr))
      (cons
       (concat
        substr
        (when search-string
          (progn
            (setq search-string (shell-quote-argument search-string))
            (setq idx (+ idx (string-width search-string)))
            search-string))
        " "
        (directory-file-name dir))
       (1+ idx))))

;;;###autoload
(defun proviso-ag (&optional arg)
  "Search for a string in a directory or project.
Uses ag, the silver searcher.  ARG allows customizing the
root search directory."
  (interactive "P")
  (grep-apply-setting 'grep-command (proviso-ag--create-command arg))
  (grep-apply-setting 'grep-use-null-device nil)
  (command-execute 'grep))

(provide 'proviso-ag)
;;; proviso-ag.el ends here
