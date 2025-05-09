;;; proviso-ag.el --- Support ag for proviso
;; Copyright (C) 2017-2021, 2025  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Thursday, November  2, 2017
;; Version: 1.0
;; Modified Time-stamp: <2025-04-19 12:15:23 dharms>
;; Modified by: Dan Harms
;; Keywords: tools unix proviso project ag silver searcher
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
;; Provides utilities enabling searching with ag.
;;

;;; Code:
(require 'proviso-core)
(require 'proviso-defines)
(require 'proviso-regexp)
(require 'grep)
(require 'subr-x)
(require 's)

(defcustom proviso-ag-args "--hidden --nogroup --null -fi"
  "Standard arguments to give to ag."
  :group 'proviso-custom-group)

(defun proviso-ag--get-args ()
  "Get the search parameters."
  (if proviso-search-ask-whole-word
      (if (y-or-n-p "Search whole word occurrences?")
          (concat proviso-ag-args "w")
        proviso-ag-args)
    (if proviso-search-whole-word
        (concat proviso-ag-args "w")
      proviso-ag-args)))

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

(defun proviso-ag--create-ag-str (proj &optional exclude-files
                                           exclude-dirs
                                           include-files)
  "Create an ag command string according to the settings of PROJ.
Optionally, EXCLUDE-FILES, EXCLUDE-DIRS and INCLUDE-FILES can be
overridden; otherwise they are taken from project settings, or
default values from `proviso-uninteresting-dirs',
`proviso-uninteresting-files' and `proviso-interesting-files'.
To override an empty value, use the synbol 'none to distinguish
from unspecified."
  (let (has-exclude-dirs-p has-exclude-files-p has-include-files-p)
    (setq exclude-files
          (if (eq exclude-files 'none) nil
            (or exclude-files
                (proviso-get proj :grep-exclude-files)
                proviso-uninteresting-files)))
    (setq exclude-dirs
          (if (eq exclude-dirs 'none) nil
            (or exclude-dirs
                (proviso-get proj :grep-exclude-dirs)
                proviso-uninteresting-dirs)))
    (setq include-files
          (if (eq include-files 'none) nil
            (or include-files
                (proviso-get proj :grep-include-files)
                proviso-interesting-files)))
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
           (cmd "")
           (prompt "Search root: ")
           (search-string (if (region-active-p)
                              (buffer-substring (region-beginning) (region-end))
                            (thing-at-point 'symbol)))
           (remote (file-remote-p default-directory))
           first dir substr idx exclude-files exclude-dirs include-files)
      (setq first (if (consp (car dirs)) (cdr (car dirs)) (car dirs)))
      (setq dir (cond ((and arg (>= (prefix-numeric-value arg) 16))
                       (read-directory-name prompt default-directory nil t))
                      ((and arg (= (prefix-numeric-value arg) 4) dirs)
                       (completing-read prompt dirs nil nil nil nil (last dirs)))
                      ((or (null dirs) (null first) (string-empty-p first))
                       (or (proviso-current-project-root) default-directory))
                      (t first)))
      (setq dir
            (if remote
                ;; remove remote prefix
                (replace-regexp-in-string (regexp-quote remote) "" dir)
              (expand-file-name dir)))  ;not sure if this is needed for ag
      (when (and arg (>= (prefix-numeric-value arg) 64))
        (setq exclude-files
              (split-string
               (read-string
                "Exclude files: "
                (mapconcat 'identity
                           (or
                            (proviso-get proj :grep-exclude-files)
                            proviso-uninteresting-files)
                           " "))))
        (unless exclude-files
          (setq exclude-files 'none))
        (setq exclude-dirs
              (split-string
               (read-string
                "Exclude dirs: "
                (mapconcat 'identity
                           (or
                            (proviso-get proj :grep-exclude-dirs)
                            proviso-uninteresting-dirs)
                           " "))))
        (unless exclude-dirs
          (setq exclude-dirs 'none))
        (setq include-files
              (split-string
               (read-string
                "Include files: "
                (mapconcat 'identity
                           (or
                            (proviso-get proj :grep-include-files)
                            proviso-interesting-files)
                           " "))))
        (unless include-files
          (setq include-files 'none)))
      (setq cmd (proviso-ag--create-ag-str proj exclude-files
                                           exclude-dirs include-files))
      (setq substr (concat "ag" cmd " " (proviso-ag--get-args) " "))
      (setq idx (string-width substr))
      (cons
       (concat
        substr
        (when search-string
          (progn
            (setq search-string
                  (proviso-ag--sanitize-search-string search-string))
            (setq idx (+ idx (string-width search-string)))
            search-string))
        " "
        (directory-file-name dir))
       (1+ idx))))

(defun proviso-ag-create-search-cmd (proj str args &optional
                                          exclude-files
                                          exclude-dirs
                                          include-files)
  "Return a command line to search for STR with args ARGS in project PROJ."
  (let ((dir (proviso-get proj :root-dir))
        (remote (proviso-get proj :remote-prefix))
        (cmd (proviso-ag--create-ag-str proj exclude-files
                                        exclude-dirs include-files)))
    (setq dir (if remote
                  (replace-regexp-in-string (regexp-quote remote) "" dir)
                (expand-file-name dir)))
    (concat "ag" cmd " " args " "
            (proviso-ag--sanitize-search-string str) " "
            (directory-file-name dir))))

(defun proviso-ag--sanitize-search-string (search-string)
  "Sanitize SEARCH-STRING."
  (shell-quote-argument search-string))

;;;###autoload
(defun proviso-ag (&optional arg)
  "Search for a string in a directory or project.
Uses ag, the silver searcher.  ARG allows customizing the
root search directory."
  (interactive "P")
  ;; HACK around a bug in 'grep-compute-defaults when accessing
  ;; grep-host-defaults-alist for a different host
  (unless (assq (intern (or (file-remote-p default-directory) "localhost"))
                grep-host-defaults-alist)
    (grep-compute-defaults))
  (grep-apply-setting 'grep-command (proviso-ag--create-command arg))
  (grep-apply-setting 'grep-use-null-device nil)
  (command-execute 'grep))

(provide 'proviso-ag)
;;; proviso-ag.el ends here
