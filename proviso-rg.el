;;; proviso-rg.el --- Support rg for proviso
;; Copyright (C) 2018-2021  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Tuesday, January 23, 2018
;; Version: 1.0
;; Modified Time-stamp: <2021-04-20 14:02:08 dharms>
;; Modified by: Dan Harms
;; Keywords: tools unix proviso project rg ripgrep
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
;; Provides utilities enabling searching with rg (ripgrep).
;;

;;; Code:
(require 'proviso-core)
(require 'proviso-defines)
(require 'proviso-regexp)
(require 'grep)
(require 'subr-x)
(require 's)

(defcustom proviso-rg-args "--no-heading --null -Snuu"
  "Standard arguments to give to rg."
  :group 'proviso-custom-group)

(defun proviso-rg--compute-quote-char (remote)
  "Return the quote character, with remote path REMOTE.
Basically rg in a dos shell barfs on surrounding quotes.  Normal,
sane shells do need the quotes to avoid premature expansion.  But
even on windows, if we're connecting via tramp, we still need the
quotes."
  (if (and (string-match-p "cmdproxy" shell-file-name)
           (not remote))
      ;; works unless we remote into windows
      ""
    "'"))

(defun proviso-rg--create-file-exclusion-str (lst quote)
  "Create an rg subcommand to exclude files from LST, surrounded by QUOTE."
  (let (str)
    (dolist (elt lst str)
      (setq str (concat str "-g " quote "!" elt quote " ")))))

(defun proviso-rg--create-dir-exclusion-str (lst quote)
  "Create an rg subcommand to exclude dirs from LST, surrounded by QUOTE."
  (let (str)
    (dolist (elt lst str)
      (setq str (concat str "-g " quote "!" elt quote " ")))))

(defun proviso-rg--create-inclusion-str (lst quote)
  "Create an rg subcommand to match files from LST, surrounded by QUOTE."
  (let (str)
    (dolist (elt lst str)
      (setq str (concat str "-g " quote elt quote " ")))))

(defun proviso-rg--create-rg-str (proj quote &optional
                                       exclude-files
                                       exclude-dirs
                                       include-files)
  "Create an rg command string according to the settings of PROJ.
The command will be delimited by QUOTE.  Optionally,
EXCLUDE-FILES, EXCLUDE-DIRS and INCLUDE-FILES can be overridden;
otherwise they are taken from project settings, or default values
from `proviso-uninteresting-dirs', `proviso-uninteresting-files'
and `proviso-interesting-files'.  To override an empty value,
use the synbol 'none to distinguish from unspecified."
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
        (proviso-rg--create-file-exclusion-str exclude-files quote)
        (proviso-rg--create-dir-exclusion-str exclude-dirs quote)))
     (when has-include-files-p
       (proviso-rg--create-inclusion-str include-files quote))
     )))

(defun proviso-rg--create-command (&optional arg)
    "Create a command suitable for rg to search for a string.
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
              (expand-file-name dir)))  ;not sure if this is needed for rg
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
      (setq cmd (proviso-rg--create-rg-str
                 proj
                 (proviso-rg--compute-quote-char remote)
                 exclude-files exclude-dirs include-files))
      (setq substr (concat "rg " cmd proviso-rg-args " "))
      (setq idx (string-width substr))
      (cons
       (concat
        substr
        (when search-string
          (progn
            (setq search-string (proviso-rg--sanitize-search-string search-string))
            (setq idx (+ idx (string-width search-string)))
            search-string))
        " "
        (directory-file-name dir))
       (1+ idx))))

(defun proviso-rg-create-search-cmd (proj str args &optional
                                          exclude-files
                                          exclude-dirs
                                          include-files)
  "Return a command line to search for STR with args ARGS in project PROJ."
  (let* ((dir (proviso-get proj :root-dir))
         (remote (proviso-get proj :remote-prefix))
         (cmd (proviso-rg--create-rg-str
               proj
               (proviso-rg--compute-quote-char remote)
               exclude-files exclude-dirs include-files)))
    (setq dir (if remote
                  (replace-regexp-in-string (regexp-quote remote) "" dir)
                (expand-file-name dir)))
    (concat "rg " cmd args " "
            (proviso-rg--sanitize-search-string str) " "
            (directory-file-name dir))))

(defun proviso-rg--sanitize-search-string (search-string)
  "Sanitize SEARCH-STRING."
  (shell-quote-argument search-string))

;;;###autoload
(defun proviso-rg (&optional arg)
  "Search for a string in a directory or project.
Uses rg, ripgrep.  ARG allows customizing the root search
directory."
  (interactive "P")
  ;; HACK around a bug in 'grep-compute-defaults when accessing
  ;; grep-host-defaults-alist for a different host
  (unless (assq (intern (or (file-remote-p default-directory) "localhost"))
                grep-host-defaults-alist)
    (grep-compute-defaults))
  (grep-apply-setting 'grep-command (proviso-rg--create-command arg))
  (grep-apply-setting 'grep-use-null-device nil)
  (command-execute 'grep))

(provide 'proviso-rg)
;;; proviso-rg.el ends here
