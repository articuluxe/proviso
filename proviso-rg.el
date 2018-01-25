;;; proviso-rg.el --- rg support for proviso
;; Copyright (C) 2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Tuesday, January 23, 2018
;; Version: 1.0
;; Modified Time-stamp: <2018-01-25 08:07:44 dharms>
;; Modified by: Dan Harms
;; Keywords: tools unix proviso project rg ripgrep
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
;; Provides utilities enabling searching with rg (ripgrep).
;;

;;; Code:
(require 'proviso-core)
(require 'proviso-defines)
(require 'proviso-regexp)
(require 'grep)
(require 'subr-x)
(require 's)

(defun proviso-rg--create-file-exclusion-str (lst)
  "Create an rg subcommand to exclude files from LST."
  (let (str)
    (dolist (elt lst str)
      (setq str (concat str "-g '!" elt "' ")))))

(defun proviso-rg--create-dir-exclusion-str (lst)
  "Create an rg subcommand to exclude dirs from LST."
  (let (str)
    (dolist (elt lst str)
      (setq str (concat str "-g '!" elt "' ")))))

(defun proviso-rg--create-inclusion-str (lst)
  "Create an rg subcommand to match files from LST."
  (let (str)
    (dolist (elt lst str)
      (setq str (concat str "-g '" elt "' ")))))

(defun proviso-rg--create-rg-str (proj)
  "Create an rg command string according to the settings of PROJ."
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
        (proviso-rg--create-file-exclusion-str exclude-files)
        (proviso-rg--create-dir-exclusion-str exclude-dirs)))
     (when has-include-files-p
       (proviso-rg--create-inclusion-str include-files))
     )))

(defun proviso-rg--create-command (&optional arg)
    "Create a command suitable for rg to search for a string.
ARG allows customizing the selection of the root search directory."
    (let* ((proj (proviso-current-project))
           (root (proviso-get proj :root-dir))
           (dirs (proviso-get proj :grep-dirs))
           (cmd (or (proviso-get proj :rg-cmd) ""))
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
              (expand-file-name dir)))  ;not sure if this is needed for rg
      (when (string-empty-p cmd)
        (setq cmd (proviso-rg--create-rg-str proj)))
      (when (and proj (not (proviso-get proj :rg-cmd)))
        (proviso-put proj :rg-cmd cmd))
      (setq substr (concat "rg " cmd "--no-heading -Suu "))
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
(defun proviso-rg (&optional arg)
  "Search for a string in a directory or project.
Uses rg, ripgrep.  ARG allows customizing the root search
directory."
  (interactive "P")
  (grep-apply-setting 'grep-command (proviso-rg--create-command arg))
  (grep-apply-setting 'grep-use-null-device nil)
  (command-execute 'grep))

(provide 'proviso-rg)
;;; proviso-rg.el ends here
