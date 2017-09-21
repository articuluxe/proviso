;;; proviso-grep.el --- setup proviso grep
;; Copyright (C) 2017  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Saturday, April  1, 2017
;; Version: 1.0
;; Modified Time-stamp: <2017-09-20 17:34:30 dharms>
;; Modified by: Dan Harms
;; Keywords: unix proviso project grep
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
;; Provides utilities enabling a grep command.
;;

;;; Code:
(require 'proviso-core)
(require 'proviso-defines)
(require 'grep)
(require 'subr-x)
(require 's)

(defun proviso--set-grep-dirs (proj)
  "Set grep directories according to PROJ's project definition."
  (let ((root (proviso-get proj :root-dir))
        (lst (proviso-get proj :proj-alist))
        elt entry dirs)
    (dolist (element lst)
      (setq entry (plist-get element :dir))
      (setq elt (if (and entry (file-name-absolute-p entry))
                    entry
                  (concat root entry)))
      ;; ensure trailing slash
      (push (file-name-as-directory elt) dirs))
    (proviso-put proj :grep-dirs (delete-dups (append (nreverse dirs) `(,root))))
    ))

(add-hook 'proviso-hook-on-project-init 'proviso--set-grep-dirs)

(defun proviso-grep--create-file-exclusion-str (lst)
  "Create a grep subcommand to exclude files from LST."
  (mapconcat 'identity lst "\" -o -name \""))

(defun proviso-grep--create-dir-exclusion-str (lst)
  "Create a grep subcommand to exclude dirs from LST."
  (mapconcat 'identity lst "\" -o -path \""))

(defun proviso-grep--create-inclusion-str (lst)
  "Create a grep subcommand to match files from LST."
  (mapconcat 'identity lst "\" -o -name \""))

(defun proviso-grep--create-grep-str (proj)
  "Create a grep command string according to the settings of PROJ."
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
     ;; add files to exclude
     (if (or has-exclude-files-p has-exclude-dirs-p)
         (concat " \"(\" "
                 (when has-exclude-files-p
                   (concat "-name \""
                           (proviso-grep--create-file-exclusion-str exclude-files)
                           "\" "))
                 (when has-exclude-dirs-p
                   (concat
                    (when has-exclude-files-p "-o ")
                    "-path \""
                    (proviso-grep--create-dir-exclusion-str exclude-dirs)
                    "\" "))
                 "\")\" -prune -o ")
       " ")
     "-type f "
     ;; add files to include
     (when has-include-files-p
       (concat "\"(\" -name \""
               (proviso-grep--create-inclusion-str include-files)
               "\" \")\" "))
     "-print0 | xargs -0 grep -Isn ")))

(defun proviso-grep--create-command (&optional arg)
  "Create a command suitable for grep to search for a string.
ARG allows customizing the selection of the root search directory."
  (let* ((proj (proviso-current-project))
         (root (proviso-get proj :root-dir))
         (dirs (proviso-get proj :grep-dirs))
         (cmd (or (proviso-get proj :grep-cmd) ""))
         (prompt "Grep root: ")
         (search-string (if (region-active-p)
                            (buffer-substring (region-beginning) (region-end))
                          (thing-at-point 'symbol)))
         (remote (file-remote-p default-directory))
         first dir)
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
            ;; some variants of grep don't handle relative paths
            ;; (but expand-file-name doesn't work remotely)
            (expand-file-name dir)))
    (when (string-empty-p cmd)
      (setq cmd (proviso-grep--create-grep-str proj)))
    (when (and proj (not (proviso-get proj :grep-cmd)))
      (proviso-put proj :grep-cmd cmd))
    (concat "find -P "
            ;; some grep variants barf on trailing slashes
            (directory-file-name dir)
            cmd
            (when search-string
              (s-replace
               "\\*" "\\\\*"
               ;; shell-quote the search-string.  `shell-quote-argument'
               ;; escapes embedded `*' but grep needs them double-escaped.
               (shell-quote-argument search-string))))))

(defun proviso-grep-clear-command ()
  "Clear out the cached grep command (if any exists) in the current project."
  (interactive)
  (let (proj (proviso-current-project))
    (when proj (proviso-put proj :grep-cmd ""))))

;;;###autoload
(defun proviso-grep (&optional arg)
  "Grep for a search string in a directory or project.
ARG allows customizing the root search directory, see `proviso-grep--create-command'."
  (interactive "P")
  ;; HACK around a bug in 'grep-compute-defaults when accessing
  ;; grep-host-defaults-alist for a different host
  (unless (assq (intern (or (file-remote-p default-directory) "localhost"))
                grep-host-defaults-alist)
    (grep-compute-defaults))
  (grep-apply-setting 'grep-command (proviso-grep--create-command arg))
  (command-execute 'grep))

(provide 'proviso-grep)
;;; proviso-grep.el ends here
