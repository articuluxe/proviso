;;; proviso-grep.el --- setup proviso grep
;; Copyright (C) 2017  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Saturday, April  1, 2017
;; Version: 1.0
;; Modified Time-stamp: <2017-06-02 07:48:43 dharms>
;; Modified by: Dan Harms
;; Keywords: proviso project grep

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

;;

;;; Code:
(require 'proviso-core)
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

(defvar proviso-extensions '(".cpp" ".cc" ".cxx" ".c" ".C"
                             ".hpp" ".hh" ".hxx" ".h" ".H"
                             ".in" ".ac" ".cmake"
                             ".xml" ".json" ".sql"
                             ".el" ".py" ".sh" ".cs" ".java"
                             ".proto" ".dart"
                             )
  "List of interesting file extensions.")

(defvar proviso-grep--extension-str "" "Sub-list cache of interesting extensions.")

(defun proviso-grep--create-extensions-str ()
  "Create a grep command string."
  (concat " \"(\" -name \"*moc_*\" -o -name \"*qrc_*\" \")\" "
          "-prune -o -type f \"(\" -name \"*"
          (mapconcat 'identity proviso-extensions "\" -o -name \"*")
          "\" \")\" -print0 | xargs -0 grep -Isn "))

(defun proviso-grep--create-command (&optional arg)
  "Create a command suitable for grep to search for a string.
ARG allows customizing the selection of the root search directory."
  (let ((root (proviso-get proviso-curr-proj :root-dir))
        (dirs (proviso-get proviso-curr-proj :grep-dirs))
        (prompt "Grep root: ")
        (search-string (if (region-active-p)
                           (buffer-substring (region-beginning) (region-end))
                         (thing-at-point 'symbol)))
        (remote (file-remote-p default-directory))
        first dir)
    (setq first (if (consp (car dirs)) (cdr (car dirs)) (car dirs)))
    (setq dir (cond ((and arg (= (prefix-numeric-value arg) 16))
                     (read-directory-name prompt (proviso-current-project-root) nil t))
                    ((and arg (= (prefix-numeric-value arg) 4) dirs)
                     (completing-read prompt dirs))
                    ((or (null dirs) (null first) (string-empty-p first))
                     (proviso-current-project-root))
                    (t first)))
    (setq dir
          (if remote
              ;; remove remote prefix
              (replace-regexp-in-string (regexp-quote remote) "" dir)
            ;; some variants of grep don't handle relative paths
            ;; (but expand-file-name doesn't work remotely)
            (expand-file-name dir)))
    (when (string-empty-p proviso-grep--extension-str)
      (setq proviso-grep--extension-str (proviso-grep--create-extensions-str)))
    (concat "find -P "
            ;; some grep variants barf on trailing slashes
            (directory-file-name dir)
            proviso-grep--extension-str
            (when search-string
              (s-replace
               "\\*" "\\\\*"
               ;; shell-quote the search-string.  `shell-quote-argument'
               ;; escapes embedded `*' but grep needs them double-escaped.
               (shell-quote-argument search-string))))))

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
