;;; proviso-grep.el --- setup proviso grep
;; Copyright (C) 2017  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Saturday, April  1, 2017
;; Version: 1.0
;; Modified Time-stamp: <2017-05-03 09:08:35 dharms>
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
(require 'ivy)
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
    (proviso-put proj :grep-dirs (delete-dups (append dirs `(,root))))
    ))

(add-hook 'proviso-hook-on-project-init 'proviso--set-grep-dirs)

(defun proviso-grep-create-command (&optional arg)
  "Create a command suitable for grep to search for a string."
  (let ((root (proviso-get proviso-curr-proj :root-dir))
        (dirs (proviso-get proviso-curr-proj :grep-dirs))
        (prompt "Grep root: ")
        (search-string (if (region-active-p)
                           (buffer-substring (region-beginning) (region-end))
                         (thing-at-point 'symbol)))
        (remote (file-remote-p default-directory))
        first dir)
    (setq first (if (consp (car dirs)) (cdr (car dirs)) (car dirs)))
    (setq dir (cond ((or (null root) (null dirs) (= arg 64))
                     (read-directory-name prompt nil nil t))
                    ((= arg 16) ".")
                    ((= arg 4) (ivy-read prompt dirs))
                    (t first)))
    (setq dir
          (if remote
              ;; remove remote prefix
              (replace-regexp-in-string (regexp-quote remote) "" dir)
            ;; some variants of grep don't handle relative paths
            ;; (but expand-file-name doesn't work remotely)
            (expand-file-name dir)))
    (concat "find -P "
            (directory-file-name dir)   ;some greps dislike trailing slashes
            " \"(\" -name \"*moc_*\" -o -name \"*qrc_*\" \")\" "
            "-prune -o -type f \"(\" -name \"*.cpp\" -o -name \"*.h\" "
            "-o -name \"*.cc\" -o -name \"*.hh\" -o -name \"*.cxx\" "
            "-o -name \"*.hxx\" -o -name \"*.h\" -o -name \"*.c\" "
            "-o -name \"*.H\" -o -name \"*.C\" -o -name \"*.hpp\" "
            "-o -name \"*.in\" -o -name \"*.ac\" -o -name \"*.el\" "
            "-o -name \"*.sql\" -o -name \"*.py\" -o -name \"*.proto\" "
            "-o -name \"*.sh\" -o -name \"*.cs\" -o -name \"*.dart\" "
            "-o -name \"*.xml\" -o -name \"*.json\" "
            "\")\" -print0 | xargs -0 grep -Isn "
            (when search-string
              (s-replace
               "\\*" "\\\\*"
               ;; shell-quote the search-string.  `shell-quote-argument'
               ;; escapes embedded `*' but grep needs them double-escaped.
               (shell-quote-argument search-string))))))

(defun proviso-grep (&optional arg)
  "Grep for a search string in a directory or project."
  (interactive)
  (grep-apply-setting 'grep-command (proviso-grep-create-command arg))
  (command-execute 'grep))

(provide 'proviso-grep)
;;; proviso-grep.el ends here
