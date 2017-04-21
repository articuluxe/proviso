;;; proviso-sml.el --- add support to profiles for smart-mode-line
;; Copyright (C) 2017  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Friday, January  6, 2017
;; Version: 1.0
;; Modified Time-stamp: <2017-04-21 08:20:01 dharms>
;; Modified by: Dan Harms
;; Keywords: proviso smart-mode-line

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
(require 'smart-mode-line)
(require 'subr-x)

(defvar proviso-orig-sml-replacer-regexp-list '()
  "The original value of `sml/replacer-regexp-list'.")

(with-eval-after-load 'smart-mode-line
  (setq proviso-orig-sml-replacer-regexp-list
        (copy-tree sml/replacer-regexp-list)))

(defun proviso--set-sml-abbrevs (proj)
  "Set mode-line abbreviations according to PROJ's project definition."
  (let ((root (proviso-get proj :root-dir))
        (lst (proviso-get proj :proj-alist))
        title elt entry result)
    (dolist (element lst)
      (setq title (plist-get element :name))
      (setq entry (plist-get element :dir))
      (setq elt (if (and entry (file-name-absolute-p entry))
                    entry (concat root entry)))
      ;; ensure a trailing slash
      (setq elt (proviso--abbreviate-dir (file-name-as-directory elt)))
      (push (list elt (concat (upcase title) ":")) result))
    ;; if no directories, point to the root
    (when (seq-empty-p lst)
      (push (list (proviso--abbreviate-dir root)
                  (concat (upcase (proviso-get proj :project-name)) ":"))
            result))
    (proviso-put proj :sml-abbrevs
                 (append (proviso-get proj :sml-abbrevs)
                         result))))

(defun proviso--sml-set-build-dirs (proj)
  "Set sml modeline according to `:build-subdirs' setting of project PROJ.
See `sml/replacer-regexp-list'."
  (let ((remote (proviso-get proj :remote-prefix))
        (root (proviso-get proj :root-dir))
        (lst (proviso-get proj :build-subdirs))
        entry dir name result)
    (dolist (element lst)
      (setq entry (plist-get element :dir))
      (setq dir (if (and entry (file-name-absolute-p entry))
                    entry (concat root entry)))
      (setq name (or (plist-get element :name)
                     (directory-file-name dir)))
      (unless (zerop (length dir))
        (push (list
               (proviso--abbreviate-dir (file-name-as-directory dir))
               (concat (upcase name) ":")) result)))
    (proviso-put proj :sml-abbrevs
                 (append (proviso-get proj :sml-abbrevs)
                         result))))

(defun proviso--activate-sml-abbrevs (proj old)
  "Activate the sml regexps as defined in PROJ.
PROJ is now the active project, replacing OLD."
  (setq sml/replacer-regexp-list
        (append proviso-orig-sml-replacer-regexp-list
                (proviso-get proj :sml-abbrevs))))

(defun proviso--abbreviate-dir (name)
  "Abbreviate NAME, a (possibly remote) project root, as necessary.
It is usually preferable to have a short project prefix.  This
may just come down to substituting `~' for the home directory.
Note that `abbreviate-file-name' doesn't work for remote paths,
in case you are tempted to try to use it."
  (let ((home
         (string-trim (shell-command-to-string "echo ~"))))
    (replace-regexp-in-string home "~" name t)))

(add-hook 'proviso-hook-on-project-init 'proviso--sml-set-build-dirs)
(add-hook 'proviso-hook-on-project-init 'proviso--set-sml-abbrevs)
(add-hook 'proviso-hook-on-project-active 'proviso--activate-sml-abbrevs)

(provide 'proviso-sml)
;;; proviso-sml.el ends here
