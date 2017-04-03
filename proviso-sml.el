;;; proviso-sml.el --- add support to profiles for smart-mode-line
;; Copyright (C) 2017  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Friday, January  6, 2017
;; Version: 1.0
;; Modified Time-stamp: <2017-04-03 17:52:41 dharms>
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
  (setq proviso-orig-sml-replacer-regexp-list 'sml/replacer-regexp-list))

(defun proviso--set-sml-abbrevs (proj)
  "Set mode-line abbreviations according to PROJ's project definition."
  (let ((root (proviso-get proj :root-dir))
        (lst (proviso-get proj :proj-alist))
        title elt entry result)
    (dolist (element lst)
      (setq title (plist-get element :name))
      (setq entry (plist-get element :dir))
      (setq elt (if (and entry (file-name-absolute-p entry))
                    entry
                  (concat root entry)))
      (setq elt (proviso--abbreviate-dir elt))
      (push (list elt (concat (upcase title) ":")) result))
    (setq sml/replacer-regexp-list
          (append sml/replacer-regexp-list result))))

(defun proviso--sml-set-build-dirs (proj)
  "Set sml modeline according to `:build-subdirs' setting of project PROJ.
See `sml/replacer-regexp-list'."
  (let ((remote (proviso-get proj :remote-prefix))
        (root (proviso-get proj :root-dir))
        (lst (proviso-get proj ::build-subdirs))
        dir name)
    (dolist (element lst)
      (setq dir (plist-get element :dir))
      (setq name (or (plist-get element :name)
                     (concat (upcase (directory-file-name dir)) ":")))
      (unless (zerop (length dir))
        (add-to-list 'sml/replacer-regexp-list (list dir name) t))
      )))

(defun proviso--abbreviate-dir (name)
  "Abbreviate NAME, a (possibly remote) project root, as necessary.
It is usually preferable to have a short project prefix.  This
may just come down to substituting `~' for the home directory.
Note that `abbreviate-file-name' doesn't work for remote paths,
in case you are tempted to try to use it."
  (let ((home
         (string-trim (shell-command-to-string "echo ~"))))
    (replace-regexp-in-string home "~" name t)))

(add-hook 'proviso-on-project-init 'proviso--set-sml-abbrevs)

(provide 'proviso-sml)
;;; proviso-sml.el ends here
