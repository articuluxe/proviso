;;; proviso-tags.el --- add tags functionality to profiles
;; Copyright (C) 2017  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Thursday, January  5, 2017
;; Version: 1.0
;; Modified Time-stamp: <2017-09-22 08:11:44 dharms>
;; Modified by: Dan Harms
;; Keywords: tools proviso tags
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
;; Helper functions to make working with TAGS easier in proviso projects.
;;

;;; Code:
(require 'proviso-core)
(require 'tramp)
(require 'proviso-etags-table)
(require 'proviso-etags-select)

(defun proviso-tags-compute-remote-subdir-stem (proj)
  "Compute remote profile PROJ's stem.
Format is useful for uniquely naming the local TAGS directory."
  (concat
   (replace-regexp-in-string
    "/\\|\\\\" "!"
    (proviso-get proj :remote-host) t t)
   "!"
   (replace-regexp-in-string
    "/\\|\\\\" "!"
    (proviso-get proj :root-stem))))

(defun proviso-tags-compute-tags-dir (proj base)
  "Compute where a profile PROJ's local TAGS should live.
BASE gives the root directory."
  (let ((tags-base (or (getenv "EMACS_TAGS_DIR") "~"))
        (sub (or (proviso-get proj :tags-subdir) ".tags/"))
        dest)
    (unless base (setq base default-directory))
    (unless (tramp-tramp-file-p base)
      ;; in the local case, set the tags-base according to the project
      (setq tags-base base))
    (setq dest (concat (file-name-as-directory tags-base)
                       (file-name-as-directory sub)))
    (if (tramp-tramp-file-p base)
        (concat dest (file-name-as-directory
                      (proviso-tags-compute-remote-subdir-stem proj)))
      dest)))

(defun proviso-tags-on-init (proj)
  "Initialize tags functionality when profile PROJ is initialized.
This includes storing the setting for `etags-table-alist'
into :tags-alist."
  (let ((remote (proviso-get proj :remote-prefix))
        (root (proviso-get proj :root-dir))
        (lst (proviso-get proj :proj-alist))
        tag-root names curr entry)
    (setq tag-root
          (proviso-tags-compute-tags-dir
           proj (concat remote root)))
    (dolist (element lst)
      (setq curr (plist-get element :name))
      (setq entry (expand-file-name
                   (concat tag-root curr "-tags")))
      (push entry names))
    (when (seq-empty-p names)
      ;; if nothing specified, add an entry for the root
      (push (expand-file-name
             (concat
              tag-root (proviso-get proj :project-name) "-tags")) names))
    (proviso-put proj :tags-dir tag-root)
    (proviso-put proj :tags-alist
                 (append (list (concat
                                ;; this first capture group is needed to match
                                ;; the remote prefix
                                "^\\(.*\\)"
                                (proviso-get proj :root-stem)
                                "\\(.*\\)$"))
                         (nreverse names)))))

(add-hook 'proviso-hook-on-project-init 'proviso-tags-on-init)
(add-hook 'proviso-hook-on-project-active 'proviso-activate-tags-table)

(defun proviso-activate-tags-table (proj old)
  "Activate the TAGS table specified in `:tags-alist'.
PROJ is now the active project, replacing OLD."
  ;; todo: instead of overwriting etags-table-alist, we could
  ;; maintain a global value comprised of all known projects
  (setq etags-table-alist `( ,(proviso-get proj :tags-alist))))

(defun proviso-etags--real-file-name (filename)
  "Return the tag's correct destination file for FILENAME.
This may prepend a remote prefix."
  (concat
   (proviso-get proviso-curr-proj :remote-prefix)
   (if (file-name-absolute-p filename)
       filename
     (concat
      (proviso-get proviso-curr-proj :root-dir)))))

;; point etags-select to our function
(setq etags-select-real-file-name 'proviso-etags--real-file-name)

(defun proviso-etags--insert-file-name(filename tag-file-path)
  "Return a display name for FILENAME.
TAG-FILE-PATH is the TAGS file being looked at."
  (if (file-name-absolute-p filename)
      filename
    (concat (proviso-get proviso-curr-proj :root-dir)
            filename)))

;; point etags-select to our function
(setq etags-select-insert-file-name 'proviso-etags--insert-file-name)

(provide 'proviso-tags)
;;; proviso-tags.el ends here
