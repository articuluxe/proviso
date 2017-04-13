;;; proviso-tags.el --- add tags functionality to profiles
;; Copyright (C) 2017  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Thursday, January  5, 2017
;; Version: 1.0
;; Modified Time-stamp: <2017-04-13 07:58:18 dharms>
;; Modified by: Dan Harms
;; Keywords: proviso tags

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
(require 'tramp)
(require 'proviso-etags-select)

(defvar proviso-tags-lookup-target-project nil
  "The active project when a tag is first looked up.")

(defun proviso-tags-store-project ()
  "Store the current project."
  (setq proviso-tags-lookup-target-project (symbol-name proviso-local-proj)))

(defun proviso-tags-find-tag ()
  "Find a tag based on the current profile."
  (interactive)
  (proviso-tags-store-project)
  (etags-select-find-tag))

(defun proviso-tags-find-tag-at-point ()
  "Find the tag at point based on the current profile."
  (interactive)
  (proviso-tags-store-project)
  (etags-select-find-tag-at-point))

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

(defun proviso-tags-compute-tags-dir (proj dir)
  "Compute where a profile PROJ's local TAGS should live.
DIR gives the root directory."
  (let ((base (or (getenv "EMACS_TAGS_DIR") "~"))
        (sub (or (proviso-get proj :tags-subdir) ".tags/"))
        dest)
    (unless dir (setq dir default-directory))
    (unless (tramp-tramp-file-p dir)
      ;; in the local case, set the base according to the project
      (setq base dir))
    (setq dest (concat (file-name-as-directory base) sub))
    (if (tramp-tramp-file-p dir)
        (concat dest (file-name-as-directory
                      (proviso-tags-compute-remote-subdir-stem proj)))
      dest)))

(defun proviso-tags-on-init (proj)
  "Initialize tags functionality when profile PROJ is initialized."
  (let ((root (proviso-get proj :root-dir)))
    (and root (not (proviso-get proj :tags-dir))
         (proviso-put proj :tags-dir
                   (proviso-tags-compute-tags-dir
                    proj
                    (concat (proviso-get proj :remote-prefix)
                            root))))))

(add-hook 'proviso-on-project-pre-init 'proviso-tags-on-init)

(defun proviso-etags--real-file-name (filename)
  "Return the tag's correct destination file for FILENAME.
This may prepend a remote prefix."
  (concat
   (proviso-get proviso-tags-lookup-target-project :remote-prefix)
   (if (file-name-absolute-p filename)
       filename
     ;; todo: used to append 'src-sub-dir here?
     (proviso-get proviso-tags-lookup-target-project :root-dir))))

;; point etags-select to our function
(setq etags-select-real-file-name 'proviso-etags--real-file-name)

(defun proviso-etags--insert-file-name(filename tag-file-path)
  "Return a display name for FILENAME.
TAG-FILE-PATH is the TAGS file being looked at."
  (if (file-name-absolute-p filename)
      filename
    (concat (proviso-get proviso-tags-lookup-target-project :root-dir)
            (proviso-get proviso-tags-lookup-target-project :src-sub-dir)
            filename)))

;; point etags-select to our function
(setq etags-select-insert-file-name 'proviso-etags--insert-file-name)

(provide 'proviso-tags)
;;; proviso-tags.el ends here
