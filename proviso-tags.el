;;; proviso-tags.el --- Add tags functionality to proviso
;; Copyright (C) 2017-2019, 2023  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Thursday, January  5, 2017
;; Version: 1.0
;; Modified Time-stamp: <2023-10-13 11:19:57 dharms>
;; Modified by: Dan Harms
;; Keywords: tools proviso tags
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
;; Helper functions to make working with TAGS easier in proviso projects.
;;

;;; Code:
(require 'proviso-core)
(require 'tramp)
(require 'proviso-etags-table)
(require 'proviso-etags-select)

(defun proviso-tags-on-init (proj)
  "Initialize tags functionality when profile PROJ is initialized.
This includes storing the setting for `etags-table-alist'
into :tags-alist."
  (let* ((remote (proviso-get proj :remote-prefix))
         (root (proviso-get proj :root-dir))
         (lst (proviso-get proj :proj-alist))
         (tags-adds (proviso-get proj :ctags-additions))
         (scratch (proviso-get proj :scratch-dir))
         (local-scratch (proviso-get proj :local-scratch-dir))
         (subdir ".tags/")
         (tag-root (if (and remote local-scratch)
                       (concat local-scratch subdir)
                     (concat scratch subdir)))
         (remote-root (if (and remote local-scratch)
                          (concat scratch subdir)))
         tags-alist ext-dirs curr entry dir)
    ;; tag-alist is a list of lists of at least one element.  Each element is
    ;; a list of strings: the car is the regex to match filenames, the cdr the
    ;; various tag files that apply to files that match.  The first element of
    ;; tags-alist represents the project root; there will be another entry for
    ;; each src dir that is external to the project root.
    (setq tags-alist
          (list
           (list (concat
                  ;; this first capture group is needed to match
                  ;; any remote prefix
                  "^\\(.*\\)" root "\\(.*\\)$"))))
    (dolist (element lst)
      (setq curr (plist-get element :name))
      (setq entry (expand-file-name
                   (concat tag-root curr "-tags")))
      (setq dir (proviso-substitute-env-vars (plist-get element :dir)))
      (when (and dir (file-name-absolute-p dir))
        ;; save external dirs, which need a separate entry
        (push (concat
               ;; this first capture group is needed to match
               ;; any remote prefix
               "^\\(.*\\)" dir "\\(.*\\)$")
              ext-dirs))
      (push entry (car tags-alist)))
    (dolist (element tags-adds)
      (setq curr (plist-get element :name))
      (setq entry (expand-file-name
                   (concat tag-root curr "-tags")))
      (setq dir (proviso-substitute-env-vars
                 (or (plist-get element :loc)
                     (plist-get element :dir))))
      ;; not pushing an external dir here, don't think it's needed
      (push entry (car tags-alist)))
    (when (seq-empty-p (cdr (car tags-alist)))
      ;; if nothing specified, add an entry for the root
      (push (expand-file-name
             (concat
              tag-root (proviso-get proj :project-name) "-tags"))
            (car tags-alist)))
    (proviso-put proj :tags-dir tag-root)
    (when remote-root (proviso-put proj :tags-remote-dir remote-root))
    (setq tags-alist (mapcar 'nreverse tags-alist))
    (mapc (lambda (dir)
            (add-to-list
             'tags-alist
             (append (list dir) (cdr (car tags-alist))) t))
          (nreverse ext-dirs))
    (proviso-put proj :tags-alist tags-alist)))

(add-hook 'proviso-hook-on-project-init 'proviso-tags-on-init)
(add-hook 'proviso-hook-on-project-active 'proviso-activate-tags-table)

(defun proviso-activate-tags-table (proj old)
  "Activate the TAGS table specified in `:tags-alist'.
PROJ is now the active project, replacing OLD."
  ;; todo: instead of overwriting etags-table-alist, we could
  ;; maintain a global value comprised of all known projects
  (setq etags-table-alist (proviso-get proj :tags-alist)))

(defun proviso-etags--real-file-name (filename)
  "Return the tag's correct destination file for FILENAME.
This may prepend a remote prefix."
  (concat
   (proviso-get proviso-curr-proj :remote-prefix)
   (if (file-name-absolute-p filename)
       filename
     (concat
      (proviso-get proviso-curr-proj :root-dir)
      filename))))

;; point etags-select to our function
(setq etags-select-real-file-name #'proviso-etags--real-file-name)

(defun proviso-etags--insert-file-name(filename tag-file-path)
  "Return a display name for FILENAME.
TAG-FILE-PATH is the TAGS file being looked at."
  (if (file-name-absolute-p filename)
      filename
    (concat (proviso-get proviso-curr-proj :root-dir)
            filename)))

;; point etags-select to our function
(setq etags-select-insert-file-name #'proviso-etags--insert-file-name)

(provide 'proviso-tags)
;;; proviso-tags.el ends here
