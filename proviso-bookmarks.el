;;; proviso-bookmarks.el --- settings for proviso bookmarks
;; Copyright (C) 2017-2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Tuesday, April 18, 2017
;; Version: 1.0
;; Modified Time-stamp: <2018-05-02 17:44:14 dharms>
;; Modified by: Dan Harms
;; Keywords: tools proviso project bookmarks
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
;; Functionality relating to using bookmarks in proviso.
;;

;;; Code:
(require 'proviso-core)
(require 'bookmark+)
(require 'f)

(defvar proviso-bookmarks-create-bmk-on-proj-init nil
  "Create bookmark file automatically when switching to a project.
If non-nil, a bookmark file will be created in the project's
root directory when a project is first made active.  If nil, the
bookmark file will only be created on demand.")

(defun proviso--init-bookmarks (proj)
  "Set bookmarks according to PROJ's project definition."
  (let ((remote (proviso-get proj :remote-prefix))
        (root (proviso-get proj :root-dir))
        (name (proviso-get proj :project-name)))
    (proviso-put proj :bookmark-file
     (concat remote root name ".bmk"))))

(defun proviso--activate-bookmarks-maybe (proj old)
  "Maybe activate the bookmark file as defined by PROJ's settings.
PROJ is now the active project, replacing OLD.
See `proviso-bookmarks-create-bmk-on-proj-init'."
  (when proviso-bookmarks-create-bmk-on-proj-init
    (proviso-bookmarks--activate-file proj)))

(defun proviso-bookmarks-switch-to-bookmark ()
  "Activate a bookmark file according to the current project."
  (interactive)
  (let ((proj (proviso-current-project)))
    (when proj
      (proviso-bookmarks--activate-file proj))))

(defun proviso-bookmarks--activate-file (proj)
  "Activate the bookmark file as defined by PROJ's settings.
The bookmark file should have been stored in :bookmark-file."
  (let ((file (proviso-get proj :bookmark-file)))
    (when file
      (bmkp-switch-bookmark-file-create file t))))

(add-hook 'proviso-hook-on-project-init 'proviso--init-bookmarks)
(add-hook 'proviso-hook-on-project-active 'proviso--activate-bookmarks-maybe)

(provide 'proviso-bookmarks)
;;; proviso-bookmarks.el ends here
