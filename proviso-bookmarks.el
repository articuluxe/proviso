;;; proviso-bookmarks.el --- Settings for proviso bookmarks
;; Copyright (C) 2017-2020, 2022, 2025  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Tuesday, April 18, 2017
;; Version: 1.0
;; Modified Time-stamp: <2025-12-19 12:37:15 dharms>
;; Modified by: Dan Harms
;; Keywords: tools proviso project bookmarks
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
;; Functionality relating to using bookmarks in proviso.
;;

;;; Code:
(require 'proviso-core)
(require 'bookmark)

(defun proviso--init-bookmarks (proj)
  "Set bookmarks according to PROJ's project definition."
  (let* ((dir (proviso-get proj :scratch-dir))
         (name (proviso-get proj :project-name))
         (file (or (proviso-get proj :bookmark-file)
                   (concat dir name ".bmk"))))
    (proviso-put proj :bookmark-file file)
    (when (file-exists-p file)
      (let ((proviso-inspect-files-p nil))
        (bookmark-load file nil nil t)))))

(add-hook 'proviso-hook-on-project-init 'proviso--init-bookmarks)

(provide 'proviso-bookmarks)
;;; proviso-bookmarks.el ends here
