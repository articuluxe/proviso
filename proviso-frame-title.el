;;; proviso-frame-title.el --- proviso frame title
;; Copyright (C) 2017  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Friday, July  7, 2017
;; Version: 1.0
;; Modified Time-stamp: <2017-09-22 08:07:46 dharms>
;; Modified by: Dan Harms
;; Keywords: tools project proviso frame title
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
;; Utility to set the frame title in concert with proviso.
;;

;;; Code:
(require 'proviso-core)

(setq-default
 frame-title-format
 '(:eval
   (format "%s@%s: %s %s"
           (or (file-remote-p default-directory 'user)
               user-real-login-name)
           (or (file-remote-p default-directory 'host)
               (system-name))
           (if dired-directory
               (concat "{" (buffer-name) "}")
             (buffer-name))
           (if (and (featurep 'proviso)
                    (proviso-current-project-name))
               (concat "(" (upcase (proviso-current-project-name)) ")")
             "")
           )))

(provide 'proviso-frame-title)
;;; proviso-frame-title.el ends here
