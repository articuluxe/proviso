;;; proviso-docker.el --- docker utilities
;; Copyright (C) 2021  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Wednesday, March 17, 2021
;; Version: 1.0
;; Modified Time-stamp: <2021-03-17 20:02:53 dharms>
;; Modified by: Dan Harms
;; Keywords: tools profiles project
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
;; Utilities useful for projects using docker containers.
;;

;;; Code:
(require 'proviso-core)

(defun proviso--test-docker (proj)
  "Test project PROJ for docker functionality."
  (when (and (executable-find "docker")
             (proviso-get proj :docker-container))
    (let ((container (proviso-get proj :docker-container)))
      ;; (add-hook 'proviso-hook-file-transformers
      ;;           #'proviso-docker-transform)
      ))
  )

(defun proviso-docker-transform (proj loc)
  "Transform the location LOC according to project PROJ."
  loc)

(add-hook 'proviso-hook-on-project-post-init 'proviso--test-docker)

(provide 'proviso-docker)
;;; proviso-docker.el ends here
