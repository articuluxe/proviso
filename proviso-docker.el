;;; proviso-docker.el --- docker utilities
;; Copyright (C) 2021  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Wednesday, March 17, 2021
;; Version: 1.0
;; Modified Time-stamp: <2021-03-18 17:31:30 dharms>
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
  (let ((container (proviso-get proj :docker-container))
        src dst)
    (when (and (executable-find "docker")
               container
               (not (string-empty-p container)))
      (setq src (proviso-docker-query-mount container "Source"))
      (setq dst (proviso-docker-query-mount container "Destination"))
      (when (and src dst
                 (not (string-empty-p src))
                 (not (string-empty-p dst)))
        (proviso-put proj :docker-mount-src src)
        (proviso-put proj :docker-mount-dst dst)
        (message "Proviso-Docker will transform %s to %s for container %s"
                 src dst container)
        (add-hook 'proviso-hook-file-transformers
                  #'proviso-docker-transform)))))

(defun proviso-docker-transform (proj loc)
  "Transform the absolute location LOC according to project PROJ."
  (let ((src (proviso-get proj :docker-mount-src))
        (dst (proviso-get proj :docker-mount-dst)))
    (if (and src dst (string-match src loc))
        (replace-match dst nil t loc)
      loc)))

(defun proviso-docker-query-mount (container source-or-dest)
  "Query CONTAINER for the source and destination mounts.
SOURCE-OR-DEST should equal \"Source\" or \"Destination\".
Returns nil if any error.  There is no trailing slash in the
return value."
  (interactive)
  (let ((buffer (get-buffer-create "*Docker Output*")))
    (process-file "docker" nil buffer nil
                  "inspect" "-f"
                  (format "'{{range .Mounts}}{{.%s}}{{end}}'"
                          source-or-dest)
                  container)
    (with-current-buffer buffer
      (let ((str (string-trim (buffer-string))))
        (if (string-match "'\\(.+\\)'" str)
            (setq str (match-string-no-properties 1 str))
          (setq str nil))
        (erase-buffer)
        str))))

(add-hook 'proviso-hook-on-project-post-init 'proviso--test-docker)

(provide 'proviso-docker)
;;; proviso-docker.el ends here
