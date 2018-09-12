;;; proviso-deploy.el --- deploy artifacts to locations
;; Copyright (C) 2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Wednesday, September 12, 2018
;; Version: 1.0
;; Modified Time-stamp: <2018-09-12 09:51:43 dharms>
;; Modified by: Dan Harms
;; Keywords: tools proviso projects
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
;; Deploy things to places.
;;

;;; Code:
(require 'proviso-core)
(require 'proviso-transfer)
(require 'proviso-gui)

(defvar-local proviso-deploy-buffer-name nil
  "Buffer name for `proviso-deploy' mode.")

(defconst proviso-deploy-buffer-name-prefix
  "*%s-deploy*"
  "Buffer prefix string for `proviso-deploy'.
This will be formatted with the project name.")

(defvar proviso-deploy-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `proviso-deploy-mode'.")

(define-derived-mode proviso-deploy-mode special-mode
  "Deploy"
  "Major mode for deploying things to places.
\<proviso-deploy-mode-map>
"
  (setq buffer-read-only t)
  (setq truncate-lines t))

;;;###autoload
(defun proviso-deploy-show (&optional arg)
  "Show a deploy buffer.
Optional arg allows choosing a project."
  (interactive "P")
  (let ((proj (if arg (proviso-choose-project)
                (proviso-current-project))))
    (proviso-deploy-create proj)
    (pop-to-buffer proviso-deploy-buffer-name)))

(defun proviso-deploy-create (proj)
  "Create a deployment buffer for project PROJ."
  (interactive)
  (setq proviso-deploy-buffer-name
        (format proviso-deploy-buffer-name-prefix proj))
  (let ((buffer (get-buffer-create proviso-deploy-buffer-name)))
    (proviso-gui-init-buffer buffer proviso-deploy-mode-map)
    (with-current-buffer buffer
      (setq-local proviso-local-proj proj)
      (proviso-deploy-mode))
    (proviso-gui-add-to-buffer
     buffer
     '((:heading "project"
                 :content
                 (lambda ()
                   (propertize
                    (proviso-get proviso-local-proj :project-name)
                    'face 'highlight)))))
    (proviso-gui-finalize-buffer buffer)
    ))

(provide 'proviso-deploy)
;;; proviso-deploy.el ends here
