;;; proviso-dashboard.el --- a dashboard for proviso projects
;; Copyright (C) 2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Wednesday, May 16, 2018
;; Version: 1.0
;; Modified Time-stamp: <2018-10-13 13:20:44 dharms>
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
;; A dashboard for a project overview.
;;

;;; Code:
(require 'proviso-core)
(require 'proviso-gui)

(defvar-local proviso-dashboard-buffer-name nil
  "Buffer name for `proviso-dashboard' mode.")

(defconst proviso-dashboard-buffer-name-prefix "*%s-project*"
  "Buffer prefix string for `proviso-dashboard'.
This will be formatted with the project name.")

(defun proviso-dashboard-revert-buffer ()
  "Reverts (recreates) the dashboard buffer."
  (interactive)
  (proviso-dashboard-create (proviso-current-project)))

(defvar proviso-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" #'proviso-dashboard-revert-buffer)
    map)
  "Keymap for `proviso-dashboard-mode'.")

(define-derived-mode proviso-dashboard-mode special-mode
  "Dashboard"
  "Major mode for providing an overview of a proviso project.
\\<proviso-dashboard-mode-map>
"
  (setq buffer-read-only t)
  (setq truncate-lines t)
  )

;;;###autoload
(defun proviso-dashboard-show (&optional arg)
  "Show a dashboard.
Optional ARG allows choosing a project."
  (interactive "P")
  (let ((proj (if arg (proviso-choose-project)
                (proviso-current-project))))
    (proviso-dashboard-create proj)
    (pop-to-buffer proviso-dashboard-buffer-name)))

(defun proviso-dashboard-goto-root ()
  "Go to root dired of the current project."
  (interactive)
  (let ((proj proviso-local-proj))
    (when proj
      (proviso-dired-open-project proj))))

(defun proviso-dashboard-create (proj)
  "Create a dashboard for project PROJ."
  (interactive)
  (setq proviso-dashboard-buffer-name
        (format proviso-dashboard-buffer-name-prefix proj))
  (let ((buffer (get-buffer-create proviso-dashboard-buffer-name)))
    (proviso-gui-init-buffer buffer proviso-dashboard-mode-map)
    (with-current-buffer buffer
      (setq-local proviso-local-proj proj)
      (proviso-dashboard-mode))
    (proviso-gui-add-to-buffer
     buffer
     '((:heading "Project"
                 :content (lambda ()
                            (propertize (proviso-get proviso-local-proj :project-name)
                                        'face 'highlight)))
       (:heading "Root"
                 :content (lambda ()
                            (let ((file (proviso-get proviso-local-proj :root-dir)))
                              (propertize (replace-regexp-in-string
                                           (getenv "HOME") "~" file)
                                          'face '(bold))))
                 :bindings (("d" . proviso-dashboard-goto-root)))
       (:heading "Remote host"
                 :predicate (lambda ()
                              (proviso-get proviso-local-proj :remote-host))
                 :content (lambda ()
                            (propertize (proviso-get proviso-local-proj :remote-host)
                                        'face '(bold))))
       (:heading "Tags file"
                 :content (lambda ()
                            (let ((gen (proviso-get proviso-local-proj :tags-lastgen)))
                              (propertize (if gen (current-time-string gen)
                                            "")
                                          'face '(bold))))
                 :bindings (("t" . #'proviso-gentags-generate-tags)))
       (:heading "Bookmarks"
                 :content (lambda ()
                            (let ((bmk (proviso-get proviso-local-proj :bookmark-file)))
                              (propertize (replace-regexp-in-string
                                           (getenv "HOME") "~" bmk)
                                          'face
                                          (if (and bmk (file-exists-p bmk))
                                              '(bold) '(shadow))))))
       (:heading "Deployments"
                 :predicate (lambda () (proviso-get proviso-local-proj :deploy-file))
                 :content (lambda ()
                            (let ((file (proviso-get proviso-local-proj :deploy-file)))
                              (propertize (replace-regexp-in-string
                                           (getenv "HOME") "~" file)
                                          'face (if (file-exists-p file)
                                                    '(bold) '(shadow))))))
       (:heading "Clang format"
                 :content (lambda ()
                            (let ((file (proviso-get proviso-local-proj :clang-format)))
                              (concat
                               (propertize (replace-regexp-in-string
                                            (getenv "HOME") "~" file)
                                           'face
                                           (if (and file (file-exists-p file))
                                               '(bold) '(shadow)))
                               " ["
                               (if proviso-clang-format-active-p
                                   (propertize "active" 'face '(bold))
                                 (propertize "inactive" 'face '(shadow)))
                               "]"
                               )))
                 :bindings (("t" . proviso-clang-format-toggle-active)
                            ("f" . #'proviso-clang-format-buffer-or-region)
                            ))
       ))
    (proviso-gui-finalize-buffer buffer)
    ))

;;;###autoload
(defun proviso-dashboard-switch-to (proj)
  "Switch to the dashboard for projecdt PROJ in other window."
  (when proj
    (proviso-dashboard-create proj)
    (display-buffer proviso-dashboard-buffer-name)))

;; (defun proviso-dashboard-refresh-buffer ()
;;   "Refresh the dashboard."
;;   (interactive)
;;   (kill-buffer proviso-dashboard-buffer-name)
;;   (proviso-dashboard-create (proviso-current-project))
;;   (switch-to-buffer proviso-dashboard-buffer-name))

(provide 'proviso-dashboard)
;;; proviso-dashboard.el ends here
