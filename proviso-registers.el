;;; proviso-registers.el --- setup proviso registers
;; Copyright (C) 2017  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Tuesday, April  4, 2017
;; Version: 1.0
;; Modified Time-stamp: <2017-08-15 17:51:24 dharms>
;; Modified by: Dan Harms
;; Keywords: proviso project register
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

;;

;;; Code:
(require 'proviso-core)

(defun proviso--set-registers (proj)
  "Set registers for quick navigation according to PROJ's project definition."
  (let ((remote (proviso-get proj :remote-prefix))
        (root (proviso-get proj :root-dir))
        (srcdirs (proviso-get proj :proj-alist))
        (blddirs (proviso-get proj :build-subdirs))
        dir reg lst)
    (dolist (element srcdirs)
      (setq reg (plist-get element :register))
      (setq dir (plist-get element :dir))
      (unless (and dir (file-name-absolute-p dir))
        (setq dir (concat root dir)))
      (when (and reg (characterp reg))
        (push (cons reg (cons 'file (concat remote dir))) lst)))
    (dolist (element blddirs)
      (setq reg (plist-get element :register))
      (setq dir (plist-get element :dir))
      (unless (and dir (file-name-absolute-p dir))
        (setq dir (concat root dir)))
      (when (and reg (characterp reg))
        (push (cons reg (cons 'file (concat remote dir))) lst)))
    ;; also set a global register to go to the root
    (push (cons ?r (cons 'file (concat remote root))) lst)
    ;; and another to go to the first (privileged) source dir
    (when (< 0 (length srcdirs))
      (push (cons ?c (cons 'file
                           (let ((dir (plist-get (car srcdirs) :dir)))
                             (if (and dir (file-name-absolute-p dir))
                                 (concat remote dir)
                               (concat remote root dir))))) lst))
    (proviso-put proj :registers lst)
    ))

(defun proviso--activate-registers (proj old)
  "Activate the registers as defined in PROJ.
PROJ is now the active project, replacing OLD.
Registers should have been stored in :registers."
  (let ((lst (proviso-get proj :registers)))
    (dolist (elt lst)
      (set-register (car elt) (cdr elt)))))

(add-hook 'proviso-hook-on-project-init 'proviso--set-registers)
(add-hook 'proviso-hook-on-project-active 'proviso--activate-registers)

(provide 'proviso-registers)
;;; proviso-registers.el ends here
