;;; proviso-gud.el --- Debugging utilities for proviso
;; Copyright (C) 2018-2019  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Friday, January 26, 2018
;; Version: 1.0
;; Modified Time-stamp: <2019-10-16 13:22:23 dharms>
;; Modified by: Dan Harms
;; Keywords: tools gdb proviso
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
;; Helper utilties to launch debugging sessions like gud or realgud.
;;

;;; Code:
(require 'proviso-core)
(require 'ivy)
(require 'realgud)

;; gud settings
(setq realgud-safe-mode nil)
;; gdb settings
(add-hook 'gud-mode-hook
          (lambda()
            (set (make-local-variable 'gdb-show-main) t)
            ;; highlight recently-changed variables
            (set (make-local-variable 'gdb-show-changed-values) t)
            ;; watch expressions sharing same variable name
            (set (make-local-variable 'gdb-use-colon-colon-notation) t)
            (set (make-local-variable 'gdb-create-source-file-list) nil)
            (gdb-many-windows 1)
            ))

(defun proviso-gud--exe-suitable-p (exe)
  "Return non-nil if EXE is an executable that can be debugged."
  (or (file-directory-p exe)
      (file-executable-p exe)))

(defun proviso-gud--script-suitable-p (script)
  "Return non-nil if SCRIPT is a valid script to be debugged."
  (or (file-directory-p script)
      (string-match-p "\\.py$" script)))

(defun proviso-gud-get-debug-script (&optional arg predicate)
  "Fetch a script to be debugged according to the current project.
ARG allows customizing the location to look in.
PREDICATE is an optional predicate to validate filenames."
  (let ((cands (proviso-gud-gather-debug-dirs (proviso-current-project)))
        (dir-prompt "Find script in: ")
        (script-prompt "Debug script: ")
        dir script)
    (cond ((and arg (eq (prefix-numeric-value arg) 16))
           (setq script (read-file-name
                         script-prompt default-directory nil t nil predicate)))
          ((eq (length cands) 1)
           (setq dir (cdr (car cands))))
          ((and cands arg (eq (prefix-numeric-value arg) 4))
           (setq dir (completing-read dir-prompt cands)))
          (cands
           (setq dir (cdr (car cands))))
          (t
           (setq dir (or (proviso-current-project-root) default-directory))))
    (unless script
      (setq script (read-file-name script-prompt dir nil t nil predicate)))
    script))

(defun proviso-gud-gather-debug-dirs (proj)
  "Gather all debug dirs for project PROJ."
  (let ((remote (proviso-get proj :remote-prefix))
        (root (proviso-get proj :root-dir))
        (dbgdirs (proviso-get proj :debug-subdirs))
        (blddirs (proviso-get proj :build-subdirs))
        lst entry dir)
    (dolist (elt dbgdirs)
      (setq entry (plist-get elt :dir))
      (setq dir (if (and entry (file-name-absolute-p entry))
                    entry (concat root entry)))
      (when (file-directory-p (concat remote dir))
        (add-to-list 'lst (cons dir (concat remote dir)))))
    (unless lst
      (dolist (elt blddirs)
        (setq entry (plist-get elt :dir))
        (setq dir (if (and entry (file-name-absolute-p entry))
                      entry (concat root entry)))
        (when (file-directory-p (concat remote dir))
          (add-to-list 'lst (cons dir (concat remote dir))))))
    (nreverse lst)))

(defun proviso-gud-get-debug-exe (&optional arg)
  "Fetch an executable to be debugged according to the current project.
ARG allows customizing the location to search in."
  (let ((cands (proviso-gud-gather-debug-dirs (proviso-current-project)))
        (dir-prompt "Find executable in: ")
        (exe-prompt "Debug executable: ")
        dir exe)
    (setq dir (cond ((and arg (eq (prefix-numeric-value arg) 16))
                     (read-file-name dir-prompt default-directory nil t))
                    ((eq (length cands) 1)
                     (cdr (car cands)))
                    ((and cands arg (eq (prefix-numeric-value arg) 4))
                     (read-file-name dir-prompt
                                     (completing-read dir-prompt cands) nil t))
                    ((and cands)
                     (cdr (car cands)))
                    (t (or (proviso-current-project-root) default-directory))))
    (setq exe (read-file-name exe-prompt dir nil t nil
                              #'proviso-gud--exe-suitable-p))
    exe))

;;;###autoload
(defun proviso-gud-open-gdb (&optional arg)
  "Open gdb according to the current project.
ARG allows customizing the directory to look in for executables."
  (interactive "P")
  (let ((exe (proviso-gud-get-debug-exe arg)))
    (if exe
        (gdb (concat "gdb -i=mi " exe))
      (message "No executable found."))))

;;;###autoload
(defun proviso-gud-open-realgud (&optional arg)
  "Open realgud according to the current project.
ARG allows customizing the directory to look in for executables."
  (interactive "P")
  (let ((exe (proviso-gud-get-debug-exe arg)))
    (if exe
        (realgud:gdb (concat "gdb " exe))
      (message "No executable found."))))

;;;###autoload
(defun proviso-gud-open-pdb (&optional arg)
  "Open pdb according to the current project.
ARG allows customizing the directory to look in for scripts."
  (interactive "P")
  (let ((script (proviso-gud-get-debug-script
                 arg
                 #'proviso-gud--script-suitable-p)))
    (if script
        (pdb (concat "python -m pdb " script))
      (message "No script found."))))

(provide 'proviso-gud)
;;; proviso-gud.el ends here
