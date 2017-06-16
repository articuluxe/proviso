;;; proviso-compile.el --- proviso compile
;; Copyright (C) 2017  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Wednesday, May 24, 2017
;; Version: 1.0
;; Modified Time-stamp: <2017-06-15 20:55:45 dharms>
;; Modified by: Dan Harms
;; Keywords: proviso project compile

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
(require 'seq)

(defvar proviso-compile--should-close-compile-window nil
  "Internal setting controls closing of the window post-compile.")

(defvar proviso-compile-command-list
  (list #'proviso-compile-command-std #'proviso-compile-command-repo)
  "List of functions to create compile commands.")
(defvar proviso-compile-command (car proviso-compile-command-list)
  "The default compile command.")

(defvar proviso-compile-dir-history '()
  "History of directories used in compile commands.")
(defvar proviso-compile-cmd-history '()
  "History of commands used in compile commands.")

(defun proviso-compile-command-std (&optional arg)
  "Create a compile command for standard projects.
ARG allows customizing behavior."
  (let ((root (or (proviso-get proviso-curr-proj :root-dir) "./"))
        (cmd (or (proviso-get proviso-curr-proj :compile-cmd) "make"))
        (blddirs (proviso-get proviso-curr-proj :build-subdirs))
        subdirs subdir dir)
    (setq subdirs (mapcar (lambda (elt)
                            (file-name-as-directory (plist-get elt :dir))) blddirs))
    (if arg
        (setq root (read-directory-name "Compile in: " root nil t))
      (setq subdir (cond ((eq (seq-length subdirs) 1)
                          (car subdirs))
                         ((seq-empty-p subdirs)
                          "")
                         (t (completing-read "Compile in: " (cons "./" subdirs) nil t)))))
    (and root (file-remote-p root)
         (setq root (with-parsed-tramp-file-name root file file-localname)))
    (setq dir (concat root (unless (string= subdir "./") subdir)))
    (add-to-list 'proviso-compile-dir-history dir)
    (format "cd %s && %s" dir cmd)
  ))

(defun proviso-compile-command-repo (&optional arg)
  "Create a compile command for standard projects.
ARG allows customizing behavior."
  (let ((root (or (proviso-get proviso-curr-proj :root-dir) "./"))
        (cmd (or (proviso-get proviso-curr-proj :compile-cmd) "make"))
        (blddirs (proviso-get proviso-curr-proj :build-subdirs))
        (preface (or (proviso-get proviso-curr-proj :compile-cmd-preface)
                     "source %srepo-setup.sh && "))
        origroot subdirs subdir dir)
    (setq origroot root)
    (setq subdirs (mapcar (lambda (elt)
                            (file-name-as-directory (plist-get elt :dir))) blddirs))
    (if arg
        (setq root (read-directory-name "Compile in: " root nil t))
      (setq subdir (cond ((eq (seq-length subdirs) 1)
                          (car subdirs))
                         ((seq-empty-p subdirs)
                          "")
                         (t (completing-read "Compile in: " (cons "./" subdirs) nil t)))))
    (and root (file-remote-p root)
         (setq root (with-parsed-tramp-file-name root file file-localname)))
    (setq dir (concat root (unless (string= subdir "./") subdir)))
    (add-to-list 'proviso-compile-dir-history dir)
    (format (concat preface "cd %s && %s") origroot dir cmd)
  ))

(defun proviso-compile-choose-compile-command()
  "Select the command used to create compile commands.
This will be used for projects that don't specify their own value."
  (interactive)
  (let ((lst (mapcar (lambda (elt)
                       (cons (symbol-name elt) elt))
                     proviso-compile-command-list))
        result)
    (setq result (completing-read "Compile command: " lst))
    (when result
      (setq proviso-compile-command (cdr result)))))

(defun proviso-compile (&optional arg)
  "Start the process of compilation, according to some settings.
ARG allows customizing behavior."
  (interactive "P")
  (setq proviso-compile--should-close-compile-window
        (not (get-buffer-window "*compilation*" 'visible)))
  (let ((cmd (or (proviso-get proviso-curr-proj :compile-defun)
                 proviso-compile-command
                 'proviso-compile-command-std)))
    (when (setq compile-command (funcall cmd arg))
      ;; (funcall-interactively 'compile (list arg))))
      (call-interactively 'compile))))

(defun proviso-recompile (&optional arg)
  "Start the process of re-compilation, according to some settings.
ARG allows customizing behavior."
  (interactive "P")
  (setq proviso-compile--should-close-compile-window
        (not (get-buffer-window "*compilation*" 'visible)))
  (call-interactively 'recompile))

(with-eval-after-load 'cc-mode
  (define-key c-mode-base-map (kbd "\C-c RET") #'proviso-compile)
  (define-key c-mode-base-map "\C-cm" #'proviso-recompile)
  (define-key c-mode-base-map "\C-ck" #'kill-compilation))

(provide 'proviso-compile)
;;; proviso-compile.el ends here
