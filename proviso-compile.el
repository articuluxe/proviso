;;; proviso-compile.el --- Compile utilities for proviso
;; Copyright (C) 2017-2020  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Wednesday, May 24, 2017
;; Version: 1.0
;; Modified Time-stamp: <2020-02-25 09:32:03 dan.harms>
;; Modified by: Dan Harms
;; Keywords: c tools languages proviso project compile
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
;; Provide compile utilities.
;;

;;; Code:
(require 'proviso-core)
(require 'compile)
(require 'dash)
(require 'seq)

(setq compilation-environment '("TERM=xterm-256color"))

(defcustom proviso-compile-cmds-comint-filters nil
  "List of regexp to apply to compile commands.
A match means that command should be run in `comint-mode'."
  :group 'proviso-custom-group)

(defvar proviso-compile--window-visible nil
  "Internal variable tracks whether the compile window was visible originally.")
(defvar proviso-compile--window-count nil
  "Internal variable tracks how many windows existed pre-compile.")
(defvar proviso-compile--notify-timeout 3
  "Default timeout for compilation notifications, in seconds.")
(defvar proviso-compile--notify-msg "Compilation finished"
  "Default message for compilation notifications.")
(defvar proviso-compile--subtitle ""
  "A subtitle for compilation notifications.")
(defvar proviso--compile-notify-send-daemon-checked nil
  "We only check once if the `notify-send' daemon is running.")

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
  (let ((root (or (proviso-get (proviso-current-project) :root-dir) "./"))
        (cmds (proviso-get (proviso-current-project) :compile-cmds))
        (blddirs (proviso-get (proviso-current-project) :build-subdirs))
        subdirs subdir dir cmd)
    (setq subdirs (mapcar (lambda (elt)
                            (file-name-as-directory (plist-get elt :dir))) blddirs))
    (setq cmd
          (cond ((= (prefix-numeric-value arg) 16)
                 (completing-read "Compile command: "
                                  (append (seq-remove (lambda (elt)
                                                        (string-equal elt "make"))
                                                      cmds) '("make"))
                                  nil nil nil nil (car cmds)))
                ((seq-empty-p cmds)
                 "make")
                (t (car cmds))))
    (if arg
        (setq root (read-directory-name "Compile in: " default-directory nil t))
      (setq subdir (cond ((eq (seq-length subdirs) 1)
                          (car subdirs))
                         ((seq-empty-p subdirs)
                          "")
                         (t
                          (completing-read "Compile in: "
                                           (cons "./" subdirs)
                                           nil t nil nil
                                           (car subdirs))))))
    (and root (file-remote-p root)
         (setq root (with-parsed-tramp-file-name root file file-localname)))
    (setq dir (concat root (unless (string= subdir "./") subdir)))
    (add-to-list 'proviso-compile-dir-history dir)
    (format "cd %s && %s" dir cmd)
    ))

(defun proviso-compile-command-repo (&optional arg)
  "Create a compile command for projects using a repo layout.
ARG allows customizing behavior."
  (let ((root (or (proviso-get (proviso-current-project) :root-dir) "./"))
        (cmds (proviso-get (proviso-current-project) :compile-cmds))
        (blddirs (proviso-get (proviso-current-project) :build-subdirs))
        (preface (or (proviso-get (proviso-current-project) :compile-cmd-preface)
                     ". %srepo-setup.sh && "))
        origroot subdirs subdir dir cmd)
    (setq origroot root)
    (setq subdirs (mapcar (lambda (elt)
                            (file-name-as-directory (plist-get elt :dir))) blddirs))
    (setq cmd
          (cond ((= (prefix-numeric-value arg) 16)
                 (completing-read "Compile command: "
                                  (append (seq-remove (lambda (elt)
                                                        (string-equal elt "make"))
                                                      cmds) '("make"))
                                  nil nil nil nil (car cmds)))
                ((seq-empty-p cmds)
                 "make")
                (t (car cmds))))
    (if arg
        (setq root (read-directory-name "Compile in: " default-directory nil t))
      (setq subdir (cond ((eq (seq-length subdirs) 1)
                          (car subdirs))
                         ((seq-empty-p subdirs)
                          "")
                         (t
                          (completing-read "Compile in: "
                                           (cons "./" subdirs)
                                           nil t nil nil
                                           (car subdirs))))))
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
        result cell)
    (setq result (completing-read "Compile command: " lst))
    (when result
      (setq cell (assoc result lst))
      (setq proviso-compile-command (cdr cell))
      (message "Set compile command to %s" (car cell)))))

(defun proviso-compile (&optional arg)
  "Start the process of compilation, according to some settings.
ARG allows customizing behavior."
  (interactive "P")
  (proviso-compile--pre-compile)
  (let ((cmd (or (proviso-get (proviso-current-project) :compile-defun)
                 proviso-compile-command
                 #'proviso-compile-command-std))
        (filters (or (proviso-get (proviso-current-project)
                                  :compile-cmds-comint-filters)
                     proviso-compile-cmds-comint-filters)))
    (when (setq compile-command (funcall cmd arg))
      (let ((current-prefix-arg
             (if (seq-find (lambda (filter)
                             (string-match-p filter compile-command))
                           filters)
                 '(4) nil)))
        (call-interactively 'compile)))))

(defun proviso-recompile (&optional arg)
  "Start the process of re-compilation, according to some settings.
ARG allows customizing behavior."
  (interactive "P")
  (proviso-compile--pre-compile)
  (call-interactively 'recompile))

(defun proviso-choose-recompile ()
  "Re-execute a compile command selected from history."
  (interactive)
  (let ((cmd (ivy-read "Recompile: " compile-history
                       :caller 'proviso-choose-recompile)))
    (setq compile-command cmd)
    (proviso-compile--pre-compile)
    (compile compile-command)))

(global-set-key (kbd "\C-c RET") #'proviso-compile)
(global-set-key "\C-cm" #'proviso-recompile)
(global-set-key "\C-ck" #'kill-compilation)

;; compile errors
(defvar proviso-ignore-compile-error-functions
  '(
    proviso-check-compile-buffer-cmake-werror
    proviso-check-compile-buffer-boost-test-output
    )
  "List of functions to filter compile errors.
If any return true, the current compile error is ignored by
`proviso-compile-check-buffer-errors'.")

(defun proviso-check-compile-buffer-cmake-werror ()
  "Check the current compile buffer error, cmake style.
Returns non-nil to signify the error can be ignored."
  (save-match-data
    (looking-back "-W" (- (point) 2))))

(defun proviso-check-compile-buffer-boost-test-output ()
  "Check the current compile buffer error, boost style.
Returns non-nil to signify the error can be ignored."
  (and
   (save-match-data
     (looking-back "[Nn]o " (- (point) 3)))
   (save-match-data
     (looking-at "errors detected"))))

(defun proviso-compile-check-buffer-errors (buffer)
  "Check compile buffer BUFFER for compile warnings or errors."
  (with-current-buffer buffer
    (catch 'found
      (goto-char 1)
      (while (search-forward-regexp "\\([Ww]arning\\|[Ee]rror\\)" nil t)
        (goto-char (match-beginning 1))
        (unless
            (-any 'funcall proviso-ignore-compile-error-functions)
          (throw 'found t))
        (goto-char (match-end 1)))
      nil)))

(defun proviso-compile--pre-compile ()
  "Store state pre-compile to be used post-compile."
  (setq proviso-compile--window-visible
        (get-buffer-window "*compilation*" 'visible))
  (setq proviso-compile--window-count (count-windows))
  (setq proviso-compile--subtitle
        (format "\"%s\""
                (or (proviso-current-project-name)
                    "proviso"))))

(defun proviso-compile-should-delete-compile-window ()
  "Return non-nil if the compilation window should be deleted."
  (or (not (eq proviso-compile--window-count (count-windows)))
      (not proviso-compile--window-visible)))

(defun proviso-compile-dispose-buffer (buffer string)
  "Bury compilation buffer BUFFER if appropriate.
STRING describes how the process finished.  A precondition to
burying the buffer is whether or not compilation succeeded
without warnings or errors.  In addition,
\\[proviso-compile-should-delete-compile-window] must return
non-nil."
  (if (and
       (string-match "compilation" (buffer-name buffer))
       (string-match "finished" string)
       (not (proviso-compile-check-buffer-errors buffer)))
      (run-with-timer 2 nil
                      (lambda (buf)
                        (let ((win (get-buffer-window buf t)))
                          (bury-buffer buf)
                          (if (proviso-compile-should-delete-compile-window)
                              (delete-window win)
                            (switch-to-prev-buffer win 'kill))))
                      buffer)))

(defun proviso-compile-notify (buffer string)
  "Make a notification based on BUFFER finishing as described by STRING."
  (when (and (string-match-p "compilation" (buffer-name buffer))
             (string-match-p "finished" string))
    (let ((cmd
           (cond ((and (eq system-type 'darwin)
                       (executable-find "alerter"))
                  (list "alerter"
                        "-title" proviso-compile--subtitle
                        "-sender" "org.gnu.Emacs"
                        "-message" proviso-compile--notify-msg
                        "-timeout"
                        (format "%d" proviso-compile--notify-timeout)))
                 ((and (eq system-type 'windows-nt)
                       (executable-find "toast"))
                  (list "toast"
                        "-t" proviso-compile--subtitle
                        "-m" proviso-compile--notify-msg))
                 ((and (executable-find "notify-send"))
                  (unless proviso--compile-notify-send-daemon-checked
                    (when (executable-find "systemctl")
                      (unless (eq 0 (call-process "systemctl" nil nil nil "status" "--user" "xfce4-notifyd"))
                        (call-process "systemctl" nil nil nil "start" "--user" "xfce4-notifyd")))
                    (setq proviso--compile-notify-send-daemon-checked t))
                  (list "notify-send"
                        "-t" (format "%d" (* 1000 proviso-compile--notify-timeout))
                        "-i" "emacs"
                        (format "%s: %s"
                                proviso-compile--subtitle
                                proviso-compile--notify-msg)))
                 )))
      (when cmd
        (apply #'call-process (car cmd) nil nil nil (cdr cmd))))))

(add-hook 'compilation-finish-functions 'proviso-compile-dispose-buffer)
(add-hook 'compilation-finish-functions 'proviso-compile-notify)

(provide 'proviso-compile)
;;; proviso-compile.el ends here
