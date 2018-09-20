;;; proviso-transfer.el --- helper to transfer a file
;; Copyright (C) 2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Monday, August 13, 2018
;; Version: 1.0
;; Modified Time-stamp: <2018-09-20 14:28:38 dan.harms>
;; Modified by: Dan Harms
;; Keywords: tools proviso project
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
;;  Helpers to transfer files, including remotely.
;;

;;; Code:
(require 'proviso-core)
(require 'dired-aux)
(require 'seq)
(require 'async)

(defvar proviso-transfer-rules-alist
  '(
    (:compress-exe "zip"
                   :uncompress-exe "unzip"
                   :transform (lambda (name)
                                (concat name ".zip"))
                   :compress-cmd "zip %o -r --filesync %i"
                   :uncompress-cmd "unzip -uo %i")
    (:compress-exe "gzip"
                   :uncompress-exe "gunzip"
                   :transform (lambda (name)
                                (concat name ".gz"))
                   :compress-cmd "gzip -c9 %i > %o"
                   :uncompress-cmd "gunzip -c9 %i > %o")
    ))

(defvar proviso-transfer-debug t
  "Controls the output of debugging info for `proviso-transfer'.")

(defun proviso-transfer-find-executable (exe &optional path)
  "Search for executable EXE given directory PATH.
If PATH is not supplied, `default-directory' is used."
  (let* ((default-directory (or path default-directory))
         (path default-directory)
         (func (if (file-remote-p path)
                   #'proviso-core-remote-executable-find
                 #'executable-find)))
    (funcall func exe)))

(defun proviso-transfer--test-compression-method (src-path dst-path method
                                                           &optional force)
  "Test SRC-PATH and DST-PATH for compression METHOD.
METHOD is a plist, see each element of `proviso-transfer-rules-alist'.
Optional FORCE specifies a compression method."
  (let ((compress (plist-get method :compress-exe))
        (uncompress (plist-get method :uncompress-exe)))
    (and (proviso-transfer-find-executable compress src-path)
         (proviso-transfer-find-executable uncompress dst-path)
         (or (not force) (string-equal force compress)))))

(defun proviso-transfer--find-compression-method (src dest rules
                                                      &optional force)
  "Return a valid compression method among RULES to use for SRC and DEST.
Optional FORCE specifies a compression method."
  (let ((method (seq-find (lambda (element)
                            (proviso-transfer--test-compression-method
                             src dest element force))
                          rules)))
    (when (and force (not method))      ;didn't find the override
      (setq method (seq-find (lambda (element)
                               (proviso-transfer--test-compression-method
                                src dest element))
                             rules)))
    method))

(defun proviso-transfer-file-async (src dest &optional force buffer)
  "Transfer SRC to DEST asynchronously.
Optional FORCE specifies a compression method.
If a non-nil BUFFER is supplied, insert message there."
  (interactive "fSource file: \nGDestination: \nsMethod: ")
  (let ((start (current-time))
        msg)
    (async-start
     `(lambda ()
        (setq inhibit-message t)
        ,(async-inject-variables "load-path")
        (require 'proviso-transfer)
        (proviso-transfer-file ,src ,dest ,force t))
     `(lambda (_)
        (setq msg
              (format "Transferred %s to %s in %.3f sec."
                      ,src ,dest
                      (float-time
                       (time-subtract
                        (current-time) (quote ,start)))))
        (if ,buffer
            (with-current-buffer ,buffer
              (insert msg))
          (message "%s" msg))))))

(defun proviso-transfer--should-compress (src dest)
  "Return non-nil if SRC should be compressed before copying to DEST."
  (or
   ;; TODO: check file size
   (file-remote-p src) (file-remote-p dest)))

(defun proviso-transfer-file (src dest &optional force quiet)
  "Transfer SRC to DEST.
Optional FORCE forces a compression method.
Optional QUIET will inhibit debugging output."
  (interactive "fSource file: \nGDestination: \nsMethod: ")
  (let* ((src-path (file-name-directory src))
         (src-file (file-name-nondirectory src))
         (dest-path (file-name-directory dest))
         (dest-file (file-name-nondirectory dest))
         (compress (proviso-transfer--should-compress src dest))
         (method (proviso-transfer--find-compression-method
                  src-path dest-path proviso-transfer-rules-alist force))
         (start (current-time))
         )
    (when (string-empty-p dest-file)
      (setq dest-file src-file))
    (make-directory dest-path t)
    (if (and compress method)
        (progn
          (setq src-file (proviso-transfer-compress-file src-path src-file dest-file method))
          (rename-file
           (expand-file-name src-file src-path)
           (expand-file-name src-file dest-path)
           t)
          (proviso-transfer-uncompress-file dest-path src-file dest-file method))
      (copy-file src dest t t t t))
    (and proviso-transfer-debug
         (not quiet)
         (message "Transferred %s to %s via %s in %.3f sec."
                  (expand-file-name src-file src-path)
                  (expand-file-name dest-file dest-path)
                  (if (and method compress)
                      (plist-get method :compress-exe)
                    "standard copy")
                  (float-time (time-subtract (current-time) start))))
    ))

(defun proviso-transfer-compress-file (path src dst method)
  "At PATH, compress SRC into DST using METHOD.
METHOD's format is a plist according to `proviso-transfer-rules-alist'."
  (let* ((default-directory path)
         (output (funcall (plist-get method :transform) dst))
         (cmd (format-spec (plist-get method :compress-cmd)
                           `((?\i . ,src)
                             (?\o . ,output))))
         return)
    (setq return (shell-command cmd))
    (when proviso-transfer-debug (message "proviso-transfer: %s (result:%d)"
                                          cmd return))
    output))

(defun proviso-transfer-uncompress-file (path src dst method)
  "At PATH, uncompress SRC to DST using METHOD.
METHOD's format is a plist according to `proviso-transfer-rules-alist'."
  (let ((default-directory path)
        (cmd (format-spec (plist-get method :uncompress-cmd)
                          `((?\i . ,src)
                            (?\o . ,dst))))
        return)
    (setq return (shell-command cmd))
    (when proviso-transfer-debug (message "proviso-transfer: %s (result:%d)"
                                          cmd return))
    (delete-file src)))

(provide 'proviso-transfer)
;;; proviso-transfer.el ends here
