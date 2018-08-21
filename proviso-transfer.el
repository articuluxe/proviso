;;; proviso-transfer.el --- helper to transfer a file
;; Copyright (C) 2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Monday, August 13, 2018
;; Version: 1.0
;; Modified Time-stamp: <2018-08-21 17:29:40 dharms>
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

(defun proviso-transfer-find-executable (path exe)
  "Search for executable EXE given directory PATH."
  (let ((default-directory path)
        (func (if (file-remote-p path)
                  #'proviso-core-remote-executable-find
                #'executable-find)))
    (funcall func exe)))

(defun proviso-transfer--test-compression-method (src-path dst-path method)
  "Test SRC-PATH and DST-PATH for compression METHOD.
METHOD is a plist, see each element of `proviso-transfer-rules-alist'."
  (let ((compress (plist-get method :compress-exe))
        (uncompress (plist-get method :uncompress-exe)))
    (and (proviso-transfer-find-executable src-path compress)
         (proviso-transfer-find-executable dst-path uncompress))))

(defun proviso-transfer--find-compression-method (src dest rules)
  "Return a valid compression method among RULES to use for SRC and DEST."
  (let ((method (seq-find (lambda (element)
                            (proviso-transfer--test-compression-method
                             src dest element))
                          rules)))
    method))

(defun proviso-transfer-file (src dest)
  "Transfer SRC to DEST."
  (interactive "fSource file: \nGDestination: ")
  ;; (message "Src %s Dest %s" src dest)
  (let* ((src-path (file-name-directory src))
         (src-file (file-name-nondirectory src))
         (dest-path (file-name-directory dest))
         (dest-file (file-name-nondirectory dest))
                                        ;(compress (or (file-remote-p src) (file-remote-p dest)))
         (compress t)
         (method (proviso-transfer--find-compression-method
                  src-path dest-path proviso-transfer-rules-alist))
         )
    (when (string-empty-p dest-file)
      (setq dest-file src-file))
    ;; (message "src-path %s src-file %s dst-path %s dst-file %s"
    ;;          src-path src-file dest-path dest-file)
    ;; (message "exe %s cmd %s transform %s" (plist-get method :compress-exe)
    ;;          (plist-get method :compress-cmd) (plist-get method :transform))
    (if (and compress method)
        (progn
          (setq src-file (proviso-transfer-compress-file src-path src-file dest-file method))
          (rename-file
           (expand-file-name src-file src-path)
           (expand-file-name src-file dest-path)
           t)
          (proviso-transfer-uncompress-file dest-path src-file dest-file method))
      (copy-file src dest t t t t))
    ))

(defun proviso-transfer-compress-file (path src dst method)
  "At PATH, compress SRC into DST using METHOD.
METHOD's format is a plist according to `proviso-transfer-rules-alist'."
  (let ((default-directory path)
        (output (funcall (plist-get method :transform) dst)))
    ;; (message "Compressing %s to %s" src output)
    (dired-shell-command
     (format-spec (plist-get method :compress-cmd)
                  `((?\i . ,src)
                    (?\o . ,output))))
    output))

(defun proviso-transfer-uncompress-file (path src dst method)
  "At PATH, uncompress SRC to DST using METHOD.
METHOD's format is a plist according to `proviso-transfer-rules-alist'."
  (let ((default-directory path))
    ;; (message "Uncompressing %s to %s" src dst)
    (dired-shell-command
     (format-spec (plist-get method :uncompress-cmd)
                  `((?\i . ,src)
                    (?\o . ,dst))))
    (delete-file src)))

(provide 'proviso-transfer)
;;; proviso-transfer.el ends here
