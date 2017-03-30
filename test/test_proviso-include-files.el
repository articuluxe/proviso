#!/bin/sh
":"; exec "$EMACSX" --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; -*-
;;; test_proviso-include-files.el --- test proviso include files
;; Copyright (C) 2017  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Thursday, March 30, 2017
;; Version: 1.0
;; Modified Time-stamp: <2017-03-30 17:29:11 dharms>
;; Modified by: Dan Harms
;; Keywords: proviso project include files test

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
;; transfer dependencies from argv into load-path
(let ((lst (cdr argv))
      add elt)
  (setq argv nil)
  (while lst
    (setq elt (car lst))
    (if add
        (progn
          (push elt load-path)
          (setq add nil))
      (unless
          (setq add (string= elt "-L"))
        (push elt argv)))
      (setq lst (cdr lst))))
(push (concat (file-name-directory load-file-name) "/..") load-path)
(push (file-name-directory load-file-name) load-path)


(require 'ert)
(require 'proviso)

;; helper defun
(defun proviso-test-reset-all ()
  "Reset all profile-related data structures to nil."
  (setq proviso-obarray (make-vector 7 0))
  (intern "default" proviso-obarray)
  (setq proviso-path-alist '())
  (setq proviso-curr-proj nil)
  (setq proviso-local-proj (default-value 'proviso-local-proj))
  )

(ert-deftest proviso-open-project-naming ()
  (proviso-test-reset-all)
  (let ((base (file-name-directory load-file-name))
        file-contents)
    (cl-letf (((symbol-function 'proviso--load-file)
               (lambda (_)
                 (proviso-eval-string file-contents))))
      ;; open file
      (setq file-contents "
(defun do-init (proj)
  (proviso-put proj :proj-alist
               '( (:name \"base\" :dir \"\")
                  )))
(proviso-define \"c\" :name \"c\" :initfun 'do-init)
")
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (proviso-name-p (proviso-get proviso-local-proj :project-name)))
      (should (equal proviso-path-alist
                     (cons (cons (concat base "a/b/c/") "c") nil)))
      (should (eq proviso-local-proj proviso-curr-proj))
      (should (eq (proviso-get proviso-local-proj :inited) t))
      (should (string= (concat base "a/b/c/")
                       (proviso-get proviso-local-proj :root-dir)))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (eq (proviso-get proviso-local-proj :inited) t))
      (should (equal (proviso-get proviso-local-proj :include-files)
                  (list (concat base "a/b/c/"))))
      ;; clean up buffers
      (kill-buffer "dfile1")
      )))


(ert-run-tests-batch-and-exit (car argv))

;;; test_proviso-include-files.el ends here
