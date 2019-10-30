#!/bin/sh
":"; exec "$VISUAL" --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; -*-
;;; test_proviso-gud.el --- test proviso gud functionality
;; Copyright (C) 2018-2019  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Thursday, February  1, 2018
;; Version: 1.0
;; Modified Time-stamp: <2019-10-30 08:59:09 dharms>
;; Modified by: Dan Harms
;; Keywords: tools proviso gud debugger test
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

;;

;;; Code:
(load-file "test/proviso-test-common.el")
(require 'proviso)

(ert-deftest proviso-gud-open-empty-project ()
  (proviso-test-reset-all)
  (let ((base (file-name-directory load-file-name))
        file-contents)
    (cl-letf (((symbol-function 'proviso--eval-file)
               (lambda (_)
                 (unless (string-empty-p (string-trim file-contents))
                   (car (read-from-string file-contents))))))
      ;; open file
      (setq file-contents "(
:initfun (lambda (proj)
   (proviso-put proj :proj-alist
               '( (:name \"base\" :dir \"\")
                  )))
)")
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (equal (proviso-gud-gather-debug-dirs proviso-local-proj)
                     nil))
      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-deftest proviso-gud-open-project ()
  (proviso-test-reset-all)
  (let ((base (file-name-directory load-file-name))
        file-contents)
    (cl-letf (((symbol-function 'proviso--eval-file)
               (lambda (_)
                 (unless (string-empty-p (string-trim file-contents))
                   (car (read-from-string file-contents))))))
      ;; open file
      (setq file-contents "(
:initfun (lambda (proj)
   (proviso-put proj :proj-alist
               '( (:name \"base\" :dir \"\")
                  ))
   (proviso-put proj :debug-subdirs
               '( (:name \"DBG\" :dir \"d2\")
                  (:name \"D2\" :dir \"d\")
                ))
   (proviso-put proj :build-subdirs
               '( (:name \"DBG\" :dir \"d3\")
                ))
   )
)")
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (equal (proviso-gud-gather-debug-dirs proviso-local-proj)
                     (list (cons (concat base "a/b/c/d2")
                                 (concat base "a/b/c/d2"))
                           (cons (concat base "a/b/c/d")
                                 (concat base "a/b/c/d")))))
      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-run-tests-batch-and-exit (car argv))

;;; test_proviso-gud.el ends here
