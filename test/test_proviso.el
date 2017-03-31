#!/bin/sh
":"; exec "$EMACSX" --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; -*-
;;; test_proviso.el --- test projects
;; Copyright (C) 2016-2017  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Friday, December  9, 2016
;; Version: 1.0
;; Modified Time-stamp: <2017-03-31 08:37:14 dharms>
;; Modified by: Dan Harms
;; Keywords: projects test

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

;; tests
(ert-deftest proviso-compile-test()
  (let ((byte-compile-error-on-warn t))
    (should (byte-compile-file load-file-name))
    (delete-file (byte-compile-dest-file load-file-name) nil)))

(ert-deftest proviso-open-project-test ()
  (proviso-test-reset-all)
  (let ((base (file-name-directory load-file-name))
        file-contents)
    (cl-letf (((symbol-function 'proviso--load-file)
               (lambda (_)
                 (proviso-eval-string file-contents))))
      ;; open first file, init new project
      (setq file-contents "(proviso-define \"c\" :name \"c\")")
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
      ;; open 2nd file, same project
      (find-file (concat base "a/b/c/d/dfile2"))
      (should (equal proviso-path-alist
                     (cons (cons (concat base "a/b/c/") "c") nil)))
      (should (eq proviso-local-proj proviso-curr-proj))
      (should (string= (concat base "a/b/c/")
                       (proviso-get proviso-local-proj :root-dir)))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (eq (proviso-get proviso-local-proj :inited) t))
      ;; open 3rd file, new project
      (setq file-contents "(proviso-define \"c2\" :name \"c2\")")
      (should (not (proviso-name-p "c2")))
      (find-file (concat base "a/b/c2/d2/dfile3"))
      (should (proviso-name-p "c2"))
      (should (equal proviso-path-alist
                     (list (cons (concat base "a/b/c2/") "c2")
                           (cons (concat base "a/b/c/") "c"))))
      (should (eq proviso-local-proj proviso-curr-proj))
      (should (string= (concat base "a/b/c2/")
                       (proviso-get proviso-local-proj :root-dir)))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c2"))
      (should (eq (proviso-get proviso-local-proj :inited) t))
      ;; clean up buffers
      (kill-buffer "dfile1")
      (kill-buffer "dfile2")
      (kill-buffer "dfile3")
      )))



(ert-run-tests-batch-and-exit (car argv))

;;; test_proviso.el ends here
