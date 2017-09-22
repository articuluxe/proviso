#!/bin/sh
":"; exec "$EMACSX" --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; -*-
;;; test_proviso-include-files.el --- test proviso include files
;; Copyright (C) 2017  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Thursday, March 30, 2017
;; Version: 1.0
;; Modified Time-stamp: <2017-09-22 08:15:02 dharms>
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
;; Proviso tests.
;;

;;; Code:
(load-file "test/proviso-test-common.el")
(require 'proviso)

(ert-deftest proviso-include-open-project-empty-dir ()
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
 (proviso-define-project \"c\" :initfun 'do-init)
")
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (proviso-name-p (proviso-get proviso-local-proj :project-name)))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (equal (proviso-get proviso-local-proj :include-files)
                  (list (concat base "a/b/c/"))))
      (should (equal (proviso-get proviso-local-proj :include-ff-files)
                     (list "."
                           (concat base "a/b/c"))))
      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-deftest proviso-include-open-project-absolute-dir ()
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
               '( (:name \"base\" :dir \"/home/\")
                  )))
 (proviso-define-project \"c\" :initfun 'do-init)
")
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (proviso-name-p (proviso-get proviso-local-proj :project-name)))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (equal (proviso-get proviso-local-proj :include-files)
                  (list "/home/")))
      (should (equal (proviso-get proviso-local-proj :include-ff-files)
                  (list "." "/home" (concat base "a/b/c"))))
      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-deftest proviso-include-open-project-relative-dir ()
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
               '( (:name \"base\" :dir \"d/\")
                  )))
 (proviso-define-project \"c\" :initfun 'do-init)
")
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (proviso-name-p (proviso-get proviso-local-proj :project-name)))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (equal (proviso-get proviso-local-proj :include-files)
                  (list (concat base "a/b/c/d/"))))
      (should (equal (proviso-get proviso-local-proj :include-ff-files)
                     (list "." (concat base "a/b/c/d")
                           (concat base "a/b/c"))))
      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-deftest proviso-include-open-project-no-def ()
  (proviso-test-reset-all)
  (let ((base (file-name-directory load-file-name))
        file-contents)
    (cl-letf (((symbol-function 'proviso--load-file)
               (lambda (_)
                 (proviso-eval-string file-contents))))
      ;; open file
      (setq file-contents "")
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (proviso-name-p (proviso-get proviso-local-proj :project-name)))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (equal (proviso-get proviso-local-proj :include-files)
                  (list (concat base "a/b/c/"))))
      (should (equal (proviso-get proviso-local-proj :include-ff-files)
                     (list "." (concat base "a/b/c")
                           )))
      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-deftest proviso-include-open-project-dirs-without-trailing-slashes ()
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
               '( (:name \"one\" :dir \"\")
                  (:name \"two\" :dir \"/home\")
                  (:name \"three\" :dir \"d\")
                  )))
 (proviso-define-project \"c\" :initfun 'do-init)
")
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (proviso-name-p (proviso-get proviso-local-proj :project-name)))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (equal (proviso-get proviso-local-proj :include-files)
                     (list
                      (concat base "a/b/c/d/")
                      "/home/"
                      (concat base "a/b/c/")
                      )))
      (should (equal (proviso-get proviso-local-proj :include-ff-files)
                     (list
                      "."
                      (concat base "a/b/c/d")
                      "/home"
                      (concat base "a/b/c")
                      )))
      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-deftest proviso-include-open-project-dirs-switch-projects ()
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
               '( (:name \"one\" :dir \"\")
                  (:name \"two\" :dir \"/home\")
                  (:name \"three\" :dir \"d\")
                  )))
 (proviso-define-project \"c\" :initfun 'do-init)
")
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (proviso-name-p (proviso-get proviso-local-proj :project-name)))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (equal (proviso-get proviso-local-proj :include-files)
                     (list
                      (concat base "a/b/c/d/")
                      "/home/"
                      (concat base "a/b/c/")
                      )))
      (should (equal (proviso-get proviso-local-proj :include-ff-files)
                     (list
                      "."
                      (concat base "a/b/c/d")
                      "/home"
                      (concat base "a/b/c")
                      )))
      (should (local-variable-p 'ff-search-directories (get-buffer "dfile1")))
      (should (equal ff-search-directories
                     (list "."
                           (concat base "a/b/c/d")
                           "/home"
                           (concat base "a/b/c"))))
      ;; open 2nd file, same project
      (find-file (concat base "a/b/c/d/dfile2"))
      (should (proviso-name-p (proviso-get proviso-local-proj :project-name)))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (local-variable-p 'ff-search-directories (get-buffer "dfile2")))
      (should (equal ff-search-directories
                     (list "."
                           (concat base "a/b/c/d")
                           "/home"
                           (concat base "a/b/c"))))
      ;; open 3rd file, new project
      (setq file-contents "
 (defun do-init (proj)
   (proviso-put proj :proj-alist
               '( (:name \"base\" :dir \"d2/\")
                  )))
 (proviso-define-project \"c2\" :initfun 'do-init)
")
      (should (not (proviso-name-p "c2")))
      (find-file (concat base "a/b/c2/d2/dfile3"))
      (should (proviso-name-p (proviso-get proviso-local-proj :project-name)))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c2/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c2"))
      (should (local-variable-p 'ff-search-directories (get-buffer "dfile3")))
      (should (equal ff-search-directories
                     (list "."
                           (concat base "a/b/c2/d2")
                           (concat base "a/b/c2"))))
      ;; switch back to initial buffer
      (switch-to-buffer "dfile1")
      (run-hooks 'post-command-hook)    ;simulate interactive use
      (should (proviso-name-p (proviso-get proviso-local-proj :project-name)))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (eq proviso-local-proj proviso-curr-proj))
      (should (local-variable-p 'ff-search-directories (get-buffer "dfile1")))
      (should (equal ff-search-directories
                     (list "."
                           (concat base "a/b/c/d")
                           "/home"
                           (concat base "a/b/c"))))

      ;; clean up buffers
      (kill-buffer "dfile1")
      (kill-buffer "dfile2")
      (kill-buffer "dfile3")
      )))

(ert-run-tests-batch-and-exit (car argv))

;;; test_proviso-include-files.el ends here
