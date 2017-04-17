#!/bin/sh
":"; exec "$EMACSX" --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; -*-
;;; test_proviso-tags.el --- test proviso tags functionality
;; Copyright (C) 2017  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Thursday, April 13, 2017
;; Version: 1.0
;; Modified Time-stamp: <2017-04-17 17:35:14 dharms>
;; Modified by: Dan Harms
;; Keywords: proviso project tags etags

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

(ert-deftest proviso-tags-test-tags ()
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
               '( (:name \"first\" :dir \"\")
                  (:name \"second\" :dir \"d/\")
                  (:name \"third\" :dir \"d2/\")
                  (:name \"fourth\" :dir \"/home/\")
                  )))
 (proviso-define \"c\" :initfun 'do-init)
")
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (proviso-name-p (proviso-get proviso-local-proj :project-name)))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (equal (proviso-get proviso-local-proj :tags-alist)
                     (list (concat base "a/b/c/" "\\(.*\\)$")
                           (concat base "a/b/c/.tags/fourth-tags")
                           (concat base "a/b/c/.tags/third-tags")
                           (concat base "a/b/c/.tags/second-tags")
                           (concat base "a/b/c/.tags/first-tags")
                                   )))

      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-deftest proviso-tags-test-empty-proviso ()
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
      (should (equal (proviso-get proviso-local-proj :tags-alist)
                     (list (concat base "a/b/c/" "\\(.*\\)$")
                           (concat base "a/b/c/.tags/c-tags")
                                   )))

      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

;; (ert-deftest proviso-tags-test-tags-dir-from-env-var ()
;;   (proviso-test-reset-all)
;;   (let ((base (file-name-directory load-file-name))
;;         file-contents)
;;     (cl-letf (((symbol-function 'proviso--load-file)
;;                (lambda (_)
;;                  (proviso-eval-string file-contents))))
;;       ;; open file
;;       (setq file-contents "
;;  (defun do-init (proj)
;;    (proviso-put proj :proj-alist
;;                '( (:name \"first\" :dir \"\")
;;                   (:name \"second\" :dir \"d/\")
;;                   (:name \"third\" :dir \"d2/\")
;;                   (:name \"fourth\" :dir \"/home/\")
;;                   )))
;;  (proviso-define \"c\" :initfun 'do-init)
;; ")
;;       (setenv "EMACS_TAGS_DIR" "~/.mytags_dir/")
;;       (find-file (concat base "a/b/c/d/dfile1"))
;;       (should (proviso-name-p (proviso-get proviso-local-proj :project-name)))
;;       (should (string= (proviso-get proviso-local-proj :root-dir)
;;                        (concat base "a/b/c/")))
;;       (should (string= (proviso-get proviso-local-proj :project-name)
;;                        "c"))
;;       ;; todo: this case only works remotely
;;       (should (equal (proviso-get proviso-local-proj :tags-alist)
;;                      (list (concat base "a/b/c/" "\\(.*\\)$")
;;                            "~/.mytags_dir/fourth-tags"
;;                            "~/.mytags_dir/third-tags"
;;                            "~/.mytags_dir/second-tags"
;;                            "~/.mytags_dir/first-tags"
;;                                    )))
;;       (setenv "EMACS_TAGS_DIR")
;;       ;; clean up buffers
;;       (kill-buffer "dfile1")
;;       )))

(ert-deftest proviso-tags-test-tags-dir-explicit-with-trailing-slash ()
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
               '( (:name \"first\" :dir \"\")
                  (:name \"second\" :dir \"d/\")
                  (:name \"third\" :dir \"d2/\")
                  (:name \"fourth\" :dir \"/home/\")
                  )))
 (proviso-define \"c\" :initfun 'do-init :tags-subdir \".mytags_subdir/\")
")
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (proviso-name-p (proviso-get proviso-local-proj :project-name)))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (equal (proviso-get proviso-local-proj :tags-alist)
                     (list (concat base "a/b/c/" "\\(.*\\)$")
                           (concat base "a/b/c/.mytags_subdir/fourth-tags")
                           (concat base "a/b/c/.mytags_subdir/third-tags")
                           (concat base "a/b/c/.mytags_subdir/second-tags")
                           (concat base "a/b/c/.mytags_subdir/first-tags")
                                   )))
      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-deftest proviso-tags-test-tags-dir-explicit-without-trailing-slash ()
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
               '( (:name \"first\" :dir \"\")
                  (:name \"second\" :dir \"d/\")
                  (:name \"third\" :dir \"d2/\")
                  (:name \"fourth\" :dir \"/home/\")
                  )))
 (proviso-define \"c\" :initfun 'do-init :tags-subdir \".mytags_subdir\")
")
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (proviso-name-p (proviso-get proviso-local-proj :project-name)))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (equal (proviso-get proviso-local-proj :tags-alist)
                     (list (concat base "a/b/c/" "\\(.*\\)$")
                           (concat base "a/b/c/.mytags_subdir/fourth-tags")
                           (concat base "a/b/c/.mytags_subdir/third-tags")
                           (concat base "a/b/c/.mytags_subdir/second-tags")
                           (concat base "a/b/c/.mytags_subdir/first-tags")
                                   )))
      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-deftest proviso-tags-test-tags ()
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
               '( (:name \"first\" :dir \"\")
                  (:name \"second\" :dir \"d/\")
                  (:name \"third\" :dir \"d2/\")
                  (:name \"fourth\" :dir \"/home/\")
                  )))
 (proviso-define \"c\" :initfun 'do-init)
")
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (proviso-name-p (proviso-get proviso-local-proj :project-name)))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (equal (proviso-get proviso-local-proj :tags-alist)
                     (list (concat base "a/b/c/" "\\(.*\\)$")
                           (concat base "a/b/c/.tags/fourth-tags")
                           (concat base "a/b/c/.tags/third-tags")
                           (concat base "a/b/c/.tags/second-tags")
                           (concat base "a/b/c/.tags/first-tags")
                                   )))
      ;; open 2nd file, same project
      (find-file (concat base "a/b/c/d/dfile2"))
      (should (proviso-name-p (proviso-get proviso-local-proj :project-name)))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (equal (proviso-get proviso-local-proj :tags-alist)
                     (list (concat base "a/b/c/" "\\(.*\\)$")
                           (concat base "a/b/c/.tags/fourth-tags")
                           (concat base "a/b/c/.tags/third-tags")
                           (concat base "a/b/c/.tags/second-tags")
                           (concat base "a/b/c/.tags/first-tags")
                                   )))
      ;; open 3rd file, new project
      (setq file-contents "
 (defun do-init (proj)
   (proviso-put proj :proj-alist
               '( (:name \"base\" :dir \"\")
                  (:name \"subdir\" :dir \"d2/\")
                  )))
 (proviso-define \"c2\" :initfun 'do-init)
")
      (should (not (proviso-name-p "c2")))
      (find-file (concat base "a/b/c2/d2/dfile3"))
      (should (proviso-name-p (proviso-get proviso-local-proj :project-name)))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c2/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c2"))
      (should (equal (proviso-get proviso-local-proj :tags-alist)
                     (list (concat base "a/b/c2/" "\\(.*\\)$")
                           (concat base "a/b/c2/.tags/subdir-tags")
                           (concat base "a/b/c2/.tags/base-tags")
                                   )))
      ;; switch back to initial buffer
      (switch-to-buffer "dfile1")
      (run-hooks 'post-command-hook)    ;simulate interactive use
      (should (proviso-name-p (proviso-get proviso-local-proj :project-name)))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (eq proviso-local-proj proviso-curr-proj))
      (should (equal (proviso-get proviso-local-proj :tags-alist)
                     (list (concat base "a/b/c/" "\\(.*\\)$")
                           (concat base "a/b/c/.tags/fourth-tags")
                           (concat base "a/b/c/.tags/third-tags")
                           (concat base "a/b/c/.tags/second-tags")
                           (concat base "a/b/c/.tags/first-tags")
                                   )))

      ;; clean up buffers
      (kill-buffer "dfile1")
      (kill-buffer "dfile2")
      (kill-buffer "dfile3")
      )))

(ert-run-tests-batch-and-exit (car argv))

;;; test_proviso-tags.el ends here
