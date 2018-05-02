#!/bin/sh
":"; exec "$VISUAL" --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; -*-
;;; test_proviso-bookmarks.el --- test proviso bookmarks
;; Copyright (C) 2017-2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Tuesday, April 18, 2017
;; Version: 1.0
;; Modified Time-stamp: <2018-05-02 17:41:14 dharms>
;; Modified by: Dan Harms
;; Keywords: tools proviso bookmark test

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

(ert-deftest proviso-register-test-bookmark-created ()
  (proviso-test-reset-all)
  (let ((base (file-name-directory load-file-name))
        (proviso-bookmarks-create-bmk-on-proj-init t)
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
      (ignore-errors
        (f-delete (concat base "a/b/c/c.bmk") t))
      (should (not (f-exists? (concat base "a/b/c/c.bmk"))))
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (proviso-name-p (proviso-get proviso-local-proj :project-name)))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (f-exists? (concat base "a/b/c/c.bmk")))
      (should (string= bmkp-current-bookmark-file
                       (abbreviate-file-name (concat base "a/b/c/c.bmk"))))
      ;; clean up buffers
      (kill-buffer "dfile1")
      (f-delete (concat base "a/b/c/c.bmk"))
      )))

(ert-deftest proviso-register-test-bookmark-empty-project-file ()
  (proviso-test-reset-all)
  (let ((base (file-name-directory load-file-name))
        (proviso-bookmarks-create-bmk-on-proj-init t)
        file-contents)
    (cl-letf (((symbol-function 'proviso--load-file)
               (lambda (_)
                 (proviso-eval-string file-contents))))
      ;; open file
      (setq file-contents "")
      (ignore-errors
        (f-delete (concat base "a/b/c/c.bmk") t))
      (should (not (f-exists? (concat base "a/b/c/c.bmk"))))
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (proviso-name-p (proviso-get proviso-local-proj :project-name)))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (f-exists? (concat base "a/b/c/c.bmk")))
      (should (string= bmkp-current-bookmark-file
                       (abbreviate-file-name (concat base "a/b/c/c.bmk"))))
      ;; clean up buffers
      (kill-buffer "dfile1")
      (f-delete (concat base "a/b/c/c.bmk"))
      )))

(ert-deftest proviso-register-test-bookmark-switch-projects ()
  (proviso-test-reset-all)
  (let ((base (file-name-directory load-file-name))
        (proviso-bookmarks-create-bmk-on-proj-init t)
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
      (ignore-errors
        (f-delete (concat base "a/b/c/c.bmk") t))
      (should (not (f-exists? (concat base "a/b/c/c.bmk"))))
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (proviso-name-p (proviso-get proviso-local-proj :project-name)))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (f-exists? (concat base "a/b/c/c.bmk")))
      (should (string= bmkp-current-bookmark-file
                       (abbreviate-file-name (concat base "a/b/c/c.bmk"))))
      ;; open 2nd file, same project
      (find-file (concat base "a/b/c/d/dfile2"))
      (should (proviso-name-p (proviso-get proviso-local-proj :project-name)))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (f-exists? (concat base "a/b/c/c.bmk")))
      (should (string= bmkp-current-bookmark-file
                       (abbreviate-file-name (concat base "a/b/c/c.bmk"))))
      ;; open 3rd file, new project
      (setq file-contents "
 (defun do-init (proj)
   (proviso-put proj :proj-alist
               '( (:name \"base\" :dir \"\" :register ?1)
                  ))
   (proviso-put proj :build-subdirs
               '( (:name \"subdir\" :dir \"d2/\" :register ?2)
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
      (should (f-exists? (concat base "a/b/c/c.bmk")))
      (should (f-exists? (concat base "a/b/c2/c2.bmk")))
      (should (string= bmkp-current-bookmark-file
                       (abbreviate-file-name (concat base "a/b/c2/c2.bmk"))))
      ;; switch back to initial buffer
      (switch-to-buffer "dfile1")
      (run-hooks 'post-command-hook)    ;simulate interactive use
      (should (proviso-name-p (proviso-get proviso-local-proj :project-name)))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (eq proviso-local-proj proviso-curr-proj))
      (should (f-exists? (concat base "a/b/c/c.bmk")))
      (should (f-exists? (concat base "a/b/c2/c2.bmk")))
      (should (string= bmkp-current-bookmark-file
                       (abbreviate-file-name (concat base "a/b/c/c.bmk"))))

      ;; clean up buffers
      (kill-buffer "dfile1")
      (kill-buffer "dfile2")
      (kill-buffer "dfile3")
      (f-delete (concat base "a/b/c/c.bmk"))
      (f-delete (concat base "a/b/c2/c2.bmk"))
      )))

(ert-run-tests-batch-and-exit (car argv))

;;; test_proviso-bookmarks.el ends here
