#!/bin/sh
":"; exec "$EMACSX" --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; -*-
;;; test_proviso-registers.el --- test proviso registers
;; Copyright (C) 2017  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Tuesday, April  4, 2017
;; Version: 1.0
;; Modified Time-stamp: <2017-04-17 17:41:54 dharms>
;; Modified by: Dan Harms
;; Keywords: proviso project registers test

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

(defun proviso-register-reset-registers()
  (set-register ?c nil)
  (set-register ?r nil)
  (set-register ?1 nil)
  (set-register ?2 nil)
  )

(ert-deftest proviso-register-test-root-register ()
  (proviso-test-reset-all)
  (proviso-register-reset-registers)
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
 (proviso-define \"c\" :initfun 'do-init)
")
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (proviso-name-p (proviso-get proviso-local-proj :project-name)))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (equal (get-register ?r) (cons 'file (concat base "a/b/c/"))))
      (should (equal (get-register ?c) (cons 'file (concat base "a/b/c/"))))
      (should (equal (get-register ?1) nil))
      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-deftest proviso-register-test-empty-dir ()
  (proviso-test-reset-all)
  (proviso-register-reset-registers)
  (let ((base (file-name-directory load-file-name))
        file-contents)
    (cl-letf (((symbol-function 'proviso--load-file)
               (lambda (_)
                 (proviso-eval-string file-contents))))
      ;; open file
      (setq file-contents "
 (defun do-init (proj)
   (proviso-put proj :proj-alist
               '( (:name \"base\" :dir \"\" :register ?1)
                  )))
 (proviso-define \"c\" :initfun 'do-init)
")
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (proviso-name-p (proviso-get proviso-local-proj :project-name)))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (equal (get-register ?r) (cons 'file (concat base "a/b/c/"))))
      (should (equal (get-register ?c) (cons 'file (concat base "a/b/c/"))))
      (should (equal (get-register ?1) (cons 'file (concat base "a/b/c/"))))
      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-deftest proviso-register-test-relative-dir ()
  (proviso-test-reset-all)
  (proviso-register-reset-registers)
  (let ((base (file-name-directory load-file-name))
        file-contents)
    (cl-letf (((symbol-function 'proviso--load-file)
               (lambda (_)
                 (proviso-eval-string file-contents))))
      ;; open file
      (setq file-contents "
 (defun do-init (proj)
   (proviso-put proj :proj-alist
               '( (:name \"base\" :dir \"d/\" :register ?1)
                  )))
 (proviso-define \"c\" :initfun 'do-init)
")
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (proviso-name-p (proviso-get proviso-local-proj :project-name)))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (equal (get-register ?r) (cons 'file (concat base "a/b/c/"))))
      (should (equal (get-register ?c) (cons 'file (concat base "a/b/c/d/"))))
      (should (equal (get-register ?1) (cons 'file (concat base "a/b/c/d/"))))
      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-deftest proviso-register-test-absolute-dir ()
  (proviso-test-reset-all)
  (proviso-register-reset-registers)
  (let ((base (file-name-directory load-file-name))
        file-contents)
    (cl-letf (((symbol-function 'proviso--load-file)
               (lambda (_)
                 (proviso-eval-string file-contents))))
      ;; open file
      (setq file-contents "
 (defun do-init (proj)
   (proviso-put proj :proj-alist
               '( (:name \"base\" :dir \"/home/\" :register ?1)
                  )))
 (proviso-define \"c\" :initfun 'do-init)
")
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (proviso-name-p (proviso-get proviso-local-proj :project-name)))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (equal (get-register ?r) (cons 'file (concat base "a/b/c/"))))
      (should (equal (get-register ?c) (cons 'file "/home/")))
      (should (equal (get-register ?1) (cons 'file "/home/")))
      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-deftest proviso-register-test-build-dirs-relative ()
  (proviso-test-reset-all)
  (proviso-register-reset-registers)
  (let ((base (file-name-directory load-file-name))
        file-contents)
    (cl-letf (((symbol-function 'proviso--load-file)
               (lambda (_)
                 (proviso-eval-string file-contents))))
      ;; open file
      (setq file-contents "
 (defun do-init (proj)
   (proviso-put proj :proj-alist
               '( (:name \"base\" :dir \"/home/\" :register ?1)
                  ))
   (proviso-put proj :build-subdirs
               '( (:name \"subdir\" :dir \"d2/\" :register ?2)
                  )))
 (proviso-define \"c\" :initfun 'do-init)
")
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (proviso-name-p (proviso-get proviso-local-proj :project-name)))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (equal (get-register ?r) (cons 'file (concat base "a/b/c/"))))
      (should (equal (get-register ?c) (cons 'file "/home/")))
      (should (equal (get-register ?1) (cons 'file "/home/")))
      (should (equal (get-register ?2) (cons 'file (concat base "a/b/c/d2/"))))
      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-deftest proviso-register-test-build-dirs-absolute ()
  (proviso-test-reset-all)
  (proviso-register-reset-registers)
  (let ((base (file-name-directory load-file-name))
        file-contents)
    (cl-letf (((symbol-function 'proviso--load-file)
               (lambda (_)
                 (proviso-eval-string file-contents))))
      ;; open file
      (setq file-contents "
 (defun do-init (proj)
   (proviso-put proj :proj-alist
               '( (:name \"base\" :dir \"/home/\" :register ?1)
                  ))
   (proviso-put proj :build-subdirs
               '( (:name \"subdir\" :dir \"/home/\" :register ?2)
                  )))
 (proviso-define \"c\" :initfun 'do-init)
")
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (proviso-name-p (proviso-get proviso-local-proj :project-name)))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (equal (get-register ?r) (cons 'file (concat base "a/b/c/"))))
      (should (equal (get-register ?c) (cons 'file "/home/")))
      (should (equal (get-register ?1) (cons 'file "/home/")))
      (should (equal (get-register ?2) (cons 'file "/home/")))
      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-deftest proviso-register-test-build-dirs-empty-dir ()
  (proviso-test-reset-all)
  (proviso-register-reset-registers)
  (let ((base (file-name-directory load-file-name))
        file-contents)
    (cl-letf (((symbol-function 'proviso--load-file)
               (lambda (_)
                 (proviso-eval-string file-contents))))
      ;; open file
      (setq file-contents "
 (defun do-init (proj)
   (proviso-put proj :proj-alist
               '( (:name \"base\" :dir \"/home/\" :register ?1)
                  ))
   (proviso-put proj :build-subdirs
               '( (:name \"subdir\" :dir \"\" :register ?2)
                  )))
 (proviso-define \"c\" :initfun 'do-init)
")
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (proviso-name-p (proviso-get proviso-local-proj :project-name)))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (equal (get-register ?r) (cons 'file (concat base "a/b/c/"))))
      (should (equal (get-register ?c) (cons 'file "/home/")))
      (should (equal (get-register ?1) (cons 'file "/home/")))
      (should (equal (get-register ?2) (cons 'file (concat base "a/b/c/"))))
      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-deftest proviso-register-test-no-project-file ()
  (proviso-test-reset-all)
  (proviso-register-reset-registers)
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
      (should (equal (get-register ?r) (cons 'file (concat base "a/b/c/"))))
      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-deftest proviso-register-test-switch-projects ()
  (proviso-test-reset-all)
  (proviso-register-reset-registers)
  (let ((base (file-name-directory load-file-name))
        file-contents)
    (cl-letf (((symbol-function 'proviso--load-file)
               (lambda (_)
                 (proviso-eval-string file-contents))))
      ;; open file
      (setq file-contents "
 (defun do-init (proj)
   (proviso-put proj :proj-alist
               '( (:name \"base\" :dir \"/home/\" :register ?1)
                  ))
   (proviso-put proj :build-subdirs
               '( (:name \"subdir\" :dir \"d2/\" :register ?2)
                  )))
 (proviso-define \"c\" :initfun 'do-init)
")
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (proviso-name-p (proviso-get proviso-local-proj :project-name)))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (equal (get-register ?r) (cons 'file (concat base "a/b/c/"))))
      (should (equal (get-register ?c) (cons 'file "/home/")))
      (should (equal (get-register ?1) (cons 'file "/home/")))
      (should (equal (get-register ?2) (cons 'file (concat base "a/b/c/d2/"))))
      ;; open 2nd file, same project
      (find-file (concat base "a/b/c/d/dfile2"))
      (should (proviso-name-p (proviso-get proviso-local-proj :project-name)))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (equal (get-register ?r) (cons 'file (concat base "a/b/c/"))))
      (should (equal (get-register ?c) (cons 'file "/home/")))
      (should (equal (get-register ?1) (cons 'file "/home/")))
      (should (equal (get-register ?2) (cons 'file (concat base "a/b/c/d2/"))))
      ;; open 3rd file, new project
      (setq file-contents "
 (defun do-init (proj)
   (proviso-put proj :proj-alist
               '( (:name \"base\" :dir \"\" :register ?1)
                  ))
   (proviso-put proj :build-subdirs
               '( (:name \"subdir\" :dir \"d2/\" :register ?2)
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
      (should (equal (get-register ?r) (cons 'file (concat base "a/b/c2/"))))
      (should (equal (get-register ?c) (cons 'file (concat base "a/b/c2/"))))
      (should (equal (get-register ?1) (cons 'file (concat base "a/b/c2/"))))
      (should (equal (get-register ?2) (cons 'file (concat base "a/b/c2/d2/"))))
      ;; switch back to initial buffer
      (switch-to-buffer "dfile1")
      (run-hooks 'post-command-hook)    ;simulate interactive use
      (should (proviso-name-p (proviso-get proviso-local-proj :project-name)))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (eq proviso-local-proj proviso-curr-proj))
      (should (equal (get-register ?r) (cons 'file (concat base "a/b/c/"))))
      (should (equal (get-register ?c) (cons 'file "/home/")))
      (should (equal (get-register ?1) (cons 'file "/home/")))
      (should (equal (get-register ?2) (cons 'file (concat base "a/b/c/d2/"))))
      ;; clean up buffers
      (kill-buffer "dfile1")
      (kill-buffer "dfile2")
      (kill-buffer "dfile3")
      )))


(ert-run-tests-batch-and-exit (car argv))

;;; test_proviso-registers.el ends here
