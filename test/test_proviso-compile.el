#!/bin/sh
":"; exec "$VISUAL" --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; -*-
;;; test_proviso-compile.el --- test proviso compile
;; Copyright (C) 2017-2019  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Thursday, May 25, 2017
;; Version: 1.0
;; Modified Time-stamp: <2019-07-29 06:52:57 dharms>
;; Modified by: Dan Harms
;; Keywords: tools proviso test compile

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

(ert-deftest proviso-compile-test-with-and-without-project ()
  (proviso-test-reset-all)
  (let* ((base (file-name-directory load-file-name))
         (default-directory base)
         file-contents read-result read-index)
    (cl-letf (((symbol-function 'proviso--eval-file)
               (lambda (_)
                 (unless (string-empty-p (string-trim file-contents))
                   (car (read-from-string file-contents)))))
              ((symbol-function 'completing-read)
               (lambda (_ collection &optional _3 _4 _5 _6 _7 _8) (interactive)
                 (seq-elt collection read-index)))
              ((symbol-function 'read-directory-name)
               (lambda (_ &optional _2 _3 _4 _5)
                 read-result))
              ((symbol-function 'proviso--query-error)
               (lambda (_ err)
                 (message "proviso-query-error: %s" err))))
      (should (string= (proviso-compile-command-std)
                       "cd ./ && make"))
      (setq read-result (concat base "a/"))
      (should (string= (proviso-compile-command-std '(4))
                       (concat "cd " base "a/ && make")))
      ;; open file
      (setq file-contents "(
:initfun
(lambda (proj)
   (proviso-put proj :proj-alist
               '( (:name \"base\" :dir \"d/e/\")
                  ))
   (proviso-put proj :build-subdirs
                '( (:name \"bld\" :dir \"d2/\")))
  )
)")
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (string= (proviso-compile-command-std)
                       (concat "cd " base "a/b/c/d2/ && make")))
      (setq read-result (concat base "a/b/c2/"))
      (should (string= (proviso-compile-command-std '(4))
                       (concat "cd " base "a/b/c2/ && make")))
      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-deftest proviso-compile-test-different-compile-sub-cmd ()
  (proviso-test-reset-all)
  (let* ((base (file-name-directory load-file-name))
         (default-directory base)
         file-contents read-result read-index)
    (cl-letf (((symbol-function 'proviso--eval-file)
               (lambda (_)
                 (unless (string-empty-p (string-trim file-contents))
                   (car (read-from-string file-contents)))))
              ((symbol-function 'completing-read)
               (lambda (_ collection &optional _3 _4 _5 _6 _7 _8) (interactive)
                 (seq-elt collection read-index)))
              ((symbol-function 'read-directory-name)
               (lambda (_ &optional _2 _3 _4 _5)
                 read-result))
              ((symbol-function 'proviso--query-error)
               (lambda (_ err)
                 (message "proviso-query-error: %s" err))))
      ;; open file
      (setq file-contents "(
:initfun
 (lambda (proj)
   (proviso-put proj :proj-alist
               '( (:name \"base\" :dir \"d/e/\")
                  ))
   (proviso-put proj :build-subdirs
                '( (:name \"bld\" :dir \"d2/\"))))
:compile-cmds (\"nmake -n \")
)")
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (string= (proviso-compile-command-std)
                       (concat "cd " base "a/b/c/d2/ && nmake -n ")))
      (setq read-result (concat base "a/b/c2/"))
      (should (string= (proviso-compile-command-std '(4))
                       (concat "cd " base "a/b/c2/ && nmake -n ")))
      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-deftest proviso-compile-test-with-2-subdirs ()
  (proviso-test-reset-all)
  (let* ((base (file-name-directory load-file-name))
         (default-directory base)
         file-contents read-result read-index)
    (cl-letf (((symbol-function 'proviso--eval-file)
               (lambda (_)
                 (unless (string-empty-p (string-trim file-contents))
                   (car (read-from-string file-contents)))))
              ((symbol-function 'completing-read)
               (lambda (_ collection &optional _3 _4 _5 _6 _7 _8) (interactive)
                 (seq-elt collection read-index)))
              ((symbol-function 'read-directory-name)
               (lambda (_ &optional _2 _3 _4 _5)
                 read-result))
              ((symbol-function 'proviso--query-error)
               (lambda (_ err)
                 (message "proviso-query-error: %s" err))))
      ;; open file
      (setq file-contents "(
:initfun
(lambda (proj)
   (proviso-put proj :proj-alist
               '( (:name \"base\" :dir \"d/e/\")
                ))
   (proviso-put proj :build-subdirs
                '( (:name \"bld\" :dir \"d2/\")
                   (:name \"two\" :dir \"d/e/f\")
                 ))
  )
)")
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (setq read-index 0)               ;picks empty choice, which is root dir
      (should (string= (proviso-compile-command-std)
                       (concat "cd " base "a/b/c/ && make")))
      (setq read-index 1)               ;picks 1st choice
      (should (string= (proviso-compile-command-std)
                       (concat "cd " base "a/b/c/d2/ && make")))
      (setq read-index 2)               ;picks 2nd choice
      (should (string= (proviso-compile-command-std)
                       (concat "cd " base "a/b/c/d/e/f/ && make")))
      (setq read-result (concat base "a/b/c2/"))
      (should (string= (proviso-compile-command-std '(4))
                       (concat "cd " base "a/b/c2/ && make")))
      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-deftest proviso-compile-test-with-repo ()
  (proviso-test-reset-all)
  (let* ((base (file-name-directory load-file-name))
         (default-directory base)
         file-contents read-result read-index)
    (cl-letf (((symbol-function 'proviso--eval-file)
               (lambda (_)
                 (unless (string-empty-p (string-trim file-contents))
                   (car (read-from-string file-contents)))))
              ((symbol-function 'completing-read)
               (lambda (_ collection &optional _3 _4 _5 _6 _7 _8) (interactive)
                 (seq-elt collection read-index)))
              ((symbol-function 'read-directory-name)
               (lambda (_ &optional _2 _3 _4 _5)
                 read-result))
              ((symbol-function 'proviso--query-error)
               (lambda (_ err)
                 (message "proviso-query-error: %s" err))))
      ;; open file
      (setq file-contents "(
:initfun
(lambda (proj)
   (proviso-put proj :proj-alist
               '( (:name \"base\" :dir \"d/e/\")
                ))
   (proviso-put proj :build-subdirs
                '( (:name \"bld\" :dir \"d2/\")
                   (:name \"two\" :dir \"d/e/f\")
                 ))
  )
)")
      (setq proviso-compile-command 'proviso-compile-command-repo)
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (setq read-index 0)               ;picks empty choice, which is root dir
      (should (string= (proviso-compile-command-repo)
                       (concat ". " base "a/b/c/repo-setup.sh && cd " base "a/b/c/ && make")))
      (setq read-index 1)               ;picks 1st choice
      (should (string= (proviso-compile-command-repo)
                       (concat ". " base "a/b/c/repo-setup.sh && cd " base "a/b/c/d2/ && make")))
      (setq read-index 2)               ;picks 2nd choice
      (should (string= (proviso-compile-command-repo)
                       (concat ". " base "a/b/c/repo-setup.sh && cd " base "a/b/c/d/e/f/ && make")))
      (setq read-result (concat base "a/b/c2/"))
      (should (string= (proviso-compile-command-repo '(4))
                       (concat ". " base "a/b/c/repo-setup.sh && cd " base "a/b/c2/ && make")))
      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-deftest proviso-compile-test-real-compile-with-repo ()
  (proviso-test-reset-all)
  (let* ((base (file-name-directory load-file-name))
         (default-directory base)
         (compilation-read-command nil)
         (compilation-always-kill t)
         file-contents read-result read-index)
    (cl-letf (((symbol-function 'proviso--eval-file)
               (lambda (_)
                 (unless (string-empty-p (string-trim file-contents))
                   (car (read-from-string file-contents)))))
              ((symbol-function 'completing-read)
               (lambda (_ collection &optional _3 _4 _5 _6 _7 _8) (interactive)
                 (seq-elt collection read-index)))
              ((symbol-function 'read-directory-name)
               (lambda (_ &optional _2 _3 _4 _5)
                 read-result))
              ((symbol-function 'proviso--query-error)
               (lambda (_ err)
                 (message "proviso-query-error: %s" err))))
      ;; open file
      (setq file-contents "(
:initfun
(lambda (proj)
   (proviso-put proj :proj-alist
               '( (:name \"base\" :dir \"d/e/\")
                ))
   (proviso-put proj :build-subdirs
                '( (:name \"bld\" :dir \"d2/\")
                   (:name \"two\" :dir \"d/e/f\")
                 ))
  )
)")
      (setq proviso-compile-command 'proviso-compile-command-repo)
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (setq read-index 0)               ;picks empty choice, which is root dir
      (proviso-compile)
      (should (string= compile-command
                       (concat ". " base "a/b/c/repo-setup.sh && cd " base "a/b/c/ && make")))
      (setq read-index 1)               ;picks 1st choice
      (proviso-compile)
      (should (string= compile-command
                       (concat ". " base "a/b/c/repo-setup.sh && cd " base "a/b/c/d2/ && make")))
      (setq read-index 2)               ;picks 2nd choice
      (proviso-compile)
      (should (string= compile-command
                       (concat ". " base "a/b/c/repo-setup.sh && cd " base "a/b/c/d/e/f/ && make")))
      (setq read-result (concat base "a/b/c2/"))
      (proviso-compile '(4))
      (should (string= compile-command
                       (concat ". " base "a/b/c/repo-setup.sh && cd " base "a/b/c2/ && make")))

      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-deftest proviso-compile-test-real-compile-with-repo-project-definition ()
  (proviso-test-reset-all)
  (let* ((base (file-name-directory load-file-name))
         (default-directory base)
         (compilation-read-command nil)
         (compilation-always-kill t)
         file-contents read-result read-index)
    (cl-letf (((symbol-function 'proviso--eval-file)
               (lambda (_)
                 (unless (string-empty-p (string-trim file-contents))
                   (car (read-from-string file-contents)))))
              ((symbol-function 'completing-read)
               (lambda (_ collection &optional _3 _4 _5 _6 _7 _8) (interactive)
                 (seq-elt collection read-index)))
              ((symbol-function 'read-directory-name)
               (lambda (_ &optional _2 _3 _4 _5)
                 read-result))
              ((symbol-function 'proviso--query-error)
               (lambda (_ err)
                 (message "proviso-query-error: %s" err))))
      ;; open file
      (setq file-contents "(
:initfun
(lambda (proj)
   (proviso-put proj :proj-alist
               '( (:name \"base\" :dir \"d/e/\")
                ))
   (proviso-put proj :build-subdirs
                '( (:name \"bld\" :dir \"d2/\")
                   (:name \"two\" :dir \"d/e/f\")
                 ))
  )
:compile-defun proviso-compile-command-repo
)")
      ;; will be overridden by the project setting for :compile-defun
      (setq proviso-compile-command 'proviso-compile-command-std)
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (setq read-index 0)               ;picks empty choice, which is root dir
      (proviso-compile)
      (should (string= compile-command
                       (concat ". " base "a/b/c/repo-setup.sh && cd " base "a/b/c/ && make")))
      (setq read-index 1)               ;picks 1st choice
      (proviso-compile)
      (should (string= compile-command
                       (concat ". " base "a/b/c/repo-setup.sh && cd " base "a/b/c/d2/ && make")))
      (setq read-index 2)               ;picks 2nd choice
      (proviso-compile)
      (should (string= compile-command
                       (concat ". " base "a/b/c/repo-setup.sh && cd " base "a/b/c/d/e/f/ && make")))
      (setq read-result (concat base "a/b/c2/"))
      (proviso-compile '(4))
      (should (string= compile-command
                       (concat ". " base "a/b/c/repo-setup.sh && cd " base "a/b/c2/ && make")))

      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-run-tests-batch-and-exit (car argv))

;;; test_proviso-compile.el ends here
