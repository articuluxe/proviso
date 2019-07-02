#!/bin/sh
":"; exec "$VISUAL" --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; -*-
;;; test_proviso-sml.el --- test proviso sml
;; Copyright (C) 2017-2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Monday, April  3, 2017
;; Version: 1.0
;; Modified Time-stamp: <2018-02-01 08:35:00 dharms>
;; Modified by: Dan Harms
;; Keywords: tools proviso project sml test

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

(ert-deftest proviso-sml-test-empty ()
  (proviso-test-reset-all)
  (let* ((base (file-name-directory load-file-name))
         (base-abbrev (proviso--abbreviate-dir base))
         file-contents)
    (cl-letf (((symbol-function 'proviso--load-file)
               (lambda (_) (proviso-eval-string file-contents))))
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
      ;; to only check the last element
      (should (equal (car (last sml/replacer-regexp-list))
                     (list (concat base-abbrev "a/b/c/") "BASE:")))
      ;; check all elements also; must update if sml updates
      (should (equal sml/replacer-regexp-list
                     (list (list "^~/org" ":Org:")
                           (list "^~/\\.emacs\\.d/" ":ED:")
                           (list "^/sudo:.*:" ":SU:")
                           (list "^~/Documents/" ":Doc:")
                           (list "^~/Dropbox/" ":DB:")
                           (list "^:\\([^:]*\\):Documento?s/" ":\\1/Doc:")
                           (list "^~/[Gg]it/" ":Git:")
                           (list "^~/[Gg]it[Hh]ub/" ":Git:")
                           (list "^~/[Gg]it\\([Hh]ub\\|\\)-?[Pp]rojects/" ":Git:")
                           (list (concat base-abbrev "a/b/c/") "BASE:"))))

      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-deftest proviso-sml-test-relative ()
  (proviso-test-reset-all)
  (let* ((base (file-name-directory load-file-name))
         (base-abbrev (proviso--abbreviate-dir base))
         file-contents)
    (cl-letf (((symbol-function 'proviso--load-file)
               (lambda (_) (proviso-eval-string file-contents))))
      ;; open file
      (setq file-contents "
 (defun do-init (proj)
   (proviso-put proj :proj-alist
               '( (:name \"rel\" :dir \"d/\")
                  )))
 (proviso-define-project \"c\" :initfun 'do-init)
")
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (proviso-name-p (proviso-get proviso-local-proj :project-name)))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      ;; to only check the last element
      (should (equal (car (last sml/replacer-regexp-list))
                     (list (concat base-abbrev "a/b/c/d/") "REL:")))
      ;; check all elements also; must update if sml updates
      (should (equal sml/replacer-regexp-list
                     (list (list "^~/org" ":Org:")
                           (list "^~/\\.emacs\\.d/" ":ED:")
                           (list "^/sudo:.*:" ":SU:")
                           (list "^~/Documents/" ":Doc:")
                           (list "^~/Dropbox/" ":DB:")
                           (list "^:\\([^:]*\\):Documento?s/" ":\\1/Doc:")
                           (list "^~/[Gg]it/" ":Git:")
                           (list "^~/[Gg]it[Hh]ub/" ":Git:")
                           (list "^~/[Gg]it\\([Hh]ub\\|\\)-?[Pp]rojects/" ":Git:")
                           (list (concat base-abbrev "a/b/c/d/") "REL:"))))

      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-deftest proviso-sml-test-absolute ()
  (proviso-test-reset-all)
  (let* ((base (file-name-directory load-file-name))
         (base-abbrev (proviso--abbreviate-dir base))
         file-contents)
    (cl-letf (((symbol-function 'proviso--load-file)
               (lambda (_) (proviso-eval-string file-contents))))
      ;; open file
      (setq file-contents "
 (defun do-init (proj)
   (proviso-put proj :proj-alist
               '( (:name \"absolute\" :dir \"/home/\")
                  )))
 (proviso-define-project \"c\" :initfun 'do-init)
")
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (proviso-name-p (proviso-get proviso-local-proj :project-name)))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      ;; to only check the last element
      (should (equal (car (last sml/replacer-regexp-list))
                     (list "/home/" "ABSOLUTE:")))
      ;; check all elements also; must update if sml updates
      (should (equal sml/replacer-regexp-list
                     (list (list "^~/org" ":Org:")
                           (list "^~/\\.emacs\\.d/" ":ED:")
                           (list "^/sudo:.*:" ":SU:")
                           (list "^~/Documents/" ":Doc:")
                           (list "^~/Dropbox/" ":DB:")
                           (list "^:\\([^:]*\\):Documento?s/" ":\\1/Doc:")
                           (list "^~/[Gg]it/" ":Git:")
                           (list "^~/[Gg]it[Hh]ub/" ":Git:")
                           (list "^~/[Gg]it\\([Hh]ub\\|\\)-?[Pp]rojects/" ":Git:")
                           (list "/home/" "ABSOLUTE:"))))

      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-deftest proviso-sml-test-no-project-file ()
  (proviso-test-reset-all)
  (let* ((base (file-name-directory load-file-name))
         (base-abbrev (proviso--abbreviate-dir base))
         file-contents)
    (cl-letf (((symbol-function 'proviso--load-file)
               (lambda (_) (proviso-eval-string file-contents))))
      ;; open file
      (setq file-contents "")
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (proviso-name-p (proviso-get proviso-local-proj :project-name)))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      ;; to only check the last element
      (should (equal (car (last sml/replacer-regexp-list))
                     (list (concat base-abbrev "a/b/c/") "C:")))
      ;; check all elements also; must update if sml updates
      (should (equal sml/replacer-regexp-list
                     (list (list "^~/org" ":Org:")
                           (list "^~/\\.emacs\\.d/" ":ED:")
                           (list "^/sudo:.*:" ":SU:")
                           (list "^~/Documents/" ":Doc:")
                           (list "^~/Dropbox/" ":DB:")
                           (list "^:\\([^:]*\\):Documento?s/" ":\\1/Doc:")
                           (list "^~/[Gg]it/" ":Git:")
                           (list "^~/[Gg]it[Hh]ub/" ":Git:")
                           (list "^~/[Gg]it\\([Hh]ub\\|\\)-?[Pp]rojects/" ":Git:")
                           (list (concat base-abbrev "a/b/c/") "C:"))))

      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-deftest proviso-sml-test-no-trailing-slash ()
  (proviso-test-reset-all)
  (let* ((base (file-name-directory load-file-name))
         (base-abbrev (proviso--abbreviate-dir base))
         file-contents)
    (cl-letf (((symbol-function 'proviso--load-file)
               (lambda (_) (proviso-eval-string file-contents))))
      ;; open file
      (setq file-contents "
 (defun do-init (proj)
   (proviso-put proj :proj-alist
               '(
                  (:name \"one\" :dir \"\")
                  (:name \"two\" :dir \"/home\")
                  (:name \"three\" :dir \"d\")
                  )))
 (proviso-define-project \"c\" :initfun 'do-init)
")
      ;; so parent directories should go first in this list, so that they're
      ;; later in the sml list, so that subdirs have a chance to go first
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (proviso-name-p (proviso-get proviso-local-proj :project-name)))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      ;; to only check the last element
      (should (equal (car (last sml/replacer-regexp-list))
                     (list (concat base-abbrev "a/b/c/") "ONE:")))
      ;; check all elements also; must update if sml updates
      (should (equal sml/replacer-regexp-list
                     (list (list "^~/org" ":Org:")
                           (list "^~/\\.emacs\\.d/" ":ED:")
                           (list "^/sudo:.*:" ":SU:")
                           (list "^~/Documents/" ":Doc:")
                           (list "^~/Dropbox/" ":DB:")
                           (list "^:\\([^:]*\\):Documento?s/" ":\\1/Doc:")
                           (list "^~/[Gg]it/" ":Git:")
                           (list "^~/[Gg]it[Hh]ub/" ":Git:")
                           (list "^~/[Gg]it\\([Hh]ub\\|\\)-?[Pp]rojects/" ":Git:")
                           (list (concat base-abbrev "a/b/c/d/") "THREE:")
                           (list "/home/" "TWO:")
                           (list (concat base-abbrev "a/b/c/") "ONE:")
                           )))

      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-deftest proviso-sml-test-switch-projects ()
  (proviso-test-reset-all)
  (let* ((base (file-name-directory load-file-name))
         (base-abbrev (proviso--abbreviate-dir base))
         file-contents)
    (cl-letf (((symbol-function 'proviso--load-file)
               (lambda (_) (proviso-eval-string file-contents))))
      ;; open file
      (setq file-contents "
 (defun do-init (proj)
   (proviso-put proj :proj-alist
               '( (:name \"base\" :dir \"\")
                  )))
 (proviso-define-project \"c\" :initfun 'do-init)
")
      ;; open 1st file
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (proviso-name-p (proviso-get proviso-local-proj :project-name)))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      ;; to only check the last element
      (should (equal (car (last sml/replacer-regexp-list))
                     (list (concat base-abbrev "a/b/c/") "BASE:")))
      ;; check all elements also; must update if sml updates
      (should (equal sml/replacer-regexp-list
                     (list (list "^~/org" ":Org:")
                           (list "^~/\\.emacs\\.d/" ":ED:")
                           (list "^/sudo:.*:" ":SU:")
                           (list "^~/Documents/" ":Doc:")
                           (list "^~/Dropbox/" ":DB:")
                           (list "^:\\([^:]*\\):Documento?s/" ":\\1/Doc:")
                           (list "^~/[Gg]it/" ":Git:")
                           (list "^~/[Gg]it[Hh]ub/" ":Git:")
                           (list "^~/[Gg]it\\([Hh]ub\\|\\)-?[Pp]rojects/" ":Git:")
                           (list (concat base-abbrev "a/b/c/") "BASE:"))))
      ;; open 2nd file, same project
      (find-file (concat base "a/b/c/d/dfile2"))
      (should (proviso-name-p (proviso-get proviso-local-proj :project-name)))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      ;; to only check the last element
      (should (equal (car (last sml/replacer-regexp-list))
                     (list (concat base-abbrev "a/b/c/") "BASE:")))
      ;; check all elements also; must update if sml updates
      (should (equal sml/replacer-regexp-list
                     (list (list "^~/org" ":Org:")
                           (list "^~/\\.emacs\\.d/" ":ED:")
                           (list "^/sudo:.*:" ":SU:")
                           (list "^~/Documents/" ":Doc:")
                           (list "^~/Dropbox/" ":DB:")
                           (list "^:\\([^:]*\\):Documento?s/" ":\\1/Doc:")
                           (list "^~/[Gg]it/" ":Git:")
                           (list "^~/[Gg]it[Hh]ub/" ":Git:")
                           (list "^~/[Gg]it\\([Hh]ub\\|\\)-?[Pp]rojects/" ":Git:")
                           (list (concat base-abbrev "a/b/c/") "BASE:"))))
      ;; open 3rd file, new project
      (setq file-contents "
 (defun do-init (proj)
   (proviso-put proj :proj-alist
               '( (:name \"second\" :dir \"d2/\")
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
      ;; to only check the last element
      (should (equal (car (last sml/replacer-regexp-list))
                     (list (concat base-abbrev "a/b/c2/d2/") "SECOND:")))
      ;; check all elements also; must update if sml updates
      (should (equal sml/replacer-regexp-list
                     (list (list "^~/org" ":Org:")
                           (list "^~/\\.emacs\\.d/" ":ED:")
                           (list "^/sudo:.*:" ":SU:")
                           (list "^~/Documents/" ":Doc:")
                           (list "^~/Dropbox/" ":DB:")
                           (list "^:\\([^:]*\\):Documento?s/" ":\\1/Doc:")
                           (list "^~/[Gg]it/" ":Git:")
                           (list "^~/[Gg]it[Hh]ub/" ":Git:")
                           (list "^~/[Gg]it\\([Hh]ub\\|\\)-?[Pp]rojects/" ":Git:")
                           (list (concat base-abbrev "a/b/c2/d2/") "SECOND:"))))

      ;; switch back to initial buffer
      (switch-to-buffer "dfile1")
      (run-hooks 'post-command-hook)    ;simulate interactive use
      ;; to only check the last element
      (should (equal (car (last sml/replacer-regexp-list))
                     (list (concat base-abbrev "a/b/c/") "BASE:")))
      ;; check all elements also; must update if sml updates
      (should (equal sml/replacer-regexp-list
                     (list (list "^~/org" ":Org:")
                           (list "^~/\\.emacs\\.d/" ":ED:")
                           (list "^/sudo:.*:" ":SU:")
                           (list "^~/Documents/" ":Doc:")
                           (list "^~/Dropbox/" ":DB:")
                           (list "^:\\([^:]*\\):Documento?s/" ":\\1/Doc:")
                           (list "^~/[Gg]it/" ":Git:")
                           (list "^~/[Gg]it[Hh]ub/" ":Git:")
                           (list "^~/[Gg]it\\([Hh]ub\\|\\)-?[Pp]rojects/" ":Git:")
                           (list (concat base-abbrev "a/b/c/") "BASE:"))))

      ;; clean up buffers
      (kill-buffer "dfile1")
      (kill-buffer "dfile2")
      (kill-buffer "dfile3")
      )))

(ert-deftest proviso-sml-test-build-subdirs-no-trailing-slash ()
  (proviso-test-reset-all)
  (let* ((base (file-name-directory load-file-name))
         (base-abbrev (proviso--abbreviate-dir base))
         file-contents)
    (cl-letf (((symbol-function 'proviso--load-file)
               (lambda (_) (proviso-eval-string file-contents))))
      ;; open file
      (setq file-contents "
 (defun do-init (proj)
   (proviso-put proj :proj-alist
               '(
                  (:name \"one\" :dir \"\")
                  (:name \"two\" :dir \"/home\")
                  (:name \"three\" :dir \"d\")
                  ))
   (proviso-put proj :build-subdirs
               '( (:name \"subdir\" :dir \"d2\")
                )))
 (proviso-define-project \"c\" :initfun 'do-init)
")
      ;; so parent directories should go first in this list, so that they're
      ;; later in the sml list, so that subdirs have a chance to go first
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (proviso-name-p (proviso-get proviso-local-proj :project-name)))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      ;; to only check the last element
      (should (equal (car (last sml/replacer-regexp-list))
                     (list (concat base-abbrev "a/b/c/d2/") "SUBDIR:")))
      ;; check all elements also; must update if sml updates
      (should (equal sml/replacer-regexp-list
                     (list (list "^~/org" ":Org:")
                           (list "^~/\\.emacs\\.d/" ":ED:")
                           (list "^/sudo:.*:" ":SU:")
                           (list "^~/Documents/" ":Doc:")
                           (list "^~/Dropbox/" ":DB:")
                           (list "^:\\([^:]*\\):Documento?s/" ":\\1/Doc:")
                           (list "^~/[Gg]it/" ":Git:")
                           (list "^~/[Gg]it[Hh]ub/" ":Git:")
                           (list "^~/[Gg]it\\([Hh]ub\\|\\)-?[Pp]rojects/" ":Git:")
                           (list (concat base-abbrev "a/b/c/d/") "THREE:")
                           (list "/home/" "TWO:")
                           (list (concat base-abbrev "a/b/c/") "ONE:")
                           (list (concat base-abbrev "a/b/c/d2/") "SUBDIR:")
                           )))

      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-run-tests-batch-and-exit (car argv))

;;; test_proviso-sml.el ends here
