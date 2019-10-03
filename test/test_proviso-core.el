#!/bin/sh
":"; exec "$VISUAL" --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; -*-
;;; test_proviso-core.el --- test proviso-core.el
;; Copyright (C) 2017-2019  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Monday, March 27, 2017
;; Version: 1.0
;; Modified Time-stamp: <2019-10-03 08:41:29 dharms>
;; Modified by: Dan Harms
;; Keywords: tools proviso projects

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
;; Proviso tests of core functionality.
;;

;;; Code:
(load-file "test/proviso-test-common.el")

;; tests
(ert-deftest proviso-core-manipulate-properties-test ()
  (proviso-test-reset-all)
  (proviso-define-project "test" '(("path")))
  (let* ((prov (intern-soft "test" proviso-provisional-obarray))
         (proj (proviso-define-active-project "test" (symbol-plist prov))))
    (should (proviso-provisional-proj-p prov))
    (should (proviso-proj-p proj))
    (should-not (proviso-get proj :a))
    (proviso-put proj :a "avalue")
    (should (string= "avalue" (proviso-get proj :a)))
    (proviso-put proj :a nil)
    (should-not (proviso-get proj :a))
    ))

(ert-deftest proviso-core-manipulate-properties-derived-test ()
  (proviso-test-reset-all)
  (proviso-define-project "parent" '(("path")) :p 'value)
  (proviso-define-project-derived "child" "parent" '(("path")))
  (let* ((prov (intern-soft "child" proviso-provisional-obarray))
         (proj (proviso-define-active-project "child" (symbol-plist prov))))
    (should (proviso-provisional-proj-p prov))
    (should (proviso-proj-p proj))
    (should (eq (proviso-get proj :p) 'value))
    (should-not (proviso-get proj :p t))
    ))

(ert-deftest proviso-core-compute-basename-test ()
  (should (string= (proviso-compute-basename-from-file "example.proviso")
                        "example"))
  (should (string= (proviso-compute-basename-from-file ".this.proviso")
                        "this"))
  (should (string= (proviso-compute-basename-from-file
                    "~/first/second/sample.proviso") "sample"))
  (should (string= (proviso-compute-basename-from-file
                    "/home/user/third/.sample.proviso") "sample"))
  (should (string= (proviso-compute-basename-from-file
                    "~/sample/.proviso") "sample"))
  (should (string= (proviso-compute-basename-from-file
                    "~/sample/.git") "sample"))
  )

(ert-deftest proviso-core-find-provisional-project-test ()
  (let ((proviso-path-alist '()))
    (should-not (proviso-find-provisional-project "~/src/path/path2/file")))
  (let ((proviso-path-alist '(("path/" "neon"))))
    (should (equal
             (proviso-find-provisional-project "~/src/path/path2/file")
             '("~/src/path/" "neon" "neon"))))
  (let ((proviso-path-alist '(("path" "neon"))))
    (should (equal
             (proviso-find-provisional-project "~/src/path/path2/file")
             '("~/src/path" "neon" "neon"))))
  (let ((proviso-path-alist '(("path2/" "neon"))))
    (should (equal
             (proviso-find-provisional-project "~/src/path/path2/file")
             '("~/src/path/path2/" "neon" "neon"))))
  (let ((proviso-path-alist '(("\\(path2\\)/" "neon" "neon-\\1"))))
    (should (equal
             (proviso-find-provisional-project "~/src/path/path2/file")
             '("~/src/path/path2/" "neon" "neon-path2"))))
  (let ((proviso-path-alist '(("\\(path\\)/\\(path\\(.*\\)\\)/" "neon" "neon-\\3"))))
    (should (equal
             (proviso-find-provisional-project "~/src/path/path2/file")
             '("~/src/path/path2/" "neon" "neon-2"))))
  (let ((proviso-path-alist '(("\\(path\\)\\(unused\\)?/\\(path2\\)/" "neon" "neon-\\3"))))
    (should (equal
             (proviso-find-provisional-project "~/src/path/path2/file")
             '("~/src/path/path2/" "neon" "neon-path2"))))
  (let ((proviso-path-alist '(("src/\\(.+?\\)/\\(.+?\\)/" "neon" "\\1-\\2"))))
    (should (equal
             (proviso-find-provisional-project "~/src/path/path2/file")
             '("~/src/path/path2/" "neon" "path-path2"))))
  )

(ert-deftest proviso-core-test-add-active-projects ()
  (let (proj)
    (proviso-test-reset-all)
    (should-not (proviso-find-active-project "/a/b/" "rem-host"))
    (proviso-add-active-project-path "a/b/" "neon#/a/b/@rem-host" "rem-host")
    (setq proj (proviso-find-active-project "/a/b/" "rem-host"))
    (should proj)
    (proviso-define-active-project proj) ;so that proviso-put works
    (should (equal proj "neon#/a/b/@rem-host"))
    ;; test hack: normally the load process would set this
    (proviso-put proj :remote-host "rem-host")
    (proviso-hard-reset proj)
    (should (not (proviso-find-active-project "/a/b/" "rem-host")))
    ))

(ert-deftest proviso-core-find-root-test ()
  (let ((base (file-name-directory load-file-name))
        dir)
    (setq dir (concat base "a/b/c/d"))
    (should (equal (proviso--find-root dir t)
                   (list (concat base "a/b/c/c.proviso")
                         (concat base "a/b/c/"))))
    ))


(ert-run-tests-batch-and-exit (car argv))

;;; test_proviso-core.el ends here
