#!/bin/sh
":"; exec "$VISUAL" --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; -*-
;;; test_proviso-core.el --- test proviso-core.el
;; Copyright (C) 2017-2019  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Monday, March 27, 2017
;; Version: 1.0
;; Modified Time-stamp: <2019-07-30 09:10:41 dharms>
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
  (proviso-define-project "test" "path")
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
  (proviso-define-project "parent" "path" :p 'value)
  (proviso-define-project-derived "child" "parent" "path")
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

(ert-deftest proviso-core-compute-stem-test ()
  (let ((proj (intern "temp" proviso-obarray)) str)
    ;; absolute path
    (setq str "/home/me/temp/")
    (proviso-put proj :root-dir str)
    (should (string= (proviso--compute-stem proj) str))
    ;; absolute, without trailing slash
    (setq str "/home/me/temp")
    (proviso-put proj :root-dir str)
    (should (string= (proviso--compute-stem proj) str))
    (proviso-put proj :root-dir "~/me")
    (should (string= (proviso--compute-stem proj) "me"))
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
