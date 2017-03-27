#!/bin/sh
":"; exec "$EMACSX" --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; -*-
;;; test_proviso-core.el --- test proviso-core.el
;; Copyright (C) 2017  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Monday, March 27, 2017
;; Version: 1.0
;; Modified Time-stamp: <2017-03-27 17:38:11 dharms>
;; Modified by: Dan Harms
;; Keywords: proviso projects

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
;; transfer dependencies from argv into load-path
(let ((lst (cdr argv))
      add elt)
  (setq argv nil)
  (while lst
    (setq elt (car lst))
    (if add
        (progn
          (push elt load-path)
          (setq add nil))
      (unless
          (setq add (string= elt "-L"))
        (push elt argv)))
      (setq lst (cdr lst))))
(push (concat (file-name-directory load-file-name) "/..") load-path)
(push (file-name-directory load-file-name) load-path)


(require 'ert)
(require 'proviso-core)

;; helper defun
(defun proviso-test-reset-all ()
  "Reset all profile-related data structures to nil."
  (setq proviso-obarray (make-vector 7 0))
  (intern "default" proviso-obarray)
  (setq proviso-path-alist '())
  (setq proviso-curr-proj nil)
  (setq proviso-local-proj (default-value 'proviso-local-proj))
  )

;; tests
(ert-deftest proviso-manipulate-properties-test ()
  (proviso-test-reset-all)
  (proviso-define "test")
  (let ((p (intern-soft "test" proviso-obarray)))
    (should (proviso-proj-p p))
    (should-not (proviso-get p :a))
    (proviso-put p :a "avalue")
    (should (string= "avalue" (proviso-get p :a)))
    (proviso-put p :a nil)
    (should-not (proviso-get p :a))
    ))

(ert-deftest proviso-manipulate-properties-derived-test ()
  (proviso-test-reset-all)
  (proviso-define "parent" :p 'value)
  (proviso-define-derived "child" "parent")
  (let ((p (intern "child" proviso-obarray)))
    (should (proviso-proj-p p))
    (should (eq (proviso-get p :p) 'value))
    (should-not (proviso-get p :p t))
    ))

(ert-deftest proviso-compute-basename-test ()
  (should (string= (proviso--compute-basename-from-file "example.proviso")
                        "example"))
  (should (string= (proviso--compute-basename-from-file ".this.proviso")
                        "this"))
  (should (string= (proviso--compute-basename-from-file
                    "~/first/second/sample.proviso") "sample"))
  (should (string= (proviso--compute-basename-from-file
                    "/home/user/third/.sample.proviso") "sample"))
  (should (string= (proviso--compute-basename-from-file
                    "~/sample/.proviso") "sample"))
  ;; (should (not (string-equal (proviso--compute-basename
  ;;                             "unknown") "")))
  )

(ert-deftest proviso-compute-stem-test ()
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

(ert-deftest proviso-find-root-test ()
  (let ((base (file-name-directory load-file-name))
        dir)
    (setq dir (concat base "a/b/c/d"))
    (should (equal (proviso--find-root dir t)
                   (cons (concat base "a/b/c/c.proviso")
                         (concat base "a/b/c/"))))
    ))


(ert-run-tests-batch-and-exit (car argv))

;;; test_proviso-core.el ends here
