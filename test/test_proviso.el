#!/bin/sh
":"; exec "$EMACSX" --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; lexical-binding: t; -*-
;;; profile-tests.el --- test profiles
;; Copyright (C) 2016-2017  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Friday, December  9, 2016
;; Version: 1.0
;; Modified Time-stamp: <2017-02-24 06:05:08 dharms>
;; Modified by: Dan Harms
;; Keywords: profiles test

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
(require 'proviso)

(defun proviso-test-reset-all ()
  "Reset all profile-related data structures to nil."
  (setq proviso-obarray (make-vector 7 0))
  (intern "default" proviso-obarray)
  (setq proviso-path-alist '())
  (setq proviso-curr-prof nil)
  (setq proviso-local-prof (default-value 'proviso-local-prof))
  )

(ert-deftest proviso-compile-test()
  (let ((byte-compile-error-on-warn t))
    (should (byte-compile-file load-file-name))
    (delete-file (byte-compile-dest-file load-file-name) nil)))

(ert-deftest proviso-manipulate-properties-test ()
  (proviso-test-reset-all)
  (proviso-define "test")
  (let ((p (intern-soft "test" proviso-obarray)))
    (should (proviso-prof-p p))
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
    (should (proviso-prof-p p))
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
  (let ((prof (intern "temp" proviso-obarray)) str)
    ;; absolute path
    (setq str "/home/me/temp/")
    (proviso-put prof :root-dir str)
    (should (string= (proviso--compute-stem prof) str))
    ;; absolute, without trailing slash
    (setq str "/home/me/temp")
    (proviso-put prof :root-dir str)
    (should (string= (proviso--compute-stem prof) str))
    (proviso-put prof :root-dir "~/me")
    (should (string= (proviso--compute-stem prof) "me"))
    ))

(ert-deftest proviso-find-root-test ()
  (let ((base (file-name-directory load-file-name))
        dir)
    (setq dir (concat base "a/b/c/d"))
    (should (equal (proviso--find-root dir t)
                   (cons (concat base "a/b/c/c.proviso")
                         (concat base "a/b/c/"))))
    ))

(ert-deftest proviso-open-profile-test ()
  (proviso-test-reset-all)
  (let ((base (file-name-directory load-file-name))
        contents)
    (cl-letf (((symbol-function 'proviso--load-file)
               (lambda (_)
                 (message "about to read from %s" contents)
                 (eval (car (read-from-string contents))))))
      ;; open first file, init new profile
      (setq contents "(proviso-define \"c\" :name \"c\")")
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (proviso-name-p (proviso-get proviso-local-prof :project-name)))
      (should (equal proviso-path-alist
                     (cons (cons (concat base "a/b/c/") "c") nil)))
      (should (eq proviso-local-prof proviso-curr-prof))
      (should (string= (concat base "a/b/c/")
                       (proviso-get proviso-local-prof :root-dir)))
      (should (string= (proviso-get proviso-local-prof :project-name)
                       "c"))
      (should (eq (proviso-get proviso-local-prof :inited) t))
      ;; open 2nd file, same profile
      (find-file (concat base "a/b/c/d/dfile2"))
      (should (equal proviso-path-alist
                     (cons (cons (concat base "a/b/c/") "c") nil)))
      (should (eq proviso-local-prof proviso-curr-prof))
      (should (string= (concat base "a/b/c/")
                       (proviso-get proviso-local-prof :root-dir)))
      (should (string= (proviso-get proviso-local-prof :project-name)
                       "c"))
      (should (eq (proviso-get proviso-local-prof :inited) t))
      ;; open 3rd file, new profile
      (should (not (proviso-name-p "c2")))
      (find-file (concat base "a/b/c2/d2/dfile3"))
      (should (proviso-name-p "c2"))
      (should (equal proviso-path-alist
                     (list (cons (concat base "a/b/c2/") "c2")
                           (cons (concat base "a/b/c/") "c"))))
      (should (eq proviso-local-prof proviso-curr-prof))
      (should (string= (concat base "a/b/c2/")
                       (proviso-get proviso-local-prof :root-dir)))
      (should (string= (proviso-get proviso-local-prof :project-name)
                       "c2"))
      (should (eq (proviso-get proviso-local-prof :inited) t))
      ;; clean up buffers
      (kill-buffer "dfile1")
      (kill-buffer "dfile2")
      (kill-buffer "dfile3")
      )))

(ert-run-tests-batch-and-exit (car argv))

;;; profile-tests.el ends here
