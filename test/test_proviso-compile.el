#!/bin/sh
":"; exec "$EMACSX" --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; -*-
;;; test_proviso-compile.el --- test proviso compile
;; Copyright (C) 2017  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Thursday, May 25, 2017
;; Version: 1.0
;; Modified Time-stamp: <2017-05-29 09:14:39 dharms>
;; Modified by: Dan Harms
;; Keywords: proviso test compile

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

(ert-deftest proviso-compile-test ()
  (proviso-test-reset-all)
  (let* ((base (file-name-directory load-file-name))
         (default-directory base)
         file-contents read-result)
    (cl-letf (((symbol-function 'proviso--load-file)
               (lambda (_)
                 (proviso-eval-string file-contents)))
              ((symbol-function 'completing-read)
               (lambda (_ collection &optional _3 _4 _5 _6 _7 _8) (interactive)
                 (car (reverse collection))))
              ((symbol-function 'read-directory-name)
               (lambda (_ &optional _2 _3 _4 _5)
                 read-result)))
      (should (string= (proviso-compile-command-std)
                       "cd ./ && make"))
      (should (string= (proviso-compile-command-std '(4))
                       "cd ./ && make"))
      ;; open file
      (setq file-contents "
 (defun do-init (proj)
   (proviso-put proj :proj-alist
               '( (:name \"base\" :dir \"d/e/\")
                  (:name \"two\" :dir \"d/e/f\")
                  ))
   (proviso-put proj :build-subdirs
                '( (:name \"bld\" :dir \"d2/\")))
  )
 (proviso-define \"c\" :initfun 'do-init)
")
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (proviso-name-p (proviso-get proviso-local-proj :project-name)))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      )))

(ert-run-tests-batch-and-exit (car argv))

;;; test_proviso-compile.el ends here
