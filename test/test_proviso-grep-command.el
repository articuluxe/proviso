#!/bin/sh
":"; exec "$EMACSX" --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; -*-
;;; test_proviso-grep-command.el --- test proviso grep-command
;; Copyright (C) 2017  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Wednesday, May  3, 2017
;; Version: 1.0
;; Modified Time-stamp: <2017-05-24 08:37:50 dharms>
;; Modified by: Dan Harms
;; Keywords: proviso project grep command

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

(ert-deftest proviso-grep-cmd-test-create-extensions-str ()
  (let ((base (file-name-directory load-file-name))
        (proviso-extensions '(".cpp" ".hpp")))
    (should (string= (proviso-grep--create-extensions-str)
                     (concat
                      " \"(\" -name \"*moc_*\" -o -name \"*qrc_*\" \")\" "
                      "-prune -o -type f \"(\" -name \"*"
                      ".cpp\" -o -name \"*.hpp"
                      "\" \")\" -print0 | xargs -0 grep -Isn ")))
    ))

(ert-deftest proviso-grep-cmd-open-project-dir ()
  (proviso-test-reset-all)
  (let* ((base (file-name-directory load-file-name))
         (default-directory base)
         file-contents read-result)
    (cl-letf (((symbol-function 'proviso--load-file)
               (lambda (_)
                 (proviso-eval-string file-contents)))
              ((symbol-function 'ivy-read)
               (lambda (_ _2)
                 read-result))
              ((symbol-function 'read-directory-name)
               (lambda (_ _2 _3 _4)
                 read-result)))
      ;; test grep without a current project
      ;; empty settings; no arg uses default-directory
      (should (equal (proviso-grep--create-command)
                     (concat "find -P " (directory-file-name base)
                             (proviso-grep--create-extensions-str)
                     )))
      ;; empty settings; arg 4 uses default-directory
      (should (equal (proviso-grep--create-command 4)
                     (concat "find -P " (directory-file-name base)
                             (proviso-grep--create-extensions-str)
                             )))
      ;; empty settings: arg 16 reads dir from user
      (setq read-result (concat (directory-file-name base) "/a/b/c/d/e/"))
      (should (equal (proviso-grep--create-command 16)
                     (concat "find -P "
                             (concat (directory-file-name base) "/a/b/c/d/e")
                             (proviso-grep--create-extensions-str)
                     )))
      ;; open file
      (setq file-contents "
 (defun do-init (proj)
   (proviso-put proj :proj-alist
               '( (:name \"base\" :dir \"d/e/\")
                  (:name \"two\" :dir \"d/e/f\")
                  )))
 (proviso-define \"c\" :initfun 'do-init)
")
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (proviso-name-p (proviso-get proviso-local-proj :project-name)))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (equal (proviso-get proviso-local-proj :grep-dirs)
                     (list
                      (concat base "a/b/c/d/e/")
                      (concat base "a/b/c/d/e/f/")
                      (concat base "a/b/c/"))))
      ;; no arg takes from the first element of dirs
      (should (equal (proviso-grep--create-command)
                     (concat "find -P " base "a/b/c/d/e"
                             (proviso-grep--create-extensions-str)
                             )))
      ;; arg 4 lets user select dir
      (setq read-result (concat base "/a/b/c/d/e/f/"))
      (should (equal (proviso-grep--create-command 4)
                     (concat "find -P " base "a/b/c/d/e/f"
                             (proviso-grep--create-extensions-str)
                     )))
      ;; arg 16 asks user for dir
      (setq read-result (concat base "/a/b/c/d/e/f/"))
      (should (equal (proviso-grep--create-command 16)
                     (concat "find -P " base "a/b/c/d/e/f"
                             (proviso-grep--create-extensions-str)
                     )))

      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-run-tests-batch-and-exit (car argv))

;;; test_proviso-grep-command.el ends here
