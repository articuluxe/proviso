#!/bin/sh
":"; exec "$VISUAL" --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; -*-
;;; test_proviso-grep-command.el --- test proviso grep-command
;; Copyright (C) 2017-2019  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Wednesday, May  3, 2017
;; Version: 1.0
;; Modified Time-stamp: <2019-07-30 00:21:29 dharms>
;; Modified by: Dan Harms
;; Keywords: tools proviso project grep command

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
;; Test proviso grep command creation functionality.
;;

;;; Code:
(load-file "test/proviso-test-common.el")
(require 'proviso)

(ert-deftest proviso-grep-cmd-test-create-cmd-exclude ()
  (let ((base (file-name-directory load-file-name))
        (proviso-interesting-files '("*.cpp" "*.hpp"))
        (proviso-uninteresting-files '("*moc_*" "*qrc_*"))
        (proviso-uninteresting-dirs '("*.git"))
        )
    (should (string= (proviso-grep--create-grep-str nil)
                     (concat
                      " \"(\" -name \"*moc_*\" -o -name \"*qrc_*\" -o -path \"*.git\" \")\" "
                      "-prune -o -type f \"(\" -name \"*.cpp\" -o -name \"*.hpp"
                      "\" \")\" -print0 | xargs -0 grep --null -Isni ")))
    ))

(ert-deftest proviso-grep-cmd-test-create-cmd-exclude-empty-file-blacklist ()
  (let ((base (file-name-directory load-file-name))
        (proviso-interesting-files '("*.cpp" "*.hpp"))
        (proviso-uninteresting-files '())
        (proviso-uninteresting-dirs '("*.git"))
        )
    (should (string= (proviso-grep--create-grep-str nil)
                     (concat
                      " \"(\" -path \"*.git\" \")\" "
                      "-prune -o -type f \"(\" -name \"*.cpp\" -o -name \"*.hpp"
                      "\" \")\" -print0 | xargs -0 grep --null -Isni ")))
    ))

(ert-deftest proviso-grep-cmd-test-create-cmd-exclude-empty-dir-blacklist ()
  (let ((base (file-name-directory load-file-name))
        (proviso-interesting-files '("*.cpp" "*.hpp"))
        (proviso-uninteresting-files '("*moc_*" "*qrc_*"))
        (proviso-uninteresting-dirs '())
        )
    (should (string= (proviso-grep--create-grep-str nil)
                     (concat
                      " \"(\" -name \"*moc_*\" -o -name \"*qrc_*\" \")\" "
                      "-prune -o -type f \"(\" -name \"*.cpp\" -o -name \"*.hpp"
                      "\" \")\" -print0 | xargs -0 grep --null -Isni ")))
    ))

(ert-deftest proviso-grep-cmd-test-create-cmd-exclude-empty-dir-and-file-blacklist ()
  (let ((base (file-name-directory load-file-name))
        (proviso-interesting-files '("*.cpp" "*.hpp"))
        (proviso-uninteresting-files '())
        (proviso-uninteresting-dirs '())
        (proviso-grep-args "-Isn")
        )
    (should (string= (proviso-grep--create-grep-str nil)
                     (concat
                      " -type f \"(\" -name \"*.cpp\" -o -name \"*.hpp"
                      "\" \")\" -print0 | xargs -0 grep -Isn ")))
    ))

(ert-deftest proviso-grep-cmd-test-create-cmd-no-include ()
  (let ((base (file-name-directory load-file-name))
        (proviso-interesting-files '())
        (proviso-uninteresting-files '("*moc_*" "*qrc_*"))
        (proviso-uninteresting-dirs '("*.git" "*.svn"))
        (proviso-grep-args "-Isn")
        )
    (should (string= (proviso-grep--create-grep-str nil)
                     (concat
                      " \"(\" -name \"*moc_*\" -o -name \"*qrc_*\" -o -path \"*.git\" -o -path \"*.svn\" \")\" "
                      "-prune -o -type f -print0 | xargs -0 grep -Isn ")))
    ))

(ert-deftest proviso-grep-cmd-test-create-cmd-no-exclude-or-include ()
  (let ((base (file-name-directory load-file-name))
        (proviso-interesting-files '())
        (proviso-uninteresting-files '())
        (proviso-uninteresting-dirs '())
        (proviso-grep-args "-Isn")
        )
    (should (string= (proviso-grep--create-grep-str nil)
                     (concat
                      " -type f -print0 | xargs -0 grep -Isn ")))
    ))

(ert-deftest proviso-grep-cmd-open-project-dir ()
  (proviso-test-reset-all)
  (let* ((base (file-name-directory load-file-name))
         (default-directory base)
         file-contents read-result)
    (cl-letf (((symbol-function 'proviso--eval-file)
               (lambda (_)
                 (unless (string-empty-p (string-trim file-contents))
                   (car (read-from-string file-contents)))))
              ((symbol-function 'completing-read)
               (lambda (_ _2)
                 read-result))
              ((symbol-function 'read-directory-name)
               (lambda (_ _2 _3 _4)
                 read-result))
              ((symbol-function 'proviso--query-error)
               (lambda (_ err)
                 (message "proviso-query-error: %s" err))))
      ;; test grep without a current project
      (should (eq (proviso-current-project-root) nil))
      (should (eq (proviso-current-project) nil))
      ;; empty settings; no arg uses default-directory
      (should (equal (proviso-grep--create-command)
                     (concat "find -P " (directory-file-name base)
                             (proviso-grep--create-grep-str nil)
                     )))
      ;; empty settings; arg 4 uses default-directory
      (should (equal (proviso-grep--create-command 4)
                     (concat "find -P " (directory-file-name base)
                             (proviso-grep--create-grep-str nil)
                             )))
      ;; empty settings: arg 16 reads dir from user
      (setq read-result (concat (directory-file-name base) "/a/b/c/d/e/"))
      (should (equal (proviso-grep--create-command 16)
                     (concat "find -P "
                             (concat (directory-file-name base) "/a/b/c/d/e")
                             (proviso-grep--create-grep-str nil)
                     )))
      ;; open file
      (setq file-contents "(
:initfun (lambda (proj)
   (proviso-put proj :proj-alist
               '( (:name \"base\" :dir \"d/e/\")
                  (:name \"two\" :dir \"d/e/f\")
                  )))
)")
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-current-project-root)
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
                             (proviso-grep--create-grep-str proviso-local-proj)
                             )))
      ;; arg 4 lets user select dir
      (setq read-result (concat base "/a/b/c/d/e/f/"))
      (should (equal (proviso-grep--create-command 4)
                     (concat "find -P " base "a/b/c/d/e/f"
                             (proviso-grep--create-grep-str proviso-local-proj)
                     )))
      ;; arg 16 asks user for dir
      (setq read-result (concat base "/a/b/c/d/e/f/"))
      (should (equal (proviso-grep--create-command 16)
                     (concat "find -P " base "a/b/c/d/e/f"
                             (proviso-grep--create-grep-str proviso-local-proj)
                     )))

      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-run-tests-batch-and-exit (car argv))

;;; test_proviso-grep-command.el ends here
