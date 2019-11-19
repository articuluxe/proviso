#!/bin/sh
":"; exec "$VISUAL" --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; -*-
;;; test_proviso-deploy.el --- test deploy utilities
;; Copyright (C) 2018-2019  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Wednesday, September 26, 2018
;; Version: 1.0
;; Modified Time-stamp: <2019-11-19 08:30:22 dharms>
;; Modified by: Dan Harms
;; Keywords: tools proviso project
;; Package-Requires: ((emacs "25.1"))

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
(require 'proviso-deploy)

(ert-deftest proviso-deploy-test-read-file-simple ()
  (let ((specs (proviso-deploy--read-from-str
                "((\"one\" . \"two\")(\"three\" . \"four\")\"pwd\")")))
    (should (equal specs
                   '((:source "one" :destination "two" :type deploy)
                     (:source "three" :destination "four" :type deploy)
                     (:command "pwd" :type command))))))

(ert-deftest proviso-deploy-test-read-file-complex ()
  (let ((specs (proviso-deploy--read-from-str
                "((deploy . ((\"one\" . \"two\") \"pwd\" (\"three\" . \"four\"))))")))
    (should (equal specs
                   '((:source "one" :destination "two" :type deploy)
                     (:command "pwd" :type command)
                     (:source "three" :destination "four" :type deploy))))))

(ert-deftest proviso-deploy-test-write-file ()
  (let ((specs '((:source "one" :destination "two" :type deploy)
                 (:command "pwd" :type command)
                 (:source "three" :destination "four" :type deploy))))
    (with-temp-buffer
      (proviso-deploy--write-to-current-buffer specs)
      (should (equal (buffer-string)
                     "((deploy . (
(\"one\" . \"two\")
\"pwd\"
(\"three\" . \"four\")
)))
"
                     )))))

(ert-deftest proviso-deploy-test-conversion ()
  (let ((specs '((:source "one" :destination "two" :type deploy)
                 (:command "pwd" :type command)
                 (:source "three" :destination "four" :type deploy)))
        result)
    (with-temp-buffer
      (proviso-deploy--write-to-current-buffer specs)
      (should (equal specs
                     (proviso-deploy--read-from-str
                      (buffer-string)))))))

(ert-deftest proviso-deploy-test-env ()
  (let ((specs '((:source "one" :destination "two" :type deploy)
                 (:command "pwd" :type command)
                 (:env "FOO=BAR" :type env)
                 (:source "three" :destination "four" :type deploy)))
        result)
    (with-temp-buffer
      (proviso-deploy--write-to-current-buffer specs)
      (should (equal (buffer-string)
                     "((env . (
\"FOO=BAR\"
))
(deploy . (
(\"one\" . \"two\")
\"pwd\"
(\"three\" . \"four\")
)))
"
                     )))))

;; (proviso-deploy--split-sources "src/proviso.*\\.el$")
;; (("src" . "/") ("proviso.*" . "\\") ".el$")

;; (proviso-deploy--walk-sources (proviso-deploy--split-sources "src/proviso-deploy.el")
;;                               (getenv "HOME"))

(defun test-proviso-deployment (contents proj files)
  "Test that deployment CONTENTS copies FILES in project PROJ."
  (let* ((specs (proviso-deploy--read-from-str contents))
         (root (proviso-get proj :root-dir))
         (dest (concat root "deploydest/"))
         spec current)
    (proviso-put proj :deployments
                 (mapcar (lambda (spec)
                           (when (eq (plist-get spec :type)
                                     'deploy)
                             (plist-put spec :real-sources
                                        (proviso-deploy-compute-real-sources
                                         spec
                                         (proviso-get proj :remote-prefix)
                                         (proviso-get proj :root-dir)))
                             (plist-put spec :real-dest
                                        (proviso-deploy-compute-real-dest
                                         spec
                                         (proviso-get proj :remote-prefix)
                                         (proviso-get proj :root-dir)))))
                         specs))
    (delete-directory dest t)
    (proviso-deploy-all specs)
    (should (file-directory-p dest))
    (setq current (directory-files dest nil directory-files-no-dot-files-regexp))
    (should (equal (sort current #'string-lessp) (sort files #'string-lessp)))
    (delete-directory dest t)
    ))

(ert-deftest test-proviso-deploy-files ()
  (proviso-test-reset-all)
  (let ((base (file-name-directory load-file-name))
        (file-contents "")
        buffers)
    (cl-letf (((symbol-function 'proviso--eval-file)
               (lambda (_)
                 (unless (string-empty-p (string-trim file-contents))
                   (car (read-from-string file-contents))))))
      ;; open first file, init new project
      (should (not proviso-local-proj))
      (find-file (concat base "a/b/c/d/dfile1"))
      (push "dfile1" buffers)
      (should proviso-local-proj)
      (should (equal proviso-proj-alist
                     (list (cons (concat base "a/b/c/")
                                 (concat "c#" base "a/b/c/")))))

      ;;  deploy one file to directory
      (test-proviso-deployment "
((deploy . (
(\"file4.el\" . \"deploydest/\")
)))
" proviso-local-proj
'("file4.el"))

      ;; deploy one file, renamed
      (test-proviso-deployment "
((deploy . (
(\"file4.el\" . \"deploydest/newfile.el\")
)))
" proviso-local-proj
'("newfile.el"))

      ;; deploy one file from subdirectory
      (test-proviso-deployment "
((deploy . (
(\"d/file7.http\" . \"deploydest/\")
)))
" proviso-local-proj
'("file7.http"))

      ;; deploy one file from subdirectory, renamed
      (test-proviso-deployment "
((deploy . (
(\"d/file7.http\" . \"deploydest/hfile.http\")
)))
" proviso-local-proj
'("hfile.http"))

      ;; deploy contents of subdirectory;
      ;; note this skips contained subdirectories
      (test-proviso-deployment "
((deploy . (
(\"subdir/\" . \"deploydest/\")
)))
" proviso-local-proj
'("file8.txt" "file9.org"))

      ;; deploy regexp matching files
      (test-proviso-deployment "
((deploy . (
(\".*\\.el$\" . \"deploydest/\")
)))
" proviso-local-proj
'("file4.el" "file5.el"))

      ;; deploy regexp in subdir
      (test-proviso-deployment "
((deploy . (
(\"d/.*\\.el$\" . \"deploydest/\")
)))
" proviso-local-proj
'("file6.el"))

      ;; clean up buffers
      (dolist (b buffers) (kill-buffer b))
      )))

(ert-run-tests-batch-and-exit (car argv))
;;; test_proviso-deploy.el ends here
