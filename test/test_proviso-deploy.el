#!/bin/sh
":"; exec "$VISUAL" --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; -*-
;;; test_proviso-deploy.el --- test deploy utilities
;; Copyright (C) 2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Wednesday, September 26, 2018
;; Version: 1.0
;; Modified Time-stamp: <2018-10-10 08:58:15 dharms>
;; Modified by: Dan Harms
;; Keywords: tools proviso project

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
(require 'proviso-deploy)

(ert-deftest proviso-deploy-test-read-file-simple ()
  (let ((specs (proviso-deploy--read-from-str
                "((\"one\" . \"two\")(\"three\" . \"four\")\"pwd\")")))
    (should (equal specs
                   '((:source "one" :destination "two")
                     (:source "three" :destination "four")
                     (:command "pwd"))))))

(ert-deftest proviso-deploy-test-read-file-complex ()
  (let ((specs (proviso-deploy--read-from-str
                "((deploy . ((\"one\" . \"two\") \"pwd\" (\"three\" . \"four\"))))")))
    (should (equal specs
                   '((:source "one" :destination "two")
                     (:command "pwd")
                     (:source "three" :destination "four"))))))

(ert-deftest proviso-deploy-test-write-file ()
  (let ((specs '((:source "one" :destination "two")
                 (:command "pwd")
                 (:source "three" :destination "four"))))
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
  (let ((specs '((:source "one" :destination "two")
                 (:command "pwd")
                 (:source "three" :destination "four")))
        result)
    (with-temp-buffer
      (proviso-deploy--write-to-current-buffer specs)
      (should (equal specs
                     (proviso-deploy--read-from-str
                      (buffer-string)))))))

(ert-run-tests-batch-and-exit (car argv))
;;; test_proviso-deploy.el ends here
