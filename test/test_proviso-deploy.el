#!/bin/sh
":"; exec "$VISUAL" --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; -*-
;;; test_proviso-deploy.el --- test deploy utilities
;; Copyright (C) 2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Wednesday, September 26, 2018
;; Version: 1.0
;; Modified Time-stamp: <2018-09-26 08:52:15 dharms>
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

(ert-deftest proviso-deploy-test-read-file ()
  (let ((specs (proviso-deploy--read-from-str
                "((\"one\" . \"two\")(\"three\" . \"four\"))")))
    (should (equal specs
                   '((:source "one" :destination "two")
                     (:source "three" :destination "four"))))))


(ert-run-tests-batch-and-exit (car argv))
;;; test_proviso-deploy.el ends here
