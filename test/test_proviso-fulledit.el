#!/bin/sh
":"; exec "$VISUAL" --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; -*-
;;; test_proviso-fulledit.el --- test proviso full-edit
;; Copyright (C) 2017-2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Wednesday, September 20, 2017
;; Version: 1.0
;; Modified Time-stamp: <2018-01-03 22:57:53 dharms>
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
;; test proviso-fulledit.el
;;

;;; Code:
(load-file "test/proviso-test-common.el")
(require 'proviso)

(ert-deftest proviso-fulledit-test-list-for-string ()
  (should (proviso-fulledit-test-list-for-string
           '(".ne" "two")
           "one"))
  (should (not (proviso-fulledit-test-list-for-string
                '(".ne" "two")
                "onq")))
  )

(ert-run-tests-batch-and-exit (car argv))
;;; test_proviso-fulledit.el ends here
