#!/bin/sh
":"; exec "$VISUAL" --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; -*-
;;; test_proviso-rg-command.el --- test proviso rg command
;; Copyright (C) 2017-2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Wednesday, January 24, 2018
;; Version: 1.0
;; Modified Time-stamp: <2018-01-25 08:13:43 dharms>
;; Modified by: Dan Harms
;; Keywords: tools proviso project rg command

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
;; test proviso rg command creation.
;;

;;; Code:
(load-file "test/proviso-test-common.el")
(require 'proviso)

(ert-deftest proviso-rg-cmd-test-create-cmd-exclude ()
  (let ((base (file-name-directory load-file-name))
        (proviso-interesting-files '("*.cpp" "*.hpp"))
        (proviso-uninteresting-files '("*moc_*" "*qrc_*"))
        (proviso-uninteresting-dirs '("*.git" "*.tags"))
        )
    (should (string= (proviso-rg--create-rg-str nil)
                     (concat
                      "-g '!*moc_*' -g '!*qrc_*' -g '!*.git' -g '!*.tags' -g '*.cpp' -g '*.hpp' ")))))

(ert-deftest proviso-rg-cmd-test-create-cmd-exclude-empty-file-blacklist ()
  (let ((base (file-name-directory load-file-name))
        (proviso-interesting-files '("*.cpp" "*.hpp"))
        (proviso-uninteresting-files '())
        (proviso-uninteresting-dirs '("*.git" "*.tags"))
        )
    (should (string= (proviso-rg--create-rg-str nil)
                     (concat
                      "-g '!*.git' -g '!*.tags' -g '*.cpp' -g '*.hpp' ")))))

(ert-deftest proviso-rg-cmd-test-create-cmd-exclude-empty-dir-blacklist ()
  (let ((base (file-name-directory load-file-name))
        (proviso-interesting-files '("*.cpp" "*.hpp"))
        (proviso-uninteresting-files '("*moc_*" "*qrc_*"))
        (proviso-uninteresting-dirs '())
        )
    (should (string= (proviso-rg--create-rg-str nil)
                     (concat
                      "-g '!*moc_*' -g '!*qrc_*' -g '*.cpp' -g '*.hpp' ")))))

(ert-deftest proviso-rg-cmd-test-create-cmd-exclude-empty-dir-and-file-blacklist ()
  (let ((base (file-name-directory load-file-name))
        (proviso-interesting-files '("*.cpp" "*.hpp"))
        (proviso-uninteresting-files '())
        (proviso-uninteresting-dirs '())
        )
    (should (string= (proviso-rg--create-rg-str nil)
                     (concat
                      "-g '*.cpp' -g '*.hpp' ")))))

(ert-deftest proviso-rg-cmd-test-create-cmd-exclude-no-include ()
  (let ((base (file-name-directory load-file-name))
        (proviso-interesting-files '())
        (proviso-uninteresting-files '("*moc_*" "*qrc_*"))
        (proviso-uninteresting-dirs '("*.git" "*.tags"))
        )
    (should (string= (proviso-rg--create-rg-str nil)
                     (concat
                      "-g '!*moc_*' -g '!*qrc_*' -g '!*.git' -g '!*.tags' ")))))

(ert-deftest proviso-rg-cmd-test-create-cmd-exclude-no-exclude-or-include ()
  (let ((base (file-name-directory load-file-name))
        (proviso-interesting-files '())
        (proviso-uninteresting-files '())
        (proviso-uninteresting-dirs '())
        )
    (should (string= (proviso-rg--create-rg-str nil)
                     (concat
                      "")))))

(ert-run-tests-batch-and-exit (car argv))
;;; test_proviso-rg-command.el ends here
