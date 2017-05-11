#!/bin/sh
":"; exec "$EMACSX" --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; -*-
;;; test_proviso-grep-command.el --- test proviso grep-command
;; Copyright (C) 2017  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Wednesday, May  3, 2017
;; Version: 1.0
;; Modified Time-stamp: <2017-05-10 17:52:36 dharms>
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

(ert-deftest proviso-grep-cmd-test-create-cmd ()
  (let ((base (file-name-directory load-file-name))
        )
    ;; (should (string= (proviso-grep--create-command
    ))

(ert-run-tests-batch-and-exit (car argv))

;;; test_proviso-grep-command.el ends here
