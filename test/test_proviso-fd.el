#!/bin/sh
":"; exec "$VISUAL" --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; -*-
;;; test_proviso-fd.el --- Test proviso-fd
;; Copyright (C) 2019  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Tuesday, October 22, 2019
;; Version: 1.0
;; Modified Time-stamp: <2019-10-22 22:40:46 dharms>
;; Modified by: Dan Harms
;; Keywords: tools proviso fd fulledit project

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
;; Test file gathering utilities.
;;

;;; Code:
(load-file "test/proviso-test-common.el")

(require 'proviso-fulledit)
(require 'proviso-fd)

(defun test-proviso-fd-fulledit (dir)
  "Get files under DIR."
  (interactive "DDir: ")
  (ivy-read "File: "
            (proviso-finder-adjust-paths
             (sort
              (proviso-fulledit-gather-files
               dir proviso-uninteresting-files
               proviso-uninteresting-dirs proviso-interesting-files)
              'string-lessp)
             nil (expand-file-name dir))))

(defun test-proviso-fd-fd (dir)
  "Get files under DIR."
  (interactive "DDir: ")
  (ivy-read "File: "
            (proviso-finder-adjust-paths
             (sort
              (proviso-fd-gather-files
               dir nil proviso-uninteresting-files
               proviso-uninteresting-dirs proviso-interesting-files)
              'string-lessp)
             nil (expand-file-name dir))))

;;; test_proviso-fd.el ends here
