;;; proviso-test-search-dirs.el --- test search dirs
;; Copyright (C) 2021  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Wednesday, April 14, 2021
;; Version: 1.0
;; Modified Time-stamp: <2021-04-14 15:41:54 dharms>
;; Modified by: Dan Harms
;; Keywords: tools project proviso
;; URL: https://github.com/articuluxe/proviso.git
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
;; Test search dir initialization.
;;

;;; Code:
(load-file "test/proviso-test-common.el")
(require 'proviso)
(require 'proviso-defines)

(ert-deftest proviso-search-test-filter-items ()
  (should (equal (proviso--filter-search-items
                  proviso-uninteresting-dirs
                  nil)                  ;specify nothing -> orig list
                 proviso-uninteresting-dirs))
  (should (equal (proviso--filter-search-items
                  proviso-uninteresting-dirs
                  '("*solitary"))       ;specify override
                 '("*solitary")))
  (should (equal (sort
                  (proviso--filter-search-items
                  proviso-uninteresting-dirs
                  '("one" "two"))       ;specify override
                  'string-lessp)
                 (sort '("one" "two") 'string-lessp)))
  (should (equal (sort
                  (proviso--filter-search-items
                  proviso-uninteresting-dirs
                  '(+ "one"))       ;specify addition
                  'string-lessp)
                 (sort (append (list "one")
                               proviso-uninteresting-dirs)
                       'string-lessp)))
  (should (equal (sort
                  (proviso--filter-search-items
                  proviso-uninteresting-dirs
                  '(- "*.tags"))       ;specify subtraction
                  'string-lessp)
                 (sort (remove "*.tags"
                               proviso-uninteresting-dirs)
                       'string-lessp)))
  (should (equal (sort
                  (proviso--filter-search-items
                  proviso-uninteresting-dirs
                  '(- "missing"))       ;specify subtraction not present
                  'string-lessp)
                 (sort (append proviso-uninteresting-dirs)
                       'string-lessp)))
  (should (equal (sort
                  (proviso--filter-search-items
                  proviso-uninteresting-dirs
                  '(- "*.tags" + "*.also"))       ;specify addition + subtraction
                  'string-lessp)
                 (sort (append (list "*.also")
                               (remove "*.tags" proviso-uninteresting-dirs))
                       'string-lessp)))
  )

;;; proviso-test-search-dirs.el ends here
