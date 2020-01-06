;;; test_proviso-regexp.el --- test proviso regexp
;; Copyright (C) 2017-2019  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Wednesday, November  8, 2017
;; Version: 1.0
;; Modified Time-stamp: <2019-12-31 12:40:12 dharms>
;; Modified by: Dan Harms
;; Keywords: tools proviso projects regexp
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
;; Test proviso regexp functionality.
;;

;;; Code:
(load-file "test/proviso-test-common.el")
(require 'proviso-regexp)

(ert-deftest proviso-regexp-test-regexes ()
  (should (string= (proviso-regexp-glob-to-regex "")
                   ""))
  (should (string= (proviso-regexp-glob-to-regex "Arbitrary")
                   "^Arbitrary$"))
  (should (string= (proviso-regexp-glob-to-regex "*.git")
                   ".*\\.git$"))
  (should (string= (proviso-regexp-glob-to-regex "bld*")
                   "^bld.*"))
  (should (string= (proviso-regexp-glob-to-regex "repos/")
                   "^repos/$"))
  (should (string= (proviso-regexp-glob-to-regex "[Mm]akefile")
                   "^[Mm]akefile$"))
  (should (string= (proviso-regexp-glob-to-regex "test.html?")
                   "^test\\.html?$"))
  (should (string= (proviso-regexp-glob-to-regex "t*e*t.c*v")
                   "^t.*e.*t\\.c.*v$"))
  )


;;; test_proviso-regexp.el ends here
