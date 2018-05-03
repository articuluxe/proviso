;;; proviso-defines.el --- defines useful for proviso
;; Copyright (C) 2017-2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Wednesday, September 20, 2017
;; Version: 1.0
;; Modified Time-stamp: <2018-05-03 15:18:21 dan.harms>
;; Modified by: Dan Harms
;; Keywords: tools project proviso
;; URL: https://github.com/articuluxe/proviso.git
;; Package-Requires: ((emacs "24.4"))

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
;; Provides defines and constants useful for proviso.
;;

;;; Code:

(defvar proviso-interesting-files
  '(
    "*.cpp" "*.cc" "*.cxx" "*.c" "*.C"
    "*.hpp" "*.hh" "*.hxx" "*.h" "*.H"
    "*.ac"
    "*.bat"
    "*.cmake"
    "*.cs"
    "*.dart"
    "*.el"
    "*.in"
    "*.java"
    "*.json"
    "*.proto"
    "*.py"
    "*.sh"
    "*.sql"
    "*.xml"
    "*.lua"
    "CMakeLists.txt"
    "[Mm]akefile"
    )
  "List of interesting file patterns.
These are in shell glob format.")

(defvar proviso-uninteresting-files '("*moc_*" "*qrc_*")
  "List of uninteresting file patterns.
These are in shell glob format.  This list could be extended to
include many more file extensions to ignore, such as `*.exe',
`*.pdb' or `*.obj', however those wouldn't be selected by
`proviso-interesting-files' anyway.  This list is currently
limited to those patterns which still apply as an additional
filter to those files already selected by
`proviso-interesting-files'.")

(defvar proviso-uninteresting-dirs '("*.git" "*.tags")
  "List of uninteresting directory patterns.
These are in shell glob format.")

(provide 'proviso-defines)
;;; proviso-defines.el ends here
