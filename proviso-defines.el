;;; proviso-defines.el --- Provide defines useful for proviso
;; Copyright (C) 2017-2019, 2021  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Wednesday, September 20, 2017
;; Version: 1.0
;; Modified Time-stamp: <2021-04-14 13:45:08 dharms>
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
    "*.ts"
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

(defun proviso--filter-search-items (lst input)
  "Filter default list LST according to INPUT.
INPUT is a list of items to replace, by default, the items in the
default list.  If INPUT contains the symbol +, then subsequent
items will be added to the defaults.  If INPUT contains the
symbol -, then subsequent items will be removed from the
defaults, if present.  There can be multiple such directives in
the INPUT list, each takes effect on subsequent elements until
another appears."
  (let ((action 'replace))
    (dolist (elt input)
      (if (symbolp elt)
          (cond ((eq elt '+)
                 (setq action 'add))
                ((eq elt '-)
                 (setq action 'subtract)))
        (if (stringp elt)
            (cond ((eq action 'add)
                   (setq lst (cons elt lst)))
                  ((eq action 'subtract)
                   (setq lst (remove elt lst)))
                  ((eq action 'replace)
                   (setq lst (list elt))
                   (setq action 'add)))
          (error "Unhandled action %S" elt)))))
  lst)

(defun proviso--set-grep-files-dirs (proj)
  "Init search settings for PROJ after project is loaded."
  (let ((exclude-dirs proviso-uninteresting-dirs)
        (exclude-files proviso-uninteresting-files)
        (include-files proviso-interesting-files)
        (ex-dir (proviso-get proj :search-exclude-dirs))
        (ex-file (proviso-get proj :search-exclude-files))
        (inc-file (proviso-get proj :search-include-files))
        action)
    (proviso-put proj :grep-exclude-dirs
                 (proviso--filter-search-items exclude-dirs ex-dir))
    (proviso-put proj :grep-exclude-files
                 (proviso--filter-search-items exclude-files ex-file))
    (proviso-put proj :grep-include-files
                 (proviso--filter-search-items include-files inc-file))))


(add-hook 'proviso-hook-on-project-post-init 'proviso--set-grep-files-dirs)

(provide 'proviso-defines)
;;; proviso-defines.el ends here
