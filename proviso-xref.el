;;; proviso-xref.el --- xref helper for proviso
;; Copyright (C) 2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Monday, October 29, 2018
;; Version: 1.0
;; Modified Time-stamp: <2018-10-29 09:04:53 dharms>
;; Modified by: Dan Harms
;; Keywords: tools unix proviso project
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
;; Utilities aiding proviso in using xref.
;;

;;; Code:
(require 'proviso-core)
(require 'xref)
(require 'etags)

(defun proviso-xref-make-etags-location (tag-info file)
  "Make an etags location for TAG-INFO and FILE.
Supplies the 'relative parameter such that the path returned is
not absolute."
  (make-instance 'xref-etags-location
                 :tag-info tag-info
                 :file (file-of-tag t)))

(advice-add 'xref-make-etags-location
            :override #'proviso-xref-make-etags-location)

(cl-defmethod proviso-xref-location-marker ((l xref-etags-location))
  "Execute `xref-location-marker'.
Cognizant of possibly remote proviso projects."
  (with-slots (tag-info file t) l
    (let* ((file (if (and (featurep 'proviso)
                          proviso-curr-proj)
                     (concat
                      (proviso-get proviso-curr-proj :remote-prefix)
                      file)
                   file))
           (buffer (find-file-noselect file)))
      (with-current-buffer buffer
        (save-excursion
          (etags-goto-tag-location tag-info)
          (point-marker))))))

(advice-add 'xref-location-marker
            :override #'proviso-xref-location-marker)

(provide 'proviso-xref)
;;; proviso-xref.el ends here
