;;; proviso-xref.el --- xref helper for proviso
;; Copyright (C) 2018-2019, 2021-2022  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Monday, October 29, 2018
;; Version: 1.0
;; Modified Time-stamp: <2022-09-16 15:08:37 dharms>
;; Modified by: Dan Harms
;; Keywords: tools unix proviso project
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
;; Utilities aiding proviso in using xref.
;;

;;; Code:
(require 'proviso-core)
(require 'etags)

(when (> emacs-major-version 24)

  (require 'xref)

  (defun proviso-xref-activate-dumb-jump ()
    "Activate `dumb-jump'."
    (interactive)
    (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
    (message "Added `dumb-jump' to xref."))

  (defun proviso-xref-deactivate-dumb-jump ()
    "Deactivate `dumb-jump'."
    (interactive)
    (remove-hook 'xref-backend-functions #'dumb-jump-xref-activate)
    (message "Removed `dumb-jump' from xref."))

  (defun proviso-xref-toggle-dumb-jump ()
    "Toggle `dumb-jump' in or out of xref."
    (interactive)
    (if (memq 'dumb-jump-xref-activate xref-backend-functions)
        (proviso-xref-deactivate-dumb-jump)
      (proviso-xref-activate-dumb-jump)))

;;   (defun proviso-xref-make-etags-location (tag-info file)
;;     "Make an etags location for TAG-INFO and FILE.
;; Supplies the 'relative parameter such that the path returned is
;; not absolute."
;;     (make-instance 'xref-etags-location
;;                    :tag-info tag-info
;;                    :file (file-of-tag t)))

  ;; (advice-add 'xref-make-etags-location
  ;;             :override #'proviso-xref-make-etags-location)

;;   (cl-defmethod xref-location-marker ((l xref-etags-location))
;;     "Execute `xref-location-marker' for `xref-etags-location'.
;; Cognizant of possibly remote proviso projects.
;; This should override the similar method from `etags.el'."
;;     (with-slots (tag-info file t) l
;;       (let* ((file (if (and (featurep 'proviso)
;;                             proviso-curr-proj)
;;                        (concat
;;                         (proviso-get proviso-curr-proj :remote-prefix)
;;                         file)
;;                      file))
;;              (buffer (find-file-noselect file)))
;;         (with-current-buffer buffer
;;           (save-excursion
;;             (etags-goto-tag-location tag-info)
;;             (point-marker))))))

  (defun proviso-xref-make-peek-frame (fun &rest args)
    "Make a new frame to peek at definition of FUN, with ARGS.
Based off @tuhdo, cf. http://tuhdo.github.io/emacs-frame-peek.html
and `smart-jump-peek', @see `smart-jump-make-peek-frame'."
    (let* ((abs-pixel-pos
            (save-excursion
              (beginning-of-thing 'symbol)
              (window-absolute-pixel-position)))
           (x (car abs-pixel-pos))
           (y (+ (cdr abs-pixel-pos)
                 (frame-char-height)))
           (frame (make-frame '((minibuffer . nil)
                                (name . " *xref-peek*")
                                (width . 80)
                                (visibility . nil)
                                (height . 15)
                                (border-width . 0)
                                (min-width . t)
                                (min-height . t)
                                (internal-border-width . 0)
                                (vertical-scroll-bars . nil)
                                (horizontal-scroll-bar . nil)
                                (left-fringe . 0)
                                (right-fringe . 0)
                                (tool-bar-lines . 0)
                                (line-spacing . 0)
                                (unsplittable . t)
                                (no-other-frame . t)
                                (no-special-glyphs . t))))
           summary)
      ;; position new frame right under beginning of symbol
      (set-frame-position frame x y)
      ;; jump to symbol
      (with-selected-frame frame
        (apply fun args)
        (read-only-mode)
        (recenter-top-bottom 0))
      (make-frame-visible frame)))

  (defun proviso-xref-peek-definition ()
    "Peek at definition at point."
    (interactive)
    (let ((func 'xref-find-definitions))
      (proviso-xref-make-peek-frame
       func
       (xref--read-identifier "Find definitions of: ")
       )))


  )                                     ;end if emacs 25+

(provide 'proviso-xref)
;;; proviso-xref.el ends here
