;;; proviso-display.el --- display information about proviso
;; Copyright (C) 2017-2019  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Tuesday, May  9, 2017
;; Version: 1.0
;; Modified Time-stamp: <2019-10-03 10:51:06 dan.harms>
;; Modified by: Dan Harms
;; Keywords: tools proviso project display
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
;; A collection of utilities pertaining to displaying status of proviso.
;;

;;; Code:
(require 'proviso-core)
(require 'proviso-dired)
(require 'tabulated-list)

(defface proviso-face-heading '((((background dark)) (:foreground "Yellow"))
                                (t (:foreground "Blue")))
  "Face used to highlight headings."
  :group 'proviso-custom-group)

;;;###autoload
(defun proviso-display-print-project (&optional arg)
  "Print properties of a selected project.
With single universal argument ARG, print a particular property.
With double universal argument, dump the raw project fields."
  (interactive "P")
  (let ((proj (proviso-choose-project)))
    (if proj
        (with-output-to-temp-buffer (format "*Proviso-project: %s*" proj)
          (pp
           (if (eq (prefix-numeric-value arg) 4)
               (proviso-display--print-project-field proj)
             (proviso-display--print-project proj (eq (prefix-numeric-value arg) 16)))))
      (error "No project selected"))))

(defun proviso-display--print-project-field (proj)
  "Print a particular field of project PROJ."
  (let ((fields (proviso-display--gather-project-properties proj))
        field)
    (if fields
        (progn
          (setq field (ivy-read "Property: " fields
                                :caller #'proviso-display--print-project-field))
          (if field
              (concat
               (format "Project %s: %s " proj field)
               (proviso-display--print-project-property proj field))
            (error "No field selected")))
      (error (format "No properties in project %s" proj)))))

(defun proviso-display--print-project (proj &optional raw)
  "Return a string containing a textual representation of PROJ.
If RAW is non-nil, just print the entire raw alist.  Otherwise, a
curated set of fields will be shown."
  (if raw
      (proviso-get-plist proj)
    (let ((seq (proviso-get-plist proj))
          (exclusions '(
                        :project-dirs
                        :project-files
                        :project-files-future
                        :project-dirs-future
                        ))
          lst)
      (while seq
        (unless (memq (car seq) exclusions)
          (push (car seq) lst)
          (push (copy-tree (cadr seq)) lst))
        (setq seq (cddr seq)))
      (nreverse lst))))

(defun proviso-display--gather-project-properties (proj)
  "Return a list of the properties of PROJ.
These are the symbols of the plist."
  (let ((seq (proviso-get-plist proj))
        lst)
    (while seq
      (push (car seq) lst)
      (setq seq (cddr seq)))
    (nreverse lst)))

(defun proviso-display--print-project-property (proj prop)
  "Return the value from project PROJ of property PROP."
  (let ((seq (proviso-get-plist proj)))
    (catch 'found
      (while seq
        (if (string-equal (car seq) prop)
            (throw 'found (format "%S" (cadr seq)))
          (setq seq (cddr seq)))))))

(defun proviso-display--get-project-names (&optional curr maxwidth-name
                                                     maxwidth-dir)
  "Return a list containing the current project names in `proviso-obarray'.
Project CURR, if non-nil, will be highlighted in the results.
MAXWIDTH-NAME is an optional hint to the longest project name to
make things pretty.  MAXWIDTH-DIR is an optional hint as to the
longest root dir."
  (let (lst)
    (mapatoms (lambda (atom)
                (if (equal atom curr)
                    (push
                     (concat "* "
                             (proviso-prettify-project
                              atom maxwidth-name maxwidth-dir))
                     lst)
                  (push
                   (concat "  "
                           (proviso-prettify-project
                            atom maxwidth-name maxwidth-dir))
                   lst)))
              proviso-obarray)
    (if (> (length lst) 0)
        (mapconcat 'identity lst "\n")
      nil)))

;;;###autoload
(defun proviso-display-echo-project-names ()
  "Echo the project names contained in `proviso-obarray'."
  (interactive)
  (let ((maxwidth-name 0)
        (maxwidth-dir 0)
        msg)
    (mapatoms (lambda (atom)
                (if (> (string-width (proviso-get atom :project-name))
                       maxwidth-name)
                    (setq maxwidth-name
                          (string-width (proviso-get atom :project-name))))
                (if (> (string-width (proviso-get atom :root-dir))
                       maxwidth-dir)
                    (setq maxwidth-dir
                          (string-width (proviso-get atom :root-dir)))))
              proviso-obarray)
    (if (setq msg (proviso-display--get-project-names
                   (proviso-current-project) maxwidth-name maxwidth-dir))
        (message msg)
      (error "No projects"))))

;;;###autoload
(defun proviso-display-echo-current-project-name ()
  "Echo the current active project name, if any."
  (interactive)
  (let ((proj (proviso-current-project)))
    (if proj
        (message "%s" (proviso-prettify-project proj))
      (user-error "No current project"))))

(defvar proviso-display-buffer-name "*proviso-projects*"
  "Name of the buffer describing proviso projects.")

(defun proviso-display-entries ()
  "Generate entry list for tabulated-list."
  (let ((curr (proviso-current-project))
        lst)
    (mapatoms (lambda (atom)
                (push (symbol-name atom) lst)) proviso-obarray)
    (append
     (mapcar (lambda (elt)
               (let ((name (proviso-get elt :project-name))
                     (root (proviso-get elt :root-dir))
                     (remote (or (proviso-get elt :remote-host) "")))
                 (list elt
                       (vconcat
                        (list
                         (propertize name 'face
                                     (if (string= elt (symbol-name curr))
                                         '(underline)
                                       '()))
                         root
                         remote
                         )))))
             (sort lst (lambda (one two)
                         (string< one two))))
     (mapcar (lambda (elt)
               (list elt
                     (vconcat
                      (list
                       (propertize (nth 1 elt) 'face '(shadow italic))
                       (propertize (nth 0 elt) 'face '(shadow italic))
                       ""))))
             (sort (copy-tree proviso-path-alist) (lambda (one two)
                                        (string< (nth 1 one) (nth 1 two))))))))

(define-derived-mode proviso-display-mode
  tabulated-list-mode "Proviso"
  "Major mode for displaying proviso projects.
\\{proviso-display-mode-map\}"
  (setq tabulated-list-format [("Project" 15 t)
                               ("Root" 55 t)
                               ("Remote" 30 t)
                               ])
  (setq tabulated-list-sort-key nil)
  (setq tabulated-list-padding 2)
  (setq tabulated-list-entries #'proviso-display-entries)
  (tabulated-list-init-header))

;;;###autoload
(defun proviso-display-projects ()
  "Display proviso projects."
  (interactive)
  (let ((buf (get-buffer-create proviso-display-buffer-name)))
    (pop-to-buffer buf)
    (proviso-display-mode)
    (tabulated-list-print)))

(define-key proviso-display-mode-map "d" #'proviso-display-open-root-dired)
(define-key proviso-display-mode-map "x" #'proviso-display-erase-project)

(defun proviso-display-open-root-dired ()
  "Open a dired buffer at the root location of the selected project."
  (interactive)
  (let* ((name (tabulated-list-get-id))
         (proj (intern-soft name proviso-obarray))
         dir)
    (when proj
      (proviso-dired-open-project proj))))

(defun proviso-display-erase-project ()
  "Erase all traces of the selected project."
  (interactive)
  (let* ((name (tabulated-list-get-id))
         (proj (intern-soft name proviso-obarray)))
    (when proj
      (proviso-hard-reset proj)
      (tabulated-list-delete-entry)
      )))

(provide 'proviso-display)
;;; proviso-display.el ends here
