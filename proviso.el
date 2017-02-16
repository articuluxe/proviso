;;; proviso.el --- manage profiles
;; Copyright (C) 2016-2017  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Thursday, November  3, 2016
;; Version: 1.0
;; Modified Time-stamp: <2017-02-16 08:29:35 dharms>
;; Modified by: Dan Harms
;; Keywords: profiles project

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
;; This project is based in part on profiles.el by Sylvain Bougerel, from
;; 2009.

;;

;;; Code:

(require 'proviso-tags)
(require 'proviso-sml)

(eval-when-compile
  (require 'cl))
(require 'f)
(require 'seq)
(require 'switch-buffer-functions)
(require 'tramp)

(defvar proviso-obarray
  (let ((intern-obarray (make-vector 7 0)))
    (intern "default" intern-obarray)
    intern-obarray)
  "Array of profile objects.")
(defvar proviso-path-alist '()
  "Alist of pairs of strings (REGEXP . PROFILE-NAME).
A profile is used for a file if the filename matches REGEXP.  In the case
of no matches, the default profile is instead used.")
(defvar proviso-local (intern-soft "default" proviso-obarray))
(defvar proviso-current nil)

;; Profile Properties:
;;  - External:
;;  - Internal:
;; :root-dir :project-name :inited :initfun
;; :remote-prefix :remote-host :root-stem

;; hooks
(defvar proviso-on-profile-pre-init '()
  "Hooks run just before a profile is first initialized.
Hook functions are called with one parameter, the new profile.")
(defvar proviso-on-profile-post-init '()
  "Hooks run just after a profile is first initialized.
Hook functions are called with one parameter, the new profile.")
(defvar proviso-on-profile-loaded '()
  "Hooks run whenever a profile becomes active.
Hook functions are called with two parameters: the new profile,
and the old one: `lambda(new old)()'.")

(defun proviso-p (prof)
  "Return non-nil if PROF is a profile."
  (intern-soft prof proviso-obarray))

(defvar proviso--last-proviso-defined nil
  "The most recent profile to be defined.")

(defvar proviso--ext "proviso"
  "The extension for profile files.")

(defun proviso-define (profile &rest plist)
  "Create or replace a profile named PROFILE.
Add to it the property list PLIST."
  (let ((p (intern profile proviso-obarray)))
    (setplist p plist)
    (setq proviso--last-proviso-defined p)))

(defun proviso-define-derived (profile parent &rest plist)
  "Create or replace a profile named PROFILE.
Its parent is PARENT.  Add to it the property list PLIST."
  (let ((p (intern profile proviso-obarray)))
    (setplist p (append (list :parent parent) plist))
    (setq proviso--last-proviso-defined p)))

(defun proviso-put (profile property value)
  "Put into PROFILE the PROPERTY with value VALUE."
  (let ((p (intern-soft profile proviso-obarray)))
    (if p (put p property value)
      (error "Invalid profile %s" profile))))

(defun proviso-get (profile property &optional inhibit-polymorphism)
  "Get from PROFILE the value associated with PROPERTY.
INHIBIT-POLYMORPHISM, if non-nil, will constrain lookup from
searching in any bases."
  (let ((p (intern-soft profile proviso-obarray))
        parent parentname)
    (when p
      (or (get p property)
          (and (not inhibit-polymorphism)
               (setq parentname (get p :parent))
               (setq parent (intern-soft parentname proviso-obarray))
               (proviso-get parent property))))))

(defun proviso-find-path-alist (&optional filename)
  "Scan `proviso-path-alist' for an entry to match FILENAME."
  (assoc-default
   (or filename (buffer-file-name) (buffer-name))
   proviso-path-alist 'string-match))

(defun proviso-name-p (prof)
  "Return non-nil if PROF names an active profile."
  (seq-find (lambda (elt)
              (string= (cdr elt) prof)) proviso-path-alist))

(define-error 'proviso-error "Profile error")
(define-error 'proviso-error-non-fatal "Profile load stopped" 'proviso-error)
(define-error 'proviso-error-aborted "Profile load aborted" 'proviso-error)

(defvar proviso--ignore-load-errors nil
  "Internal variable is non-nil if user desires errors to be skipped.")

(defun proviso--query-error (profile err)
  "While loading PROFILE, error ERR has occurred; ask the user what to do."
  (interactive)
  (let ((buf (get-buffer-create "*Profile Error*")))
    (with-current-buffer buf
      (erase-buffer)
      (toggle-truncate-lines -1)
      (insert "An error occurred while loading profile \""
              (propertize (symbol-name profile) 'face 'bold)
              "\":\n\n"
              (propertize err 'face '(bold error))
              "\n\nWould you like to continue loading this profile?  "
              "Please select:\n\n "
              (propertize "[y]" 'face '(bold warning))
              " Continue loading profile, ignoring this error\n "
              (propertize "[!]" 'face '(bold warning))
              " Continue loading profile, ignoring this and future errors\n "
              (propertize "[n]" 'face '(bold warning))
              " Stop loading profile\n "
              (propertize "[a]" 'face '(bold warning))
              " Abort loading of profile, and revert profile load\n"
              ))
    (pop-to-buffer buf)
    (let ((choices '(?y ?n ?a ?!))
          (prompt "Please type y, n, ! or a: ")
          ch)
      (while (null ch)
        (setq ch (read-char-choice prompt choices)))
      (quit-window t)
      (cond ((eq ch ?n)
             (signal 'proviso-error-non-fatal err))
            ((eq ch ?a)
             (proviso-hard-reset profile)
             (signal 'proviso-error-aborted
                     (format "Aborted (and reset) profile \"%s\" (%s)"
                             (symbol-name profile) err)))
            ((eq ch ?!)
             (setq proviso--ignore-load-errors t))
            ))
    nil))

(defun proviso--safe-funcall (prof property &rest rem)
  "Call a function from profile PROF stored in its PROPERTY tag.
The function is called with arguments REM, if the function exists
and is bound."
  (let ((func (intern-soft
               (proviso-get prof property))))
    (and func (fboundp func) (funcall func rem))))

(defun proviso-soft-reset ()
  "Reset the current profile.
This does not otherwise remove any profiles from memory."
  (interactive)
  ;; kill-local-variable insufficient due to permanent-local property
  (setq proviso-current nil)
  (setq proviso-local (default-value 'proviso-local)))

(defun proviso-hard-reset (&optional profile)
  "Remove all traces of PROFILE."
  (interactive)
  (proviso--remove-proviso-from-alist profile)
  (proviso--remove-prof profile)
  (proviso-soft-reset))

(defun proviso--remove-prof (profile)
  "Delete the profile PROFILE, which can be a symbol or string (name)."
  (unintern profile proviso-obarray))

(defun proviso--remove-proviso-from-alist (profile)
  "Remove profile PROFILE from the internal data structure."
  (setq proviso-path-alist
        (seq-remove
         (lambda (elt)
           ;; string-equal handles a symbol using its print-name
           (string-equal (cdr elt) profile))
         proviso-path-alist)))

(defun proviso--validate-include-files (prof)
  "Validate the set of include files of profile PROF."
  (let ((remote (proviso-get prof :remote-prefix))
        (root (proviso-get prof :root-dir))
        (lst (proviso-get prof :dirs))     ;todo
        entry path)
    (setq proviso--ignore-load-errors nil)
    (setq lst
          (seq-filter (lambda (elt)
                        (setq entry (cadr elt)) ;todo
                        (setq path
                              (concat
                               remote
                               (concat
                                (when (or (zerop (length entry))
                                          (f-relative? entry))
                                  root)
                                entry)))
                        (cond ((null entry) nil)
                              ((f-exists? path) path)
                              (proviso--ignore-load-errors nil)
                              (t
                               (proviso--query-error
                                prof
                                (format "%s does not exist!" path)))))
                      lst))
    (proviso-put prof :dirs lst)))         ;todo

(defun proviso-load (prof)
  "Load a profile PROF."
  (condition-case err
      t
    ('proviso-error-non-fatal
     (proviso-put prof :inited nil)
     (message "Stopped loading prof \"%s\" (%s)"
              (symbol-name prof) (cdr err)))
    ((proviso-error-aborted proviso-error)
     (ignore-errors
       (proviso-put prof :inited nil))
     (error (cdr err)))))

(defun proviso-find-file-upwards-helper (path file)
  "Helper function to search upward from PATH for FILE."
  (let* ((parent (file-name-directory path))
         files)
    (cond
     ;; parent of ~ is nil, parent of / is itself
     ;; This terminating condition accounts for both
     ((or (null parent) (equal parent (directory-file-name parent)))
      nil)
     ((setq files (directory-files parent t file))
      (car files))                      ;found
     (t (proviso-find-file-upwards-helper
         (directory-file-name parent) file)))))

(defun proviso-find-file-upwards (dir file)
  "Recursively search upward from DIR for FILE.
Return path to file or nil if not found."
  (interactive)
  (proviso-find-file-upwards-helper (or dir default-directory) file))

(defun proviso-find-file-dir-upwards (file)
  "Recursively search upward for FILE.
Return that file's directory or nil if not found."
  (interactive)
  (let ((file (proviso-find-file-upwards nil file)))
    (when file (file-name-directory file))))

(defun proviso--find-root (dir &optional absolute)
  "Search for the project root, starting from DIR and moving up the file tree.
Returns a cons (file, dir) containing the project file and its parent
directory, if found, else nil.  If ABSOLUTE is non-nil, the path, if found,
will be absolute.  Profile files can look like any of the following:
    1) .proviso
    2) proj.proviso
    3) .proj.proviso"
  (let (root file)
    (setq root
          (if (functionp 'locate-dominating-file)
              (locate-dominating-file
               dir
               (lambda (parent)
                 (setq file
;                       (car (directory-files parent t "\\sw+\\.e?prof$")))))
                       (car (directory-files parent t "\\sw*\\.proviso$")))))
;            (proviso-find-file-upwards dir "\\sw+\\.e?prof$")))
            (proviso-find-file-upwards dir "\\sw*\\.proviso$")))
    (when root
      (if absolute
          (cons file (expand-file-name root))
        (cons file root)))))

(defun proviso--compute-basename-from-file (name)
  "Compute the basename of the profile located at NAME.
We first attempt to derive a name from the base of the file.
Otherwise we look at the current directory.  For example, the
following examples would all yield `sample':
    1)  ~/first/second/sample.proviso
    2)  /home/user/third/.sample.proviso
    3)  ~/sample/.proviso"
  (let ((base (file-name-nondirectory name)))
    (if (string-match
         (concat "\\.?\\(.+\\)\\." proviso--ext)
         base)
        (match-string-no-properties 1 base)
      (file-name-nondirectory
       (directory-file-name
        (file-name-directory name))))))

(defun proviso--compute-remote-props (dir)
  "Compute the remote properties associated with DIR.
DIR may be remote."
  (and dir (file-remote-p dir)
       (with-parsed-tramp-file-name dir file
         `( ,file-host ,file-localname
                       ,(tramp-make-tramp-file-name
                         file-method file-user file-host "")))))

(defun proviso--compute-stem (prof)
  "Compute a profile PROF's stem.
This is useful in regexp-matching.  The profile's root-dir is
probably a relative path, possibly including a `~' that
represents the user's home directory."
  (replace-regexp-in-string "~/" "" (proviso-get prof :root-dir)))

(defun proviso--log-profile-loaded (prof)
  "Log a profile PROF upon initialization."
  (let ((name (symbol-name prof)))
    (unless (string-equal name "default")
      (message "Loaded profile %s (project %s) at %s"
               name
               (proviso-get prof :project-name)
               (proviso-get prof :root-dir)))))

(defun proviso--inited (prof)
  "Initialize a profile PROF."
  )

(defun proviso--loaded (prof)
  "A profile PROF has been loaded.
This may or may not be for the first time."
  (unless (proviso-get prof :inited)
    (proviso-put prof :inited t)
    (run-hook-with-args 'proviso-on-profile-pre-init prof)
    (proviso--safe-funcall prof :initfun)
    (proviso-load prof)
    (run-hook-with-args 'proviso-on-profile-post-init prof)
    (proviso--log-profile-loaded prof)
    )
  (unless (eq prof proviso-current)
    (let ((proviso-old proviso-current))
      (setq proviso-current prof)
      (run-hook-with-args 'proviso-on-profile-loaded prof proviso-old)
      )))

;; (add-hook 'switch-buffer-functions
;;           (lambda (prev curr)
;;             (when (local-variable-p 'proviso-local curr)
;;               (with-current-buffer curr ;todo: is there a better way?
;;                 (setq proviso-current proviso-local)))))

;; (defadvice find-file-noselect-1
;;     (before before-find-file-no-select-1 activate)
;;   (proviso--file-opened-advice buf filename))

(defun proviso--load-file (filename)
  "Load the settings contained within FILENAME."
  (load-file filename))

(advice-add 'find-file-noselect-1 :before 'proviso--file-opened-advice)

(defun proviso--file-opened-advice (buf filename nowarn rawfile truename number)
  "Advice that helps to initialize a profile, if necessary, for BUF, visiting FILENAME."
  (proviso--file-opened buf filename))

(defun proviso--file-opened (buffer filename)
  "Initialize a profile, if necessary, for BUFFER, visiting FILENAME."
  (with-current-buffer buffer
    (make-local-variable 'proviso-local)
    (put 'proviso-local 'permanent-local t)
    (setq proviso-local
          (intern-soft (proviso-find-path-alist
                        (expand-file-name filename))
                       proviso-obarray))
    (let* ((root (proviso--find-root (file-name-directory filename) t))
           (root-file (car root))
           (root-dir (cdr root))
           (remote-props (proviso--compute-remote-props root-dir))
           remote-host remote-localname remote-prefix basename)
      (when remote-props
        (setq remote-host (car remote-props))
        (setq remote-localname (cadr remote-props))
        (setq remote-prefix (caddr remote-props)))
      (when (and root root-file root-dir
;                 (string-match "\\.[er]prof$" root-file)
                 (string-match
                  (concat "\\." proviso--ext "$")
                  root-file)
                 (or (not proviso-local)
                     (not (string-equal root-dir
                                        (proviso-get proviso-local :root-dir)))))
        ;; a new profile, not yet inited
        (setq proviso--last-proviso-defined nil)
        (proviso--load-file root-file)
        ;; project name defaults to filename, unless overridden
        (princ proviso-path-alist)
        (message "drh proviso-path-alist %s" proviso-path-alist)
        (message "drh last profile %s" proviso--last-proviso-defined)
        (setq basename (proviso-get proviso--last-proviso-defined :project-name))
        (message "drh basename %s" basename)
        (unless basename
          (setq basename (proviso--compute-basename-from-file root-file)))
        (message "drh basename 2 %s" basename)
        ;; todo: check for uniqueness; alter if necessary
        ;; (while (proviso-name-p basename)
        (when remote-props
          (setq root-dir remote-localname))
        (push (cons root-dir basename) proviso-path-alist)
        (message "drh proviso-path-alist 2 %s" proviso-path-alist)
        (message "drh looking for %s (%s)"
                 (expand-file-name filename) filename)
        (setq proviso-local
              (intern-soft (proviso-find-path-alist
                            (expand-file-name filename))
                           proviso-obarray))
        (message "drh latest proviso is %s" proviso-local)
        (unless (proviso-get proviso-local :root-dir)
          (proviso-put proviso-local :root-dir root-dir))
        ;; change to absolute if necessary: in case the profile listed
        ;; root-dir as relative
        (when (f-relative? (proviso-get proviso-local :root-dir))
          (proviso-put proviso-local :project-name
                    (f-long (proviso-get proviso-local :root-dir))))
        (unless (proviso-get proviso-local :project-name)
          (proviso-put proviso-local :project-name basename))
        (unless (proviso-get proviso-local :remote-host)
          (proviso-put proviso-local :remote-host remote-host))
        (unless (proviso-get proviso-local :remote-prefix)
          (proviso-put proviso-local :remote-prefix remote-prefix))
        (unless (proviso-get proviso-local :root-stem)
          (proviso-put proviso-local :root-stem
                    (proviso--compute-stem proviso-local)))
        )
      (proviso--loaded proviso-local)
      )))

(provide 'proviso)
;;; proviso.el ends here
