;;; proviso-core.el --- Core functionality for proviso.
;; Copyright (C) 2017-2021, 2023  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Monday, March 27, 2017
;; Version: 1.0
;; Modified Time-stamp: <2023-10-13 10:34:22 dharms>
;; Modified by: Dan Harms
;; Keywords: tools proviso projects
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
;; Core utilities used by proviso.
;;

;;; Code:
(require 'xfer-util)
(require 'ivy)
(require 'seq)
(require 'tramp)
(require 'parsenv nil t)
(require 'ht)

(defvar proviso-obarray
  (let ((intern-obarray (make-vector 10 0)))
    intern-obarray)
  "Array of project objects.")

(defvar proviso-provisional-obarray
  (let ((intern-obarray (make-vector 10 0)))
    intern-obarray)
  "Array of provisional project objects.")

(defvar proviso-path-alist nil
  "Alist of lists mapping potential projects.
Each element is a list of the form (REGEXP PROJECT NAME).  REGEXP
is used to match the path.  PROJECT is the resulting project.
NAME is an optional NAME to apply to the project for display
purposes; if NAME is nil, PROJECT is used for the display name.")

(defvar proviso-remote-projects
  (ht-create 'equal)
  "Registry of active remote projects keyed by host.")

(defvar proviso-proj-alist nil
  "Active projects on localhost.
Alist of pairs of strings (DIR . PROJECT#DIR[@REMOTE]).
Maps filepaths to projects already loaded.")

(defvar-local proviso-local-proj nil "Current buffer's project.")
(defvar proviso-curr-proj nil "Last known global project.")

(defun proviso-current-project ()
  "Return the current project, or if none, the last used."
  (if proviso-local-proj proviso-local-proj proviso-curr-proj))

(defun proviso-current-project-root ()
  "Return the root directory of the current or last-known project.
If there is no active project, nil is returned."
  (interactive)
  (cond (proviso-local-proj (proviso-get proviso-local-proj :root-dir))
        (proviso-curr-proj (proviso-get proviso-curr-proj :root-dir))))

(defun proviso-current-project-name ()
  "Return the name of the current project, if any."
  (interactive)
  (let ((proj (proviso-current-project)))
    (when proj
      (proviso-get proj :project-name))))

(defun proviso-find-project (dir)
  "Return project associated with DIR.
Melds `proviso' functionality into Emacs' `project'."
  (let ((proj (proviso-find-active-project
               (if (file-remote-p dir)
                   (file-remote-p dir 'localname)
                 (expand-file-name dir))
               (file-remote-p dir 'host))))
    (when proj
      (cons 'proviso (proviso-get proj :root-dir)))))

(cl-defmethod project-roots ((proj (head proviso)))
  "Return list of directory roots of PROJ."
  (list (cdr proj)))                    ;TODO: be smarter

(cl-defmethod project-root ((proj (head proviso)))
  "Return root of PROJ."
  (cdr proj))

(cl-defmethod project-files ((proj (head proviso)) &optional _dirs)
  "Return list of project files from PROJ."
  (let ((p (proviso-find-active-project (cdr proj))))
    (when p
      (let ((files (proviso-get p :project-files)))
        (unless files
          (unless (async-ready (proviso-get p :project-files-future))
            (message "Still gathering files..."))
          (when (setq files (async-get (proviso-get p :project-files-future)))
            (proviso-put p :project-files files)))
        (mapcar 'car files)))))

(cl-defmethod project-ignores ((proj (head proviso)) _dir)
  "Return list of ignore patterns from PROJ."
  (let ((p (proviso-find-active-project (cdr proj))))
    (proviso-get p :grep-exclude-dirs)))


;; Project Properties:
;;   - External:
;;  :env-file
;;  :include-files
;;  :proj-alist
;;  :ff-search-directories: literal additions to ff-search-directories
;;  :include-ff-files: dirs treated as relative to root
;;  :include-ff-recurse-dirs: dirs (relative to root) will be traversed recursively
;;  :ctags-additions
;;  :build-subdirs :debug-subdirs
;;  :compile-cmds :compile-defun
;;  :compile-cmds-comint-filters
;;  :gcc-standard :clang-standard
;;  :search-include-files
;;  :search-exclude-files
;;  :search-exclude-dirs
;;  :clang-format
;;  :docker-container
;;   - Internal:
;; :root-dir :project-name :project-uid
;; :scratch-dir :local-scratch-dir
;; :inited :initfun :init-errors
;; :remote-prefix :remote-host :root-stem
;; :grep-include-files :grep-exclude-files
;; :grep-exclude-dirs
;; :registers
;; :tags-alist :tags-dir :tags-lastgen :tags-remote-dir
;; :project-files :project-dirs
;; :file-cache
;; :deployments :deploy-file :last-deploy :deploy-id :deploy-buffer
;; :docker-mount-src :docker-mount-dst

;; hooks
(defvar proviso-hook-on-project-pre-init '()
  "Hooks run just before a project is first initialized.
Hook functions are called with one parameter, the new project.")
(defvar proviso-hook-on-project-init '()
  "Hooks run as a project is first initialized.
Hook functions are called with one parameter, the new project.")
(defvar proviso-hook-on-project-post-init '()
  "Hooks run just after a project is first initialized.
Hook functions are called with one parameter, the new project.")
(defvar proviso-hook-on-project-active '()
  "Hooks run whenever a project becomes active.
Hook functions are called with two parameters: the new project,
and the old one: `lambda(new old)()'.")
(defvar proviso-hook-on-file-opened '()
  "Hooks run whenever a file is opened that belongs to a project.
Hook functions are called with one parameter: the file's major mode.")
(defvar proviso-hook-file-transformers '()
  "Hooks run in certain scenarios to transform filenames.
Hook functions are called with one parameter: the original filename.
They return the possibly transformed filename.")

(defgroup proviso-custom-group nil
  "Proviso is a collection of coding utilities."
  :group 'coding)

(defcustom proviso-prefix-key
  "\C-cp"
  "Prefix key for `proviso'."
  :group 'proviso-custom-group)

(defun proviso-proj-p (proj)
  "Return non-nil if PROJ is an active project.
PROJ is not simply a basename but includes a path."
  (intern-soft proj proviso-obarray))

(defun proviso-provisional-proj-p (proj)
  "Return non-nil if PROJ is a provisional project."
  (intern-soft proj proviso-provisional-obarray))

(defvar proviso-project-signifiers '("\\.proviso$"
                                     "\\.git$"
                                     "\\.svn$"
                                     ;; other possibilities:
                                     ;; .hg .projectile Makefile .bzr
                                     )
  "A list of patterns that signify project roots.")

(defun proviso-define-project (project forms &rest plist)
  "Create a provisional project named PROJECT with path FORMS.
FORMS is an alist with each element of the format (PATH . NAME),
where PATH is a regexp to match a file path, and NAME is an
optional name given to the resultant project.  If NAME is not
supplied, the project name itself is used.  To the project is
added the property list PLIST."
  (let ((proj (intern project proviso-provisional-obarray)))
    (when plist
      (setplist proj plist))
    (dolist (form forms)
      (setq proviso-path-alist
            (cons (append
                   (list (car form)
                         project)
                   (if (cdr form)
                       (list (cdr form))
                     nil))
                  proviso-path-alist)))
    proj))

(defun proviso-define-project-derived (project parent forms &rest plist)
  "Create a provisional project PROJECT with parent PARENT at path FORMS.
FORMS is an alist with each element of the format (PATH . NAME),
where PATH is a regexp to match a file path, and NAME is an
optional name given to the resultant project.  If NAME is not
supplied, the project name itself is used.  To the project is
added the property list PLIST."
  (let ((parent (intern-soft parent proviso-provisional-obarray))
        (child (intern project proviso-provisional-obarray)))
    (setplist child
              (append
               (list :parent parent)
               plist))
    (dolist (form forms)
      (setq proviso-path-alist
            (cons (append
                   (list (car form)
                         project)
                   (if (cdr form)
                       (list (cdr form))
                     nil))
                  proviso-path-alist)))
    child))

(defun proviso-define-active-project (project &optional plist)
  "Create or replace an active project named PROJECT.
Add to it the property list PLIST."
  (let ((proj (intern project proviso-obarray)))
    (when plist
      (setplist proj plist))
    proj))

(defun proviso-get-plist (project &optional provisional)
  "Return the plist associated with project PROJECT.
If optional PROVISIONAL is non-nil, look for a provisional project."
  (let ((ob (if provisional proviso-provisional-obarray proviso-obarray)))
    (symbol-plist (intern-soft project ob))))

(defun proviso-put (project property value &optional provisional)
  "Put into PROJECT the PROPERTY with value VALUE.
If optional PROVISIONAL is non-nil, find a provisional project."
  (let* ((ob (if provisional proviso-provisional-obarray proviso-obarray))
         (proj (intern-soft project ob)))
    (if proj (put proj property value))))

(defun proviso-get (project property &optional provisional inhibit-polymorphism)
  "Get from PROJECT the value associated with PROPERTY.
If optional PROVISIONAL is non-nil, find a provisional project.
INHIBIT-POLYMORPHISM, if non-nil, will constrain lookup from
searching in any bases."
  (let* ((ob (if provisional proviso-provisional-obarray proviso-obarray))
         (proj (intern-soft project ob))
         parent parentname)
    (when proj
      (or (get proj property)
          (and (not inhibit-polymorphism)
               (setq parentname (get proj :parent))
               (if (setq parent (intern-soft parentname ob))
                   (proviso-get parent property provisional)
                 (if (setq parent (intern-soft parentname
                                               (if provisional proviso-obarray
                                                 proviso-provisional-obarray)))
                     (proviso-get parent property (not provisional)))))))))

(defun proviso--get-provisonal-match-data (orig data)
  "Return new string and match data to match ORIG string with DATA.
Returned is a list '(newstring data) that contains all the
matched sub-expressions contained within ORIG, according to
`match-data' DATA."
  (let ((sz (length data))
        (i 2)
        beg end substr
        str match)
    (while (<= (1+ i) sz)
      (setq beg (nth i data))
      (setq end (nth (1+ i) data))
      (if (and beg
               end
               (setq substr (substring-no-properties orig beg end)))
          (progn
            (push (length str) match)
            (push (+ (length str) (length substr)) match)
            (setq str (concat str substr)))
        (push nil match)
        (push nil match))
      (setq i (+ i 2)))
    (list str
          (append (list 0 (length str))
                  (nreverse match)))))

(defun proviso-find-provisional-project (&optional filename)
  "Scan `proviso-path-alist' for an entry to match FILENAME.
If found, returns a list '(PATH PROJECT NAME)."
  (let ((file (or filename (buffer-file-name) (buffer-name))))
    (catch 'exit
      (progn
        (mapc (lambda (elt)
                (let ((path (proviso-substitute-env-vars (nth 0 elt)))
                      (project (proviso-substitute-env-vars (nth 1 elt)))
                      (name (proviso-substitute-env-vars (nth 2 elt))))
                  (if (string-match path file)
                      (throw 'exit
                             (list
                              (substring file 0 (match-end 0))
                              project
                              (seq-let [str md]
                                  (proviso--get-provisonal-match-data file (match-data))
                                (if (and str md name)
                                    (progn
                                      (set-match-data md)
                                      (replace-match name t nil str 0))
                                  project)))))))
              proviso-path-alist)
        nil))))

(defun proviso-find-active-project (dir &optional host)
  "Return an active project for DIR.
HOST defaults to nil for localhost."
  (if host
      (let ((alist (ht-get proviso-remote-projects host)))
        (when alist
          (proviso-find-active-proj--alist dir alist)))
    (proviso-find-active-proj--alist dir proviso-proj-alist)))

(defun proviso-find-active-proj--alist (dir alist)
  "Find any active project in ALIST with root DIR."
  (assoc-default dir alist 'string-match))

(defun proviso-add-active-project-path (dir uid &optional host)
  "Add project UID centered at absolute path DIR.
HOST defaults to nil for localhost.
If the project already existed, nil is returned, else t."
  (let (result)
    (if host
        (let ((alist (ht-get proviso-remote-projects host)))
          (unless (eq t (setq result
                              (proviso-add-active-proj-path--alist alist dir uid)))
            (ht-set! proviso-remote-projects host result)))
      (unless (eq t (setq result
                          (proviso-add-active-proj-path--alist proviso-proj-alist dir uid)))
        (setq proviso-proj-alist result)))
    (if (eq result t) nil result)))

(defun proviso-add-active-proj-path--alist (alist dir uid)
  "Add definition for project UID at DIR to ALIST.
If the project already exists, t is returned."
  (if (seq-contains-p alist (cons dir uid)
                      ;; first check if this project already exists; need to look
                      ;; in the cdr of each alist entry for the full unique name
                      (lambda (a b) (string= (cdr a) (cdr b))))
      t
    (cons (cons dir uid) alist)))

(defun proviso-create-project-uid (project dir &optional host)
  "Return a unique ID identifying PROJECT located at DIR on HOST.
HOST defaults to nil for localhost."
  (let ((uid (concat project "#" dir)))
    (if host
        (concat uid "@" host)
      uid)))

(define-error 'proviso-error "Project error")
(define-error 'proviso-error-non-fatal "Project load stopped" 'proviso-error)
(define-error 'proviso-error-aborted "Project load aborted" 'proviso-error)

(defvar proviso--ignore-load-errors nil
  "Internal variable is non-nil if user desires errors to be skipped.")

(defun proviso--query-error (project err)
  "While loading PROJECT, error ERR has occurred; ask the user what to do."
  (interactive)
  (unless (stringp err) (setq err (prin1-to-string err)))
  (when noninteractive
    (message "proviso--query-error: %s" err))
  (let ((buf (get-buffer-create "*Proviso Error*")))
    (with-current-buffer buf
      (erase-buffer)
      (toggle-truncate-lines -1)
      (insert "An error occurred while loading project \""
              (propertize (symbol-name project) 'face 'bold)
              "\":\n\n"
              (propertize err 'face '(bold error))
              "\n\nWould you like to continue loading this project?  "
              "Please select:\n\n "
              (propertize "[y]" 'face '(bold warning))
              " Continue loading project, ignoring this error\n "
              (propertize "[!]" 'face '(bold warning))
              " Continue loading project, ignoring this and future errors\n "
              (propertize "[n]" 'face '(bold warning))
              " Stop loading project\n "
              (propertize "[a]" 'face '(bold warning))
              " Abort loading of project, and revert project load\n"
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
             (proviso-hard-reset project)
             (signal 'proviso-error-aborted
                     (format "Aborted (and reset) project \"%s\" (%s)"
                             (symbol-name project) err)))
            ((eq ch ?!)
             (setq proviso--ignore-load-errors t))
            ))
    nil))

(defun proviso--safe-funcall (proj property &rest rem)
  "Call a function from project PROJ stored in its PROPERTY tag.
The function is called with arguments REM, if the function exists
and is bound."
  (let ((func (proviso-get proj property)))
    (and func (or (and (symbolp func)
                       (fboundp func))
                  (functionp func))
         (funcall func (car rem)))))

(defun proviso-soft-reset ()
  "Reset the current project.
This does not otherwise remove any projects from memory."
  (interactive)
  ;; kill-local-variable insufficient due to permanent-local property
  (setq proviso-curr-proj nil)
  (setq proviso-local-proj (default-value 'proviso-local-proj)))

(defun proviso-hard-reset (project)
  "Remove all traces of PROJECT."
  (interactive)
  (proviso--remove-proviso-from-alist
   project
   (proviso-get project :remote-host))
  (proviso--remove-proj project)
  (proviso-soft-reset))

(defun proviso--remove-proj (project)
  "Delete the project PROJECT, which can be a symbol or string (name)."
  (unintern project proviso-obarray))

(defun proviso--remove-proviso-from-alist (project &optional host)
  "Remove project PROJECT from the internal data structure.
HOST is an optional remote host."
  (if host
      (if-let ((alist (ht-get proviso-remote-projects host)))
          (progn
            (ht-set! proviso-remote-projects host
                     (seq-remove
                      (lambda (elt)
                        (string-equal (cdr elt) project))
                      alist))))
    (setq proviso-proj-alist
          (seq-remove
           (lambda (elt)
             ;; string-equal handles a symbol using its print-name
             (string-equal (cdr elt) project))
           proviso-proj-alist))))

(defun proviso-substitute-env-vars (str)
  "Substitute environment variables contained in STR.
Differs with `substitute-env-vars' because multiple embedded
layers of substitutions are allowed."
  (if str
      (let ((s (substitute-env-vars str t)))
        (while (not (string-equal s str))
          (setq str s)
          (setq s (substitute-env-vars s t)))
        str)))

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

(defun proviso--find-root-helper (dir pattern)
  "Look for project files upward from DIR matching PATTERN.
Returns a list (ROOT FILE)."
  (let (root file)
    (setq root
          (if (functionp 'locate-dominating-file)
              (locate-dominating-file
               dir
               (lambda (parent)
                 (let ((files (directory-files
                               parent t
                               (concat "\\sw*" pattern)
                               t)))
                   (setq file (car
                               (seq-remove
                                (lambda (f)
                                  (and
                                   (string-match-p "\\.git$" f)
                                   (not (file-directory-p f))
                                   ;; a .git file (not directory): this should
                                   ;; be sufficient to ignore submodules.
                                   ))
                                files))))))
            (proviso-find-file-upwards
             dir
             (concat "\\sw*" pattern))))
    (list root file)))

(defun proviso--find-root (dir &optional absolute)
  "Search for the project root, starting from DIR and moving up the file tree.
Returns a list (file dir) containing the project file and its parent
directory, if found, else nil.  If ABSOLUTE is non-nil, the path, if found,
will be absolute.  Project files can look like any of the following:
    1) .proviso
    2) proj.proviso
    3) .proj.proviso
    4) .git          (please note git submodules are explicitly ignored)
    5) .svn
These are tried in order until one is matched.  Note that the car, FILE, may
be a directory."
  (catch 'found
    (dolist (pattern proviso-project-signifiers)
      (seq-let [root file] (proviso--find-root-helper dir pattern)
        (and root file
             (throw 'found
                    (list file
                          (if absolute (expand-file-name root) root))))))))

(defun proviso-compute-basename-from-file (name)
  "Compute the basename of the project located at NAME.
We first attempt to derive a name from the base of the file.
Otherwise we look at the current directory.  For example, the
following examples would all yield `sample':
    1)  ~/first/second/sample.proviso
    2)  /home/user/third/.sample.proviso
    3)  ~/sample/.proviso
    4)  ~/sample/.git
See also `proviso-project-signifiers'."
  (let ((base (file-name-nondirectory name)))
    (if (string-match
         (concat "\\.?\\(.+\\)"
                 (car proviso-project-signifiers))
         base)
        (match-string-no-properties 1 base)
      (file-name-nondirectory
       (directory-file-name
        (file-name-directory name))))))

(defun proviso-compute-proviso-dir (&optional remote)
  "Compute the proviso directory.
REMOTE, if non-nil, signifies a remote path of interest."
  (let ((home (or (and remote (xfer-util-remote-homedir-find remote))
                  (expand-file-name "~")))
        (base (or (getenv "PROVISO_BASE")
                  ".proviso.d")))
    (concat (file-name-as-directory home)
            (file-name-as-directory base))))

(defun proviso--compute-scratch-dir (root-dir &optional remote-host remote-prefix local)
  "Compute scratch dir for a project at ROOT-DIR.
Remote files may optionally include host REMOTE-HOST and REMOTE-PREFIX.
LOCAL signifies the scratch dir should be local, even when
there's a remote host.  This may be a local dir tracking a remote
location, or a writeable dir tracking a non-writeable one."
  (let ((sub "projects/"))
    (file-name-as-directory
     (concat (unless local remote-prefix)
             (proviso-compute-proviso-dir (and (not local)
                                               remote-prefix
                                               (concat remote-prefix root-dir)))
             sub
             (when (and local remote-host)
               (concat remote-host "@"))
             (replace-regexp-in-string "/\\|\\\\" "!" root-dir)))))

(defun proviso-compute-projectdef-dir ()
  "Compute the directory where project definitions are kept."
  (concat (proviso-compute-proviso-dir) "projectdefs/"))

(defun proviso-core-remote-executable-find (exe)
  "Try to find the binary associated with EXE on a remote host.
Note that `executable-find' operates on the local host."
  (string-trim (shell-command-to-string
                (format "which %s" exe))))

(defun proviso-eval-string (str)
  "Evaluate the contents of STR."
  (let ((i 0) curr)
    (while (< i (1- (length str)))
      (setq curr (read-from-string str i))
      (eval (car curr))
      (setq i (cdr curr)))))

(defun proviso-prettify-project (proj &optional maxwidth-name
                                      maxwidth-dir)
  "Return a prettified description of PROJ.
MAXWIDTH-NAME is an optional max width for the name parameter.
MAXWIDTH-DIR is an optional max width for the dir parameter."
  (let ((name (proviso-get proj :project-name))
        (dir (proviso-get proj :root-dir))
        (host (proviso-get proj :remote-host)))
    (unless maxwidth-name
      (setq maxwidth-name (string-width name)))
    (unless maxwidth-dir
      (setq maxwidth-dir (string-width dir)))
    (concat
     (propertize
      (format
       (concat "%-"
               (format "%d" maxwidth-name)
               "s")
       name)
      'face '(bold))
     " "
     (propertize
      (format
       (concat "%-"
               (format "%d" maxwidth-dir)
               "s")
       (abbreviate-file-name dir))
      'face '(italic))
     (when host
       (propertize
        (concat " @" host) 'face '(shadow))))))

;;;###autoload
(defun proviso-choose-project (&optional prompt-string)
  "Allow the user to choose a project among those currently defined.
PROMPT-STRING allows customizing a special prompt."
  (interactive)
  (let ((prompt (or prompt-string "Project: "))
        (maxwidth-name 0)
        (maxwidth-dir 0)
        lst)
    (mapatoms (lambda (atom)
                (if (> (string-width (proviso-get atom :project-name))
                       maxwidth-name)
                    (setq maxwidth-name (string-width (proviso-get atom :project-name))))
                (if (> (string-width (proviso-get atom :root-dir))
                       maxwidth-dir)
                    (setq maxwidth-dir (string-width (proviso-get atom :root-dir)))))
              proviso-obarray)
    (mapatoms (lambda (atom)
                (push (cons
                       (proviso-prettify-project atom maxwidth-name maxwidth-dir)
                       atom)
                      lst))
              proviso-obarray)
    (if (seq-empty-p lst)
        (user-error "No projects defined")
      (catch 'exit
        (ivy-read prompt lst
                  :action (lambda (x)
                            (throw 'exit (cdr x)))
                  :caller 'proviso-choose-project
                  )))))

;;;###autoload
(defun proviso-load-environment-variables-from-file (name)
  "Load environment variables in root dir of PROJ from file NAME.
Nothing is done if no such file exists in the root director of PROJ."
  (interactive "fLoad environment variables from file: ")
  (proviso-load-environment-file name))

(defun proviso-load-environment-file-from-project (proj)
  "Load an environment variable from a file for project PROJ."
  (let* ((remote (proviso-get proj :remote-prefix))
         (root (proviso-get proj :root-dir))
         (file (or (proviso-get proj :env-file) ".env")))
    (setenv "REPO_ROOT" (directory-file-name root))
    (proviso-load-environment-file
     (if (file-name-absolute-p file)
         (concat remote (expand-file-name file))
       (concat remote root file)))))

(defun proviso-load-environment-file (file)
  "Load environment variables from FILE, if it exists."
  (when (and (featurep 'parsenv)
             (file-exists-p file))
    (parsenv-load-env file)
    (message "Loaded environment file %s" file)))

(provide 'proviso-core)
;;; proviso-core.el ends here
