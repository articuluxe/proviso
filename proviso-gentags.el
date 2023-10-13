;;; proviso-gentags.el --- Generate TAGS files
;; Copyright (C) 2015-2023   (dan.harms)
;; Author:  <dan.harms@xrtrading.com>
;; Created: Wednesday, March 18, 2015
;; Version: 1.0
;; Modified Time-stamp: <2023-10-13 12:16:16 dharms>
;; Modified by: Dan Harms
;; Keywords: tools proviso project etags ctags
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
;; Functionality to generate TAGS tables using proviso.
;;

;;; Code:
(require 'proviso-core)
(require 'subr-x)
(require 'find-file)
(eval-when-compile (require 'cl-lib))
(require 'async)
(require 'xfer)

;; customization points
(defun proviso-gentags-exe (remote)
  "Return path to executable to use for TAGS generation.
The remote settings, if any, are described by REMOTE."
  (let ((func (if remote
                  #'proviso-core-remote-executable-find
                #'executable-find)))
    (or (funcall func "exctags")
        (funcall func "ctags")
        "ctags")))

(defvar proviso-gentags-ctags-cpp-kinds "+l"
  "Default ctags cpp-kinds options.
l: local variables
p: function prototypes
N: names imported via `using'.")

(defun proviso-gentags-command (exe &optional rest)
  "Generate the ctags command using executable EXE.
REST, if not nil, is appended."
  (append
   (list exe
         "-Re"
         (concat "--kinds-c++=" proviso-gentags-ctags-cpp-kinds)
         "--extras=+F"
         "--tag-relative=no")
   (if (listp rest) rest (list rest))))

(defvar proviso-gentags-max-jobs-local 10
  "Maximum allowed concurrently spawned processes for local projects.")
(defvar proviso-gentags-max-jobs-remote 8
  "Maximum allowed concurrently spawned processes for remote projects.")
(defvar-local proviso-gentags--max-jobs proviso-gentags-max-jobs-local
  "Maximum number of spawned processes allowed at once.")
(defvar-local proviso-gentags--waiting-jobs nil
  "List of commands that will be spawned to generate tags.")
(defvar-local proviso-gentags--procs nil
  "Spawned processes to generate tags.")
(defvar-local proviso-gentags--num-working-jobs 0
  "Number of spawned processes currently working.")
(defvar-local proviso-gentags--start-time nil
  "Per-process start time that tags generation began.")

;;;###autoload
(defun proviso-gentags-generate-tags (&optional arg)
  "Generate tags according to the current project.
If ARG is 16 (C-u C-u), another project can be chosen.  If ARG is
4 (1 universal argument), the behavior of copying
remotely-generated files will be toggled."
  (interactive "P")
  (let ((proj (if (= (prefix-numeric-value current-prefix-arg) 16)
                  (proviso-choose-project) (proviso-current-project))))
    (unless proj
      (user-error "Could not generate tags: no active project"))
    (proviso-gentags--start-gen
     proj
     current-prefix-arg)))

(defun proviso-gentags--start-gen (proj &optional arg)
  "Generate tags for project PROJ.
Normally if the project is remote, then tags will also be
automatically copied to a local destination.  Also, normally tags
will be generated anew, but it is also possible to just copy over
existing files.  ARG allows customizing these options
interactively."
  (let* ((tags-alist (proviso-get proj :proj-alist))
         (tags-adds (proviso-get proj :ctags-additions))
         (remote (proviso-get proj :remote-prefix))
         (do-gen t)
         (copy-remote remote)
         (exe (if (and remote
                       (string-match-p "\\s-" remote))
                  (concat "\""
                          (proviso-gentags-exe remote)
                          "\"")
                (proviso-gentags-exe remote)))
         (tags-dir (proviso-get proj :tags-dir))
         (int-dir (proviso-get proj :tags-remote-dir))
         (root (proviso-get proj :root-dir))
         lst)
    (when arg
      (setq do-gen (y-or-n-p "Generate TAGS (otherwise use existing)? "))
      (when remote
        (setq copy-remote (y-or-n-p "Copy remote files? "))))
    (make-directory tags-dir t)
    (when int-dir (make-directory int-dir t))
    (dolist (elt tags-alist)
      (let* ((name (plist-get elt :name))
             (dir (proviso-substitute-env-vars (plist-get elt :dir)))
             (subdir (proviso-substitute-env-vars (plist-get elt :ctags-subdir)))
             (dir-abs (if subdir
                          (if (file-name-absolute-p subdir)
                              subdir
                            (if (and dir (file-name-absolute-p dir))
                                ;; we can assume root ends with '/',
                                ;; but not dir
                                (concat (file-name-as-directory dir) subdir)
                              (concat root (file-name-as-directory dir) subdir)))
                        (if (and dir (file-name-absolute-p dir))
                            dir
                          (concat root dir))))
             (subname (concat name "-tags"))
             (destfile (concat
                        (if int-dir
                            (file-remote-p int-dir 'localname)
                          tags-dir)
                        subname))
             (localfile (concat tags-dir subname)) ;only used for remote
             (arglist (plist-get elt :ctags-opts))
             (args (append
                    (proviso-gentags-command exe arglist)
                    (mapcar (lambda (dir)
                              (concat "--exclude=" dir))
                            (or (proviso-get proj :grep-exclude-dirs)
                                proviso-uninteresting-dirs))
                    (mapcar (lambda (file)
                              (concat "--exclude=" file))
                            (or (proviso-get proj :grep-exclude-files)
                                proviso-uninteresting-files))
                    (list "-f" destfile
                          (directory-file-name dir-abs))))
             (cmd (mapconcat 'identity args " ")))
        (push (append
               (list :cmd (list cmd)
                     :dir (file-name-as-directory
                           (if remote (concat remote dir-abs) dir-abs))
                     :do-gen do-gen
                     :copy-remote copy-remote
                     )
               (when copy-remote
                 (list
                  :remote-src (concat remote destfile)
                  :remote-dst localfile)))
              lst)))
    (dolist (elt tags-adds)
      (let* ((name (plist-get elt :name))
             (dir (proviso-substitute-env-vars
                   (plist-get elt :dir)))
             (file (proviso-substitute-env-vars
                    (plist-get elt :loc)))
             (dir-abs (when dir
                        (if (and dir (file-name-absolute-p dir))
                            dir
                          (concat root dir))))
             (file-abs (when file
                         (if (and file (file-name-absolute-p file))
                             file
                           (concat root file))))
             (subname (concat name "-tags"))
             (destfile (concat
                        (if int-dir
                            (file-remote-p int-dir 'localname)
                          tags-dir)
                        subname))
             (localfile (concat tags-dir subname)) ;only used for remote
             (cmd (concat exe " " (plist-get elt :cmd) " -f "
                          destfile " "
                          (if file-abs file-abs
                            (directory-file-name dir-abs)))))
        (push (append
               (list :cmd (list cmd)
                     :dir (if file-abs
                              (file-name-directory
                               (if remote (concat remote file-abs) file-abs))
                            (file-name-as-directory
                             (if remote (concat remote dir-abs) dir-abs)))
                     :do-gen do-gen
                     :copy-remote copy-remote
                     )
               (when copy-remote
                 (list
                  :remote-src (concat remote destfile)
                  :remote-dst localfile)))
              lst)))
    (proviso-gentags--prerun tags-dir)
    (proviso-gentags--run (proviso-get proj :project-name)
                          (nreverse lst) remote)
    (proviso-put proj :tags-lastgen (current-time))))

(defun proviso-gentags--prerun (dir)
  "Prepare DIR for storing TAGS files.
Move existing TAGS files to a backup location.
Existing files in the backup location are deleted."
  (let ((subdir (file-name-as-directory
                 (concat (file-name-as-directory dir) ".prior"))))
    (delete-directory subdir t)
    (make-directory subdir t)
    (dolist (file (directory-files dir t ".+-tags" t))
      (rename-file file subdir t))
    ))

(defun proviso-gentags--run (name lst &optional remote)
  "Run a series of tags invocations for project NAME according to LST.
LST is a list of plists, each of which contains necessary parameters.
REMOTE is non-nil if the project is on a remote host."
  (let ((buffer (get-buffer-create (format " *gentags-%s*" name)))
        (start-time (current-time)))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (setq-local window-point-insertion-type t)
      (setq-local proviso-gentags--waiting-jobs lst)
      (setq-local proviso-gentags--procs nil)
      (setq-local proviso-gentags--num-working-jobs 0)
      (setq-local proviso-gentags--max-jobs
                  (if remote proviso-gentags-max-jobs-remote
                    proviso-gentags-max-jobs-local))
      (let ((map (make-sparse-keymap)))
        (define-key map "q" #'quit-window)
        (set-keymap-parent map (current-local-map))
        (use-local-map map))
      (insert (format "TAGS generation started at %s\n\n"
                      (current-time-string start-time)))
      (setq-local proviso-gentags--start-time start-time))
    (proviso-gentags--spawn-jobs buffer)))

(defun proviso-gentags--spawn-jobs (buffer)
  "Spawn as many jobs as appropriate, with buffer BUFFER."
  (with-current-buffer buffer
    (let ((pnd (length proviso-gentags--waiting-jobs))
          (wrk proviso-gentags--num-working-jobs))
      (cond ((eq pnd 0)
             (if (eq wrk 0)
                 (proviso-gentags--done buffer)))
            ((>= wrk proviso-gentags--max-jobs)
             nil)
            (t (dotimes (i (- proviso-gentags--max-jobs wrk) t)
                 (unless (seq-empty-p proviso-gentags--waiting-jobs)
                   (cl-incf proviso-gentags--num-working-jobs)
                   (proviso-gentags--spawn
                    (pop proviso-gentags--waiting-jobs)
                    buffer))))))))

(defun proviso-gentags--spawn (plist buffer)
  "Execute a tags invocation according to PLIST.
BUFFER is an output buffer."
  (let* ((default-directory (plist-get plist :dir))
         (do-gen (plist-get plist :do-gen))
         (cmd
          (if do-gen
              (car (plist-get plist :cmd))
            ""))
         ;; /bin/sh -c "<script>" requires its argument (the script) be
         ;; quoted by strings; and `start-file-process' expects
         ;; a string of all arguments to be passed, starting with the executable.
         (process
          (if do-gen
              (start-file-process
               "generate TAGS"
               buffer
               "sh" "-c"
               cmd)
            (start-file-process
             "copy TAGS"
             buffer
             "sh" "-c"
             (format "echo Using existing file %s"
                     (plist-get plist :remote-src))))))
    (with-current-buffer buffer
      (insert (format "%s\n" cmd))
      (push (cons process
                  (list
                   (plist-get plist :copy-remote)
                   (plist-get plist :remote-src)
                   (plist-get plist :remote-dst)))
            proviso-gentags--procs)
      (set-process-sentinel process #'proviso-gentags--sentinel))))

(defun proviso-gentags--sentinel (proc change)
  "Process sentinel notifying that PROC underwent CHANGE."
  (when (string-match-p "\\(finished\\|exited\\)" change)
    (let ((buffer (process-buffer proc)))
      (with-current-buffer buffer
        (if-let ((entry (assoc proc proviso-gentags--procs)))
            (progn
              (setq proviso-gentags--procs (delete entry proviso-gentags--procs))
              (if-let ((copy (nth 1 entry))
                       (src (nth 2 entry))
                       (dst (nth 3 entry)))
                  (async-start
                   `(lambda ()
                      (setq inhibit-message t)
                      ,(async-inject-variables "load-path\\|xfer-.+-schemes")
                      (require 'xfer)
                      (xfer-transfer-file-silent ,src ,dst))
                   `(lambda (result)
                      (with-current-buffer ,buffer
                        (insert "  " (cdr result) "\n")
                        (cl-decf proviso-gentags--num-working-jobs))
                      (proviso-gentags--spawn-jobs ,buffer)))
                (cl-decf proviso-gentags--num-working-jobs)
                (proviso-gentags--spawn-jobs buffer))))))))

(defun proviso-gentags--done (buffer)
  "Called when tags invocation has completed for buffer BUFFER."
  (with-current-buffer buffer
    (let* ((now (current-time))
           (nowstr (current-time-string))
           (elapsed (float-time (time-subtract
                                 now proviso-gentags--start-time))))
      (insert (format "\nTAGS generation finished at %s (it took %s).\n\n"
                      nowstr
                      (format-seconds "%H, %M and %Z%S" elapsed))))))

(provide 'proviso-gentags)
;;; proviso-gentags.el ends here
