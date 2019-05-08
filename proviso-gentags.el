;;; proviso-gentags.el --- Generate TAGS files
;; Copyright (C) 2015-2019   (dan.harms)
;; Author:  <dan.harms@xrtrading.com>
;; Created: Wednesday, March 18, 2015
;; Version: 1.0
;; Modified Time-stamp: <2019-05-08 09:39:46 dan.harms>
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
         (concat "--c++-kinds=" proviso-gentags-ctags-cpp-kinds)
         "--file-scope=no"
         "--tag-relative=no")
   (if (listp rest) rest (list rest))))

(defvar proviso-gentags-max-jobs-local 10
  "Maximum allowed concurrently spawned processes for local projects.")
(defvar proviso-gentags-max-jobs-remote 2
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
If ARG is non-nil any remotely-generated files will be copied
locally."
  (interactive "P")
  (let ((proj (if (= (prefix-numeric-value current-prefix-arg) 16)
                  (proviso-choose-project) (proviso-current-project))))
    (unless (proviso-proj-p proj)
      (error "Could not generate tags: no active project"))
    (proviso-gentags--start-gen proj arg)))

(defun proviso-gentags--start-gen (proj &optional copy-remote)
  "Generate tags for project PROJ.
If COPY-REMOTE is non-nil, remote tags files will be copied to a
local destination automatically."
  (let* ((tags-alist (proviso-get proj :proj-alist))
         (remote (proviso-get proj :remote-prefix))
         (exe (if (and remote
                       (string-match-p "\\s-" remote))
                  (concat "\""
                          (proviso-gentags-exe remote)
                          "\"")
                (proviso-gentags-exe remote)))
         (root (proviso-get proj :root-dir))
         (dest-dir (proviso-get proj :tags-dir))
         (remote-tags-dir (or (proviso-get proj :tags-subdir) ".tags/"))
         (intermediate-dir (if (not remote) dest-dir
                             (file-name-as-directory
                              (if (file-name-absolute-p remote-tags-dir)
                                  remote-tags-dir
                                (concat root remote-tags-dir)))))
         lst)
    (when remote
      (make-directory (concat remote intermediate-dir) t))
    (make-directory dest-dir t)
    (dolist (elt tags-alist)
      (let* ((name (plist-get elt :name))
             (dir (plist-get elt :dir))
             (dir-abs (if (and dir (file-name-absolute-p dir)) dir
                        (concat root dir)))
             (subname (concat name "-tags"))
             (destfile (concat intermediate-dir subname))
             (remotefile (concat dest-dir subname)) ;only used for remote
             (arglist (plist-get elt :ctags-opts))
             (args (append
                    (proviso-gentags-command exe arglist)
                    (list "-f" destfile
                          (directory-file-name dir-abs))))
             (cmd (mapconcat 'identity args " ")))
        (push (append
               (list :cmd (list cmd)
                     :dir (if remote (concat remote dir-abs) dir-abs)
                     :copy-remote copy-remote
                     ) (when copy-remote
                     (list
                      :remote-src (concat remote destfile)
                      :remote-dst remotefile)))
              lst)))
    (proviso-gentags--run (proviso-get proj :project-name)
                          (nreverse lst) remote)
    (proviso-put proj :tags-lastgen (current-time))))

(defun proviso-gentags--run (name lst &optional remote)
  "Run a series of tags invocations for project NAME according to LST.
LST is a list of plists, each of which contains necessary parameters.
REMOTE is non-nil if the project is on a remote host."
  (let ((buffer (get-buffer-create (format " *gentags-%s*" name)))
        (start-time (current-time)))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (setq-local window-point-insertion-type t)
      (setq proviso-gentags--waiting-jobs lst)
      (setq proviso-gentags--procs nil)
      (setq proviso-gentags--num-working-jobs 0)
      (setq proviso-gentags--max-jobs
            (if remote proviso-gentags-max-jobs-remote
              proviso-gentags-max-jobs-local))
      (let ((map (make-sparse-keymap)))
        (define-key map "q" #'quit-window)
        (set-keymap-parent map (current-local-map))
        (use-local-map map))
      (insert (format "TAGS generation started at %s\n\n"
                      (current-time-string start-time)))
      (setq proviso-gentags--start-time start-time))
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
                   (incf proviso-gentags--num-working-jobs)
                   (proviso-gentags--spawn
                    (pop proviso-gentags--waiting-jobs)
                    buffer))))))))

(defun proviso-gentags--spawn (plist buffer)
  "Execute a tags invocation according to PLIST.
BUFFER is an output buffer."
  (let* ((default-directory (plist-get plist :dir))
         (cmd (car (plist-get plist :cmd)))
         ;; /bin/sh -c "<script>" requires its argument (the script) be
         ;; quoted by strings; and `start-file-process' expects
         ;; a string of all arguments to be passed, starting with the executable.
         (process (start-file-process
                   "generate TAGS"
                   buffer
                   "sh" "-c"
                   cmd)))
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
                        (decf proviso-gentags--num-working-jobs))
                      (proviso-gentags--spawn-jobs ,buffer)))
                (proviso-gentags--spawn-jobs buffer))))))))

(defun proviso-gentags--done (buffer)
  "Called when tags invocation has completed for buffer BUFFER."
  (with-current-buffer buffer
    (let* ((now (current-time))
           (nowstr (current-time-string))
           (elapsed (float-time (time-subtract
                                 now proviso-gentags--start-time))))
      (insert (format "\nTAGS generation finished at %s (it took %.3f seconds).\n\n"
                      nowstr elapsed)))))

(provide 'proviso-gentags)
;;; proviso-gentags.el ends here
