;; proviso-gentags.el --- Generate TAGS files
;; Copyright (C) 2015-2017   (dan.harms)
;; Author:  <dan.harms@xrtrading.com>
;; Created: Wednesday, March 18, 2015
;; Version: 1.0
;; Modified Time-stamp: <2017-04-24 17:49:48 dharms>
;; Modified by: Dan Harms
;; Keywords: proviso project etags ctags

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

;;

;;; Code:
(require 'proviso-core)
(require 'find-file)

;; customization variables
(defvar proviso-gentags-exe
  (cond ((executable-find "exctags") "exctags")
        ((executable-find "ctags") "ctags")
        (t "ctags"))
  "The ctags executable.")
(defvar proviso-gentags-ctags-cpp-kinds "+l" "Default ctags cpp-kinds options.")
(defvar proviso-gentags-ctags-cpp-options
  (list
   (concat "--c++-kinds=" proviso-gentags-ctags-cpp-kinds)
   "--file-scope=no"
   "--tag-relative=no")
  "Default ctags cpp options.")
(defvar proviso-gentags-copy-remote nil)

;; internal variables
(defvar proviso-gentags--iter nil "Current item being processed.")
(defvar proviso-gentags--total-num 0 "Total number of TAGS files to create.")
(defvar proviso-gentags--curr-num 0 "Current number of TAGS file being created.")
(defvar proviso-gentags--buffer nil "Buffer used by proviso-gentags.")
(defvar proviso-gentags--remote nil
  "Are tags being generating for a remote source repository?")
(defvar proviso-gentags--msg)
(defvar proviso-gentags--start-time 0
  "Time TAGS generation commenced.")
(defvar proviso-gentags--curr-proj nil "Current project.")
(defvar proviso-gentags--intermediate-dest-dir nil
  "An intermediate staging location for each TAGS file being generated.
This is useful for generating TAGS on a remote server.")
(defvar proviso-gentags--intermediate-dest-file nil
  "The intermediate staging file for each TAGS file that is generated.
This is useful for generating TAGS on a remote server.  On a local server,
this will be the same as the tags-dir.")
(defvar proviso-gentags--final-dest-dir nil
  "The final destination for the TAGS files being generated.")
(defvar proviso-gentags--final-dest-file nil
  "The final file being written for the current TAGS generation.")

(defun proviso-gentags-generate-tags (&optional arg)
  "Generate TAGS files according to the current profile.  If optional
ARG is supplied, also copy them to the local machine, if you are
running on a remote host."
  (interactive)
  (unless (proviso-get proviso-curr-proj :project-name)
    (error "Could not generate tags: no active project"))
  (setq proviso-gentags-copy-remote current-prefix-arg)
  (proviso-gentags--first-file))

(defun proviso-gentags--on-finish ()
  "Called when TAGS generation completes."
  (let ((elapsed (float-time
                  (time-subtract (current-time) proviso-gentags--start-time))))
    (with-current-buffer proviso-gentags--buffer
      (goto-char (point-max))
      (insert
       (format "\nTAGS generation finished at %s (it took %.3f seconds).\n\n\n"
               (current-time-string) elapsed)))))

(defun proviso-gentags--first-file ()
  "Start generating a series of TAGS files."
  (let ((tags-alist (proviso-get proviso-curr-proj :proj-alist))
        (remote-tags-dir (or
                          (proviso-get proviso-curr-proj :remote-tags-dir) ;todo get directory name
                          ".tags/")))
    (setq proviso-gentags--curr-proj (symbol-name proviso-curr-proj))
    (setq proviso-gentags--buffer (get-buffer-create " *proviso-gentags*"))
    (setq proviso-gentags--total-num (length tags-alist))
    (setq proviso-gentags--iter tags-alist)
    (setq proviso-gentags--remote (proviso-get proviso-curr-proj :remote-prefix))
    (setq proviso-gentags--final-dest-dir (proviso-get proviso-curr-proj :tags-dir))
    (if proviso-gentags--remote
        ;; we're generating TAGS on a remote host, so set up a
        ;; staging area for generation, before we copy them to the
        ;; local destination.
        (progn
          (setq proviso-gentags--intermediate-dest-dir
                (if (file-name-absolute-p remote-tags-dir)
                    remote-tags-dir
                  (concat (proviso-get proviso-curr-proj :root-dir)
                          remote-tags-dir)))
          (make-directory (concat proviso-gentags--remote
                                  proviso-gentags--intermediate-dest-dir) t))
      ;; else everything is local, so set up our variables in order to
      ;; generate output directly into the final destination.
      (setq proviso-gentags--intermediate-dest-dir
            proviso-gentags--final-dest-dir))
    (make-directory proviso-gentags--final-dest-dir t)
    (pop-to-buffer proviso-gentags--buffer)
    (with-current-buffer proviso-gentags--buffer
      (goto-char (point-max))
      (insert (format "TAGS generation started at %s\n\n"
                      (current-time-string)))))
  (setq proviso-gentags--start-time (current-time))
  (proviso-gentags--try-gen-next-file))

(defun proviso-gentags--try-gen-next-file ()
  "Generate a tags file."
  (proviso-set-current proviso-gentags--curr-proj)
  (if proviso-gentags--iter
      (proviso-gentags--gen-next-file)
    (proviso-gentags--on-finish)))

(defun proviso-gentags--gen-next-file ()
  "Generate TAGS for the current element."
  (let* ((src-name (nth 0 (car proviso-gentags--iter)))
         (src-dir (nth 1 (car proviso-gentags--iter)))
         (arg-list (cdr (cdr (car proviso-gentags--iter))))
         (default-directory
           (if (file-name-absolute-p src-dir)
               src-dir
             (concat
              (proviso-get proviso-curr-proj :root-dir)
              src-dir)))
         (sub-name (concat src-name "-tags"))
         process args)
    (setq proviso-gentags--intermediate-dest-file
          (concat proviso-gentags--intermediate-dest-dir sub-name))
    ;; this won't be used in the local scenario
    (setq proviso-gentags--final-dest-file
          (concat proviso-gentags--final-dest-dir sub-name))
    (setq proviso-gentags--msg (format "Generating tags for %s into %s..."
                                default-directory
                                proviso-gentags--intermediate-dest-file))
    (with-current-buffer proviso-gentags--buffer
      (goto-char (point-max))
      (insert proviso-gentags--msg))
    (setq args
          (append arg-list
                  ;; ctags on windows will barf if the source
                  ;; directory has a trailing slash
                  (list "-f" proviso-gentags--intermediate-dest-file
                        (directory-file-name default-directory))))
    ;; if remote, we need the remote prefix
    (when proviso-gentags--remote
      (setq default-directory
            (concat proviso-gentags--remote default-directory)))
    ;; /bin/sh -c "<script>" requires its argument (the script) be
    ;; quoted by strings; and `apply' expects a list as its last argument,
    ;; to be flattened out when the process is called.  Hence the
    ;; massaging of the input below to be a list containing a single item:
    ;; a string of all arguments to be passed, starting with the executable.
    (setq process (apply 'start-file-process
                         "generate TAGS"
                         proviso-gentags--buffer
                         "sh" "-c"
                         (list (mapconcat 'identity args " "))
                         ))
    (set-process-sentinel
     process
     (lambda (proc change)
       (when (string-match "\\(finished\\|exited\\)" change)
         (with-current-buffer proviso-gentags--buffer
           (goto-char (point-max))
           (insert "done.\n")
           (when (and proviso-gentags--remote proviso-gentags-copy-remote)
             (let ((start (current-time)))
               (copy-file
                (concat proviso-gentags--remote
                        proviso-gentags--intermediate-dest-file)
                proviso-gentags--final-dest-file t)
               (insert
                (format
                 " Copied to %s (remote transfer took %.3f sec.)\n"
                 proviso-gentags--final-dest-file
                 (float-time (time-subtract (current-time) start))))))
           (setq proviso-gentags--iter (cdr proviso-gentags--iter))
           (setq proviso-gentags--curr-num (1+ proviso-gentags--curr-num))
           (proviso-gentags--try-gen-next-file)))))
       ))

(provide 'proviso-gentags)

;; code ends here
