#!/bin/sh
":"; exec "$EMACSX" --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; -*-
;;; test_proviso-gentags.el --- test proviso gentags
;; Copyright (C) 2017  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Monday, April 24, 2017
;; Version: 1.0
;; Modified Time-stamp: <2017-06-15 17:30:49 dharms>
;; Modified by: Dan Harms
;; Keywords: proviso project tags gentags

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
(load-file "test/proviso-test-common.el")
(require 'proviso)

(ert-deftest proviso-gentags-test-tags ()
  (proviso-test-reset-all)
  (let ((base (file-name-directory load-file-name))
        file-contents arg-contents)
    (cl-letf (((symbol-function 'proviso--load-file)
               (lambda (_)
                 (proviso-eval-string file-contents)))
              ((symbol-function 'set-process-sentinel)
               (lambda (_ _2)
                 (pop proviso-gentags--iter)
                 (proviso-gentags--try-gen-next-file)))
              ((symbol-function 'start-file-process)
               (lambda (name buf prog &rest args)
                 (let ((str (mapconcat 'identity args " ")))
                   (should (string= (car arg-contents) str))
                   (pop arg-contents)))))
      (should-error (proviso-gentags-generate-tags))
      ;; open file
      (setq file-contents "
 (defun do-init (proj)
   (proviso-put proj :proj-alist
               '( (:name \"first\" :dir \"\")
                  (:name \"second\" :dir \"d/\")
                  (:name \"third\" :dir \"d2/\")
                  (:name \"fourth\" :dir \"/home/\")
                  )))
 (proviso-define \"c\" :initfun 'do-init)
")
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (proviso-name-p (proviso-get proviso-local-proj :project-name)))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (eq proviso-local-proj proviso-curr-proj))
      (should (equal (proviso-get proviso-local-proj :tags-alist)
                     (list (concat base "a/b/c/" "\\(.*\\)$")
                           (concat base "a/b/c/.tags/first-tags")
                           (concat base "a/b/c/.tags/second-tags")
                           (concat base "a/b/c/.tags/third-tags")
                           (concat base "a/b/c/.tags/fourth-tags")
                           )))
      (setq arg-contents (list
                          (concat "-c exctags -Re --c++-kinds=+l --file-scope=no --tag-relative=no -f "
                           base "a/b/c/.tags/first-tags "
                           base "a/b/c")
                          (concat "-c exctags -Re --c++-kinds=+l --file-scope=no --tag-relative=no -f "
                           base "a/b/c/.tags/second-tags "
                           base "a/b/c/d")
                          (concat "-c exctags -Re --c++-kinds=+l --file-scope=no --tag-relative=no -f "
                           base "a/b/c/.tags/third-tags "
                           base "a/b/c/d2")
                          (concat "-c exctags -Re --c++-kinds=+l --file-scope=no --tag-relative=no -f "
                           base "a/b/c/.tags/fourth-tags "
                           "/home")
                          ))
      (proviso-gentags-generate-tags)

      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-deftest proviso-gentags-test-empty-proviso-file ()
  (proviso-test-reset-all)
  (let ((base (file-name-directory load-file-name))
        file-contents arg-contents)
    (cl-letf (((symbol-function 'proviso--load-file)
               (lambda (_)
                 (proviso-eval-string file-contents)))
              ((symbol-function 'set-process-sentinel)
               (lambda (_ _2)
                 (pop proviso-gentags--iter)
                 (proviso-gentags--try-gen-next-file)))
              ((symbol-function 'start-file-process)
               (lambda (name buf prog &rest args)
                 (let ((str (mapconcat 'identity args " ")))
                   (should (string= (car arg-contents) str))
                   (pop arg-contents)))))
      (should-error (proviso-gentags-generate-tags))
      ;; open file
      (setq file-contents "")
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (proviso-name-p (proviso-get proviso-local-proj :project-name)))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (eq proviso-local-proj proviso-curr-proj))
      (should (equal (proviso-get proviso-local-proj :tags-alist)
                     (list (concat base "a/b/c/" "\\(.*\\)$")
                           (concat base "a/b/c/.tags/c-tags")
                           )))
      (setq arg-contents (list
                          (concat "-c exctags -Re --c++-kinds=+l --file-scope=no --tag-relative=no -f "
                           base "a/b/c/.tags/c-tags "
                           base "a/b/c")
                          ))
      (proviso-gentags-generate-tags)

      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-deftest proviso-gentags-test-tags-dirs-without-trailing-slashes ()
  (proviso-test-reset-all)
  (let ((base (file-name-directory load-file-name))
        file-contents arg-contents)
    (cl-letf (((symbol-function 'proviso--load-file)
               (lambda (_)
                 (proviso-eval-string file-contents)))
              ((symbol-function 'set-process-sentinel)
               (lambda (_ _2)
                 (pop proviso-gentags--iter)
                 (proviso-gentags--try-gen-next-file)))
              ((symbol-function 'start-file-process)
               (lambda (name buf prog &rest args)
                 (let ((str (mapconcat 'identity args " ")))
                   (should (string= (car arg-contents) str))
                   (pop arg-contents)))))
      (should-error (proviso-gentags-generate-tags))
      ;; open file
      (setq file-contents "
 (defun do-init (proj)
   (proviso-put proj :proj-alist
               '( (:name \"first\" :dir \"\")
                  (:name \"second\" :dir \"d\")
                  (:name \"third\" :dir \"d2\")
                  (:name \"fourth\" :dir \"/home\")
                  )))
 (proviso-define \"c\" :initfun 'do-init)
")
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (proviso-name-p (proviso-get proviso-local-proj :project-name)))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (eq proviso-local-proj proviso-curr-proj))
      (should (equal (proviso-get proviso-local-proj :tags-alist)
                     (list (concat base "a/b/c/" "\\(.*\\)$")
                           (concat base "a/b/c/.tags/first-tags")
                           (concat base "a/b/c/.tags/second-tags")
                           (concat base "a/b/c/.tags/third-tags")
                           (concat base "a/b/c/.tags/fourth-tags")
                           )))
      (setq arg-contents (list
                          (concat "-c exctags -Re --c++-kinds=+l --file-scope=no --tag-relative=no -f "
                           base "a/b/c/.tags/first-tags "
                           base "a/b/c")
                          (concat "-c exctags -Re --c++-kinds=+l --file-scope=no --tag-relative=no -f "
                           base "a/b/c/.tags/second-tags "
                           base "a/b/c/d")
                          (concat "-c exctags -Re --c++-kinds=+l --file-scope=no --tag-relative=no -f "
                           base "a/b/c/.tags/third-tags "
                           base "a/b/c/d2")
                          (concat "-c exctags -Re --c++-kinds=+l --file-scope=no --tag-relative=no -f "
                           base "a/b/c/.tags/fourth-tags "
                           "/home")
                          ))
      (proviso-gentags-generate-tags)

      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-deftest proviso-gentags-test-tags-specify-tags-subdir ()
  (proviso-test-reset-all)
  (let ((base (file-name-directory load-file-name))
        file-contents arg-contents)
    (cl-letf (((symbol-function 'proviso--load-file)
               (lambda (_)
                 (proviso-eval-string file-contents)))
              ((symbol-function 'set-process-sentinel)
               (lambda (_ _2)
                 (pop proviso-gentags--iter)
                 (proviso-gentags--try-gen-next-file)))
              ((symbol-function 'start-file-process)
               (lambda (name buf prog &rest args)
                 (let ((str (mapconcat 'identity args " ")))
                   (should (string= (car arg-contents) str))
                   (pop arg-contents)))))
      (should-error (proviso-gentags-generate-tags))
      ;; open file
      (setq file-contents "
 (defun do-init (proj)
   (proviso-put proj :proj-alist
               '( (:name \"first\" :dir \"\")
                  (:name \"second\" :dir \"d\")
                  (:name \"third\" :dir \"d2\")
                  (:name \"fourth\" :dir \"/home\")
                  )))
 (proviso-define \"c\" :initfun 'do-init :tags-subdir \".mytags/\")
")
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (proviso-name-p (proviso-get proviso-local-proj :project-name)))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (eq proviso-local-proj proviso-curr-proj))
      (should (equal (proviso-get proviso-local-proj :tags-alist)
                     (list (concat base "a/b/c/" "\\(.*\\)$")
                           (concat base "a/b/c/.mytags/first-tags")
                           (concat base "a/b/c/.mytags/second-tags")
                           (concat base "a/b/c/.mytags/third-tags")
                           (concat base "a/b/c/.mytags/fourth-tags")
                           )))
      (setq arg-contents (list
                          (concat "-c exctags -Re --c++-kinds=+l --file-scope=no --tag-relative=no -f "
                           base "a/b/c/.mytags/first-tags "
                           base "a/b/c")
                          (concat "-c exctags -Re --c++-kinds=+l --file-scope=no --tag-relative=no -f "
                           base "a/b/c/.mytags/second-tags "
                           base "a/b/c/d")
                          (concat "-c exctags -Re --c++-kinds=+l --file-scope=no --tag-relative=no -f "
                           base "a/b/c/.mytags/third-tags "
                           base "a/b/c/d2")
                          (concat "-c exctags -Re --c++-kinds=+l --file-scope=no --tag-relative=no -f "
                           base "a/b/c/.mytags/fourth-tags "
                           "/home")
                          ))
      (proviso-gentags-generate-tags)

      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-deftest proviso-gentags-test-tags-specify-tags-subdir-no-trailing-slash ()
  (proviso-test-reset-all)
  (let ((base (file-name-directory load-file-name))
        file-contents arg-contents)
    (cl-letf (((symbol-function 'proviso--load-file)
               (lambda (_)
                 (proviso-eval-string file-contents)))
              ((symbol-function 'set-process-sentinel)
               (lambda (_ _2)
                 (pop proviso-gentags--iter)
                 (proviso-gentags--try-gen-next-file)))
              ((symbol-function 'start-file-process)
               (lambda (name buf prog &rest args)
                 (let ((str (mapconcat 'identity args " ")))
                   (should (string= (car arg-contents) str))
                   (pop arg-contents)))))
      (should-error (proviso-gentags-generate-tags))
      ;; open file
      (setq file-contents "
 (defun do-init (proj)
   (proviso-put proj :proj-alist
               '( (:name \"first\" :dir \"\")
                  (:name \"second\" :dir \"d\")
                  (:name \"third\" :dir \"d2\")
                  (:name \"fourth\" :dir \"/home\")
                  )))
 (proviso-define \"c\" :initfun 'do-init :tags-subdir \".mytags\")
")
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (proviso-name-p (proviso-get proviso-local-proj :project-name)))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (eq proviso-local-proj proviso-curr-proj))
      (should (equal (proviso-get proviso-local-proj :tags-alist)
                     (list (concat base "a/b/c/" "\\(.*\\)$")
                           (concat base "a/b/c/.mytags/first-tags")
                           (concat base "a/b/c/.mytags/second-tags")
                           (concat base "a/b/c/.mytags/third-tags")
                           (concat base "a/b/c/.mytags/fourth-tags")
                           )))
      (setq arg-contents (list
                          (concat "-c exctags -Re --c++-kinds=+l --file-scope=no --tag-relative=no -f "
                           base "a/b/c/.mytags/first-tags "
                           base "a/b/c")
                          (concat "-c exctags -Re --c++-kinds=+l --file-scope=no --tag-relative=no -f "
                           base "a/b/c/.mytags/second-tags "
                           base "a/b/c/d")
                          (concat "-c exctags -Re --c++-kinds=+l --file-scope=no --tag-relative=no -f "
                           base "a/b/c/.mytags/third-tags "
                           base "a/b/c/d2")
                          (concat "-c exctags -Re --c++-kinds=+l --file-scope=no --tag-relative=no -f "
                           base "a/b/c/.mytags/fourth-tags "
                           "/home")
                          ))
      (proviso-gentags-generate-tags)

      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-deftest proviso-gentags-test-tags-additional-ctags-options ()
  (proviso-test-reset-all)
  (let ((base (file-name-directory load-file-name))
        file-contents arg-contents)
    (cl-letf (((symbol-function 'proviso--load-file)
               (lambda (_)
                 (proviso-eval-string file-contents)))
              ((symbol-function 'set-process-sentinel)
               (lambda (_ _2)
                 (pop proviso-gentags--iter)
                 (proviso-gentags--try-gen-next-file)))
              ((symbol-function 'start-file-process)
               (lambda (name buf prog &rest args)
                 (let ((str (mapconcat 'identity args " ")))
                   (should (string= (car arg-contents) str))
                   (pop arg-contents)))))
      (should-error (proviso-gentags-generate-tags))
      ;; open file
      (setq file-contents "
 (defun do-init (proj)
   (proviso-put proj :proj-alist
               '( (:name \"first\" :dir \"\" :ctags-opts \"--exclude=boost\")
                  (:name \"second\" :dir \"d/\" :ctags-opts \"--exclude=asio --exclude=spirit\")
                  (:name \"third\" :dir \"d2/\")
                  (:name \"fourth\" :dir \"/home/\")
                  )))
 (proviso-define \"c\" :initfun 'do-init)
")
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (proviso-name-p (proviso-get proviso-local-proj :project-name)))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (eq proviso-local-proj proviso-curr-proj))
      (should (equal (proviso-get proviso-local-proj :tags-alist)
                     (list (concat base "a/b/c/" "\\(.*\\)$")
                           (concat base "a/b/c/.tags/first-tags")
                           (concat base "a/b/c/.tags/second-tags")
                           (concat base "a/b/c/.tags/third-tags")
                           (concat base "a/b/c/.tags/fourth-tags")
                           )))
      (setq arg-contents (list
                          (concat "-c exctags -Re --c++-kinds=+l --file-scope=no --tag-relative=no --exclude=boost -f "
                           base "a/b/c/.tags/first-tags "
                           base "a/b/c")
                          (concat "-c exctags -Re --c++-kinds=+l --file-scope=no --tag-relative=no --exclude=asio --exclude=spirit -f "
                           base "a/b/c/.tags/second-tags "
                           base "a/b/c/d")
                          (concat "-c exctags -Re --c++-kinds=+l --file-scope=no --tag-relative=no -f "
                           base "a/b/c/.tags/third-tags "
                           base "a/b/c/d2")
                          (concat "-c exctags -Re --c++-kinds=+l --file-scope=no --tag-relative=no -f "
                           base "a/b/c/.tags/fourth-tags "
                           "/home")
                          ))
      (proviso-gentags-generate-tags)

      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-deftest proviso-gentags-test-tags-override-cpp-kinds ()
  (proviso-test-reset-all)
  (let ((base (file-name-directory load-file-name))
        (proviso-gentags-ctags-cpp-kinds "+lw")
        file-contents arg-contents)
    (cl-letf (((symbol-function 'proviso--load-file)
               (lambda (_)
                 (proviso-eval-string file-contents)))
              ((symbol-function 'set-process-sentinel)
               (lambda (_ _2)
                 (pop proviso-gentags--iter)
                 (proviso-gentags--try-gen-next-file)))
              ((symbol-function 'start-file-process)
               (lambda (name buf prog &rest args)
                 (let ((str (mapconcat 'identity args " ")))
                   (should (string= (car arg-contents) str))
                   (pop arg-contents)))))
      (should-error (proviso-gentags-generate-tags))
      ;; open file
      (setq file-contents "
 (defun do-init (proj)
   (proviso-put proj :proj-alist
               '( (:name \"first\" :dir \"\")
                  (:name \"second\" :dir \"d\")
                  (:name \"third\" :dir \"d2\")
                  (:name \"fourth\" :dir \"/home\")
                  )))
 (proviso-define \"c\" :initfun 'do-init :tags-subdir \".mytags\")
")
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (proviso-name-p (proviso-get proviso-local-proj :project-name)))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (eq proviso-local-proj proviso-curr-proj))
      (should (equal (proviso-get proviso-local-proj :tags-alist)
                     (list (concat base "a/b/c/" "\\(.*\\)$")
                           (concat base "a/b/c/.mytags/first-tags")
                           (concat base "a/b/c/.mytags/second-tags")
                           (concat base "a/b/c/.mytags/third-tags")
                           (concat base "a/b/c/.mytags/fourth-tags")
                           )))
      (setq arg-contents (list
                          (concat "-c exctags -Re --c++-kinds=+lw --file-scope=no --tag-relative=no -f "
                           base "a/b/c/.mytags/first-tags "
                           base "a/b/c")
                          (concat "-c exctags -Re --c++-kinds=+lw --file-scope=no --tag-relative=no -f "
                           base "a/b/c/.mytags/second-tags "
                           base "a/b/c/d")
                          (concat "-c exctags -Re --c++-kinds=+lw --file-scope=no --tag-relative=no -f "
                           base "a/b/c/.mytags/third-tags "
                           base "a/b/c/d2")
                          (concat "-c exctags -Re --c++-kinds=+lw --file-scope=no --tag-relative=no -f "
                           base "a/b/c/.mytags/fourth-tags "
                           "/home")
                          ))
      (proviso-gentags-generate-tags)

      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-deftest proviso-gentags-test-tags-override-gentags-exe ()
  (proviso-test-reset-all)
  (let ((base (file-name-directory load-file-name))
        (proviso-gentags-exe "myctags")
        file-contents arg-contents)
    (cl-letf (((symbol-function 'proviso--load-file)
               (lambda (_)
                 (proviso-eval-string file-contents)))
              ((symbol-function 'set-process-sentinel)
               (lambda (_ _2)
                 (pop proviso-gentags--iter)
                 (proviso-gentags--try-gen-next-file)))
              ((symbol-function 'start-file-process)
               (lambda (name buf prog &rest args)
                 (let ((str (mapconcat 'identity args " ")))
                   (should (string= (car arg-contents) str))
                   (pop arg-contents)))))
      (should-error (proviso-gentags-generate-tags))
      ;; open file
      (setq file-contents "
 (defun do-init (proj)
   (proviso-put proj :proj-alist
               '( (:name \"first\" :dir \"\")
                  (:name \"second\" :dir \"d\")
                  (:name \"third\" :dir \"d2\")
                  (:name \"fourth\" :dir \"/home\")
                  )))
 (proviso-define \"c\" :initfun 'do-init)
")
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (proviso-name-p (proviso-get proviso-local-proj :project-name)))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (eq proviso-local-proj proviso-curr-proj))
      (should (equal (proviso-get proviso-local-proj :tags-alist)
                     (list (concat base "a/b/c/" "\\(.*\\)$")
                           (concat base "a/b/c/.tags/first-tags")
                           (concat base "a/b/c/.tags/second-tags")
                           (concat base "a/b/c/.tags/third-tags")
                           (concat base "a/b/c/.tags/fourth-tags")
                           )))
      (setq arg-contents (list
                          (concat "-c myctags -Re --c++-kinds=+l --file-scope=no --tag-relative=no -f "
                           base "a/b/c/.tags/first-tags "
                           base "a/b/c")
                          (concat "-c myctags -Re --c++-kinds=+l --file-scope=no --tag-relative=no -f "
                           base "a/b/c/.tags/second-tags "
                           base "a/b/c/d")
                          (concat "-c myctags -Re --c++-kinds=+l --file-scope=no --tag-relative=no -f "
                           base "a/b/c/.tags/third-tags "
                           base "a/b/c/d2")
                          (concat "-c myctags -Re --c++-kinds=+l --file-scope=no --tag-relative=no -f "
                           base "a/b/c/.tags/fourth-tags "
                           "/home")
                          ))
      (proviso-gentags-generate-tags)

      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-deftest proviso-gentags-test-tags-override-settings ()
  (proviso-test-reset-all)
  (let ((base (file-name-directory load-file-name))
        file-contents arg-contents)
    (cl-letf (((symbol-function 'proviso--load-file)
               (lambda (_)
                 (proviso-eval-string file-contents)))
              ((symbol-function 'set-process-sentinel)
               (lambda (_ _2)
                 (pop proviso-gentags--iter)
                 (proviso-gentags--try-gen-next-file)))
              ((symbol-function 'start-file-process)
               (lambda (name buf prog &rest args)
                 (let ((str (mapconcat 'identity args " ")))
                   (should (string= (car arg-contents) str))
                   (pop arg-contents))))
              ((symbol-function 'proviso-gentags-command)
               (lambda (args)
                 (list "totally new options"))))
      (should-error (proviso-gentags-generate-tags))
      ;; open file
      (setq file-contents "
 (defun do-init (proj)
   (proviso-put proj :proj-alist
               '( (:name \"first\" :dir \"\")
                  (:name \"second\" :dir \"d\")
                  (:name \"third\" :dir \"d2\")
                  (:name \"fourth\" :dir \"/home\")
                  )))
 (proviso-define \"c\" :initfun 'do-init)
")
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (proviso-name-p (proviso-get proviso-local-proj :project-name)))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (eq proviso-local-proj proviso-curr-proj))
      (should (equal (proviso-get proviso-local-proj :tags-alist)
                     (list (concat base "a/b/c/" "\\(.*\\)$")
                           (concat base "a/b/c/.tags/first-tags")
                           (concat base "a/b/c/.tags/second-tags")
                           (concat base "a/b/c/.tags/third-tags")
                           (concat base "a/b/c/.tags/fourth-tags")
                           )))
      (setq arg-contents (list
                          (concat "-c totally new options -f "
                           base "a/b/c/.tags/first-tags "
                           base "a/b/c")
                          (concat "-c totally new options -f "
                           base "a/b/c/.tags/second-tags "
                           base "a/b/c/d")
                          (concat "-c totally new options -f "
                           base "a/b/c/.tags/third-tags "
                           base "a/b/c/d2")
                          (concat "-c totally new options -f "
                           base "a/b/c/.tags/fourth-tags "
                           "/home")
                          ))
      (proviso-gentags-generate-tags)

      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-run-tests-batch-and-exit (car argv))

;;; test_proviso-gentags.el ends here
