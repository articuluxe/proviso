;;; test_proviso-gentags.el --- test proviso gentags
;; Copyright (C) 2017-2021  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Monday, April 24, 2017
;; Version: 1.0
;; Modified Time-stamp: <2021-01-05 09:03:30 dharms>
;; Modified by: Dan Harms
;; Keywords: tools proviso project tags gentags
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
;; Proviso tests.
;;

;;; Code:
(load-file "test/proviso-test-common.el")
(require 'proviso)

(defvar tags-executable
  (cond ((executable-find "exctags")
         "exctags")
        (t "ctags"))
  "Tags executable on the host machine.")

(ert-deftest proviso-gentags-test-tags ()
  (proviso-test-reset-all)
  (let ((proviso-gentags-ctags-cpp-kinds "+l")
        file-contents arg-contents)
    (cl-letf (((symbol-function 'proviso--eval-file)
               (lambda (_)
                 (unless (string-empty-p (string-trim file-contents))
                   (car (read-from-string file-contents)))))
              ((symbol-function 'set-process-sentinel)
               (lambda (_ _2)
                 ))
              ((symbol-function 'start-file-process)
               (lambda (name buf prog &rest args)
                 (let ((str (mapconcat 'identity args " ")))
                   (should (string= (car arg-contents) str))
                   (pop arg-contents))))
              ((symbol-function 'proviso-gentags-exe)
               (lambda(_) tags-executable)))
      (should-error (proviso-gentags-generate-tags))
      ;; open file
      (setq file-contents (concat "(
:initfun (lambda (proj)
   (proviso-put proj :proj-alist
               '( (:name \"first\" :dir \"\")
                  (:name \"second\" :dir \"d/\")
                  (:name \"third\" :dir \"d2/\")
                  (:name \"fourth\" :dir \""
                                  (file-name-as-directory absolute-root-dir)
                                  "\")
                  )))
)"))
      (find-file (concat base-test-dir "a/b/c/d/dfile1"))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base-test-dir "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (eq proviso-local-proj proviso-curr-proj))
      (should (equal (proviso-get proviso-local-proj :tags-alist)
                     (list (list (concat "^\\(.*\\)" base-test-dir "a/b/c/" "\\(.*\\)$")
                                 (concat base-test-dir "a/b/c/.tags/first-tags")
                                 (concat base-test-dir "a/b/c/.tags/second-tags")
                                 (concat base-test-dir "a/b/c/.tags/third-tags")
                                 (concat base-test-dir "a/b/c/.tags/fourth-tags"))
                           (list (concat "^\\(.*\\)"
                                         (file-name-as-directory absolute-root-dir)
                                         "\\(.*\\)$")
                                 (concat base-test-dir "a/b/c/.tags/first-tags")
                                 (concat base-test-dir "a/b/c/.tags/second-tags")
                                 (concat base-test-dir "a/b/c/.tags/third-tags")
                                 (concat base-test-dir "a/b/c/.tags/fourth-tags"))
                           )))
      (setq arg-contents (list
                          (concat "-c " tags-executable " -Re --kinds-c++=+l --extras=-F --tag-relative=no -f "
                           base-test-dir "a/b/c/.tags/first-tags "
                           base-test-dir "a/b/c")
                          (concat "-c " tags-executable " -Re --kinds-c++=+l --extras=-F --tag-relative=no -f "
                           base-test-dir "a/b/c/.tags/second-tags "
                           base-test-dir "a/b/c/d")
                          (concat "-c " tags-executable " -Re --kinds-c++=+l --extras=-F --tag-relative=no -f "
                           base-test-dir "a/b/c/.tags/third-tags "
                           base-test-dir "a/b/c/d2")
                          (concat "-c " tags-executable " -Re --kinds-c++=+l --extras=-F --tag-relative=no -f "
                           base-test-dir "a/b/c/.tags/fourth-tags "
                           absolute-root-dir)
                          ))
      (proviso-gentags-generate-tags)

      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-deftest proviso-gentags-test-empty-proviso-file ()
  (proviso-test-reset-all)
  (let ((proviso-gentags-ctags-cpp-kinds "+l")
        file-contents arg-contents)
    (cl-letf (((symbol-function 'proviso--eval-file)
               (lambda (_)
                 (unless (string-empty-p (string-trim file-contents))
                   (car (read-from-string file-contents)))))
              ((symbol-function 'set-process-sentinel)
               (lambda (_ _2)
                 ))
              ((symbol-function 'start-file-process)
               (lambda (name buf prog &rest args)
                 (let ((str (mapconcat 'identity args " ")))
                   (should (string= (car arg-contents) str))
                   (pop arg-contents))))
              ((symbol-function 'proviso-gentags-exe)
               (lambda(_) tags-executable)))
      (should-error (proviso-gentags-generate-tags))
      ;; open file
      (setq file-contents "")
      (find-file (concat base-test-dir "a/b/c/d/dfile1"))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base-test-dir "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (eq proviso-local-proj proviso-curr-proj))
      (should (equal (proviso-get proviso-local-proj :tags-alist)
                     (list (list (concat "^\\(.*\\)" base-test-dir "a/b/c/" "\\(.*\\)$")
                                 (concat base-test-dir "a/b/c/.tags/c-tags")
                                 ))))
      (setq arg-contents (list
                          (concat "-c " tags-executable " -Re --kinds-c++=+l --extras=-F --tag-relative=no -f "
                           base-test-dir "a/b/c/.tags/c-tags "
                           base-test-dir "a/b/c")
                          ))
      (proviso-gentags-generate-tags)

      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-deftest proviso-gentags-test-tags-dirs-without-trailing-slashes ()
  (proviso-test-reset-all)
  (let ((proviso-gentags-ctags-cpp-kinds "+l")
        file-contents arg-contents)
    (cl-letf (((symbol-function 'proviso--eval-file)
               (lambda (_)
                 (unless (string-empty-p (string-trim file-contents))
                   (car (read-from-string file-contents)))))
              ((symbol-function 'set-process-sentinel)
               (lambda (_ _2)
                 ))
              ((symbol-function 'start-file-process)
               (lambda (name buf prog &rest args)
                 (let ((str (mapconcat 'identity args " ")))
                   (should (string= (car arg-contents) str))
                   (pop arg-contents))))
              ((symbol-function 'proviso-gentags-exe)
               (lambda(_) tags-executable)))
      (should-error (proviso-gentags-generate-tags))
      ;; open file
      (setq file-contents (concat "(
:initfun (lambda (proj)
   (proviso-put proj :proj-alist
               '( (:name \"first\" :dir \"\")
                  (:name \"second\" :dir \"d\")
                  (:name \"third\" :dir \"d2\")
                  (:name \"fourth\" :dir \""
                                  absolute-root-dir
                                  "\")
                  )))
)"))
      (find-file (concat base-test-dir "a/b/c/d/dfile1"))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base-test-dir "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (eq proviso-local-proj proviso-curr-proj))
      (should (equal (proviso-get proviso-local-proj :tags-alist)
                     (list (list (concat "^\\(.*\\)" base-test-dir "a/b/c/" "\\(.*\\)$")
                                 (concat base-test-dir "a/b/c/.tags/first-tags")
                                 (concat base-test-dir "a/b/c/.tags/second-tags")
                                 (concat base-test-dir "a/b/c/.tags/third-tags")
                                 (concat base-test-dir "a/b/c/.tags/fourth-tags"))
                           (list (concat "^\\(.*\\)"
                                         absolute-root-dir
                                         "\\(.*\\)$")
                                 (concat base-test-dir "a/b/c/.tags/first-tags")
                                 (concat base-test-dir "a/b/c/.tags/second-tags")
                                 (concat base-test-dir "a/b/c/.tags/third-tags")
                                 (concat base-test-dir "a/b/c/.tags/fourth-tags"))
                           )))
      (setq arg-contents (list
                          (concat "-c " tags-executable " -Re --kinds-c++=+l --extras=-F --tag-relative=no -f "
                           base-test-dir "a/b/c/.tags/first-tags "
                           base-test-dir "a/b/c")
                          (concat "-c " tags-executable " -Re --kinds-c++=+l --extras=-F --tag-relative=no -f "
                           base-test-dir "a/b/c/.tags/second-tags "
                           base-test-dir "a/b/c/d")
                          (concat "-c " tags-executable " -Re --kinds-c++=+l --extras=-F --tag-relative=no -f "
                           base-test-dir "a/b/c/.tags/third-tags "
                           base-test-dir "a/b/c/d2")
                          (concat "-c " tags-executable " -Re --kinds-c++=+l --extras=-F --tag-relative=no -f "
                           base-test-dir "a/b/c/.tags/fourth-tags "
                           absolute-root-dir)
                          ))
      (proviso-gentags-generate-tags)

      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-deftest proviso-gentags-test-tags-specify-tags-subdir ()
  (proviso-test-reset-all)
  (let (file-contents arg-contents)
    (cl-letf (((symbol-function 'proviso--eval-file)
               (lambda (_)
                 (unless (string-empty-p (string-trim file-contents))
                   (car (read-from-string file-contents)))))
              ((symbol-function 'set-process-sentinel)
               (lambda (_ _2)
                 ))
              ((symbol-function 'start-file-process)
               (lambda (name buf prog &rest args)
                 (let ((str (mapconcat 'identity args " ")))
                   (should (string= (car arg-contents) str))
                   (pop arg-contents))))
              ((symbol-function 'proviso-gentags-exe)
               (lambda(_) tags-executable)))
      (should-error (proviso-gentags-generate-tags))
      ;; open file
      (setq file-contents (concat "(
:initfun (lambda (proj)
   (proviso-put proj :proj-alist
               '( (:name \"first\" :dir \"\")
                  (:name \"second\" :dir \"d\")
                  (:name \"third\" :dir \"d2\")
                  (:name \"fourth\" :dir \""
                                  absolute-root-dir
                                  "\")
                  )))
)"))
      (find-file (concat base-test-dir "a/b/c/d/dfile1"))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base-test-dir "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (eq proviso-local-proj proviso-curr-proj))
      (should (equal (proviso-get proviso-local-proj :tags-alist)
                     (list (list (concat "^\\(.*\\)" base-test-dir "a/b/c/" "\\(.*\\)$")
                                 (concat base-test-dir "a/b/c/.tags/first-tags")
                                 (concat base-test-dir "a/b/c/.tags/second-tags")
                                 (concat base-test-dir "a/b/c/.tags/third-tags")
                                 (concat base-test-dir "a/b/c/.tags/fourth-tags"))
                           (list (concat "^\\(.*\\)"
                                         absolute-root-dir
                                         "\\(.*\\)$")
                                 (concat base-test-dir "a/b/c/.tags/first-tags")
                                 (concat base-test-dir "a/b/c/.tags/second-tags")
                                 (concat base-test-dir "a/b/c/.tags/third-tags")
                                 (concat base-test-dir "a/b/c/.tags/fourth-tags"))
                           )))
      (setq arg-contents (list
                          (concat "-c " tags-executable " -Re --kinds-c++=+l --extras=-F --tag-relative=no -f "
                           base-test-dir "a/b/c/.tags/first-tags "
                           base-test-dir "a/b/c")
                          (concat "-c " tags-executable " -Re --kinds-c++=+l --extras=-F --tag-relative=no -f "
                           base-test-dir "a/b/c/.tags/second-tags "
                           base-test-dir "a/b/c/d")
                          (concat "-c " tags-executable " -Re --kinds-c++=+l --extras=-F --tag-relative=no -f "
                           base-test-dir "a/b/c/.tags/third-tags "
                           base-test-dir "a/b/c/d2")
                          (concat "-c " tags-executable " -Re --kinds-c++=+l --extras=-F --tag-relative=no -f "
                           base-test-dir "a/b/c/.tags/fourth-tags "
                           absolute-root-dir)
                          ))
      (proviso-gentags-generate-tags)

      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-deftest proviso-gentags-test-tags-specify-tags-subdir-no-trailing-slash ()
  (proviso-test-reset-all)
  (let (file-contents arg-contents)
    (cl-letf (((symbol-function 'proviso--eval-file)
               (lambda (_)
                 (unless (string-empty-p (string-trim file-contents))
                   (car (read-from-string file-contents)))))
              ((symbol-function 'set-process-sentinel)
               (lambda (_ _2)
                 ))
              ((symbol-function 'start-file-process)
               (lambda (name buf prog &rest args)
                 (let ((str (mapconcat 'identity args " ")))
                   (should (string= (car arg-contents) str))
                   (pop arg-contents))))
              ((symbol-function 'proviso-gentags-exe)
               (lambda(_) tags-executable)))
      (should-error (proviso-gentags-generate-tags))
      ;; open file
      (setq file-contents (concat "(
:initfun (lambda (proj)
   (proviso-put proj :proj-alist
               '( (:name \"first\" :dir \"\")
                  (:name \"second\" :dir \"d\")
                  (:name \"third\" :dir \"d2\")
                  (:name \"fourth\" :dir \""
                                  absolute-root-dir
                                  "\")
                  )))
)"))
      (find-file (concat base-test-dir "a/b/c/d/dfile1"))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base-test-dir "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (eq proviso-local-proj proviso-curr-proj))
      (should (equal (proviso-get proviso-local-proj :tags-alist)
                     (list (list (concat "^\\(.*\\)" base-test-dir "a/b/c/" "\\(.*\\)$")
                                 (concat base-test-dir "a/b/c/.tags/first-tags")
                                 (concat base-test-dir "a/b/c/.tags/second-tags")
                                 (concat base-test-dir "a/b/c/.tags/third-tags")
                                 (concat base-test-dir "a/b/c/.tags/fourth-tags"))
                           (list (concat "^\\(.*\\)"
                                         absolute-root-dir
                                         "\\(.*\\)$")
                                 (concat base-test-dir "a/b/c/.tags/first-tags")
                                 (concat base-test-dir "a/b/c/.tags/second-tags")
                                 (concat base-test-dir "a/b/c/.tags/third-tags")
                                 (concat base-test-dir "a/b/c/.tags/fourth-tags"))
                           )))
      (setq arg-contents (list
                          (concat "-c " tags-executable " -Re --kinds-c++=+l --extras=-F --tag-relative=no -f "
                           base-test-dir "a/b/c/.tags/first-tags "
                           base-test-dir "a/b/c")
                          (concat "-c " tags-executable " -Re --kinds-c++=+l --extras=-F --tag-relative=no -f "
                           base-test-dir "a/b/c/.tags/second-tags "
                           base-test-dir "a/b/c/d")
                          (concat "-c " tags-executable " -Re --kinds-c++=+l --extras=-F --tag-relative=no -f "
                           base-test-dir "a/b/c/.tags/third-tags "
                           base-test-dir "a/b/c/d2")
                          (concat "-c " tags-executable " -Re --kinds-c++=+l --extras=-F --tag-relative=no -f "
                           base-test-dir "a/b/c/.tags/fourth-tags "
                           absolute-root-dir)
                          ))
      (proviso-gentags-generate-tags)

      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-deftest proviso-gentags-test-tags-additional-ctags-options ()
  (proviso-test-reset-all)
  (let ((proviso-gentags-ctags-cpp-kinds "+l")
        file-contents arg-contents)
    (cl-letf (((symbol-function 'proviso--eval-file)
               (lambda (_)
                 (unless (string-empty-p (string-trim file-contents))
                   (car (read-from-string file-contents)))))
              ((symbol-function 'set-process-sentinel)
               (lambda (_ _2)
                 ))
              ((symbol-function 'start-file-process)
               (lambda (name buf prog &rest args)
                 (let ((str (mapconcat 'identity args " ")))
                   (should (string= (car arg-contents) str))
                   (pop arg-contents))))
              ((symbol-function 'proviso-gentags-exe)
               (lambda(_) tags-executable)))
      (should-error (proviso-gentags-generate-tags))
      ;; open file
      (setq file-contents (concat "(
:initfun (lambda (proj)
   (proviso-put proj :proj-alist
               '( (:name \"first\" :dir \"\" :ctags-opts \"--exclude=boost\")
                  (:name \"second\" :dir \"d/\" :ctags-opts \"--exclude=asio --exclude=spirit\")
                  (:name \"third\" :dir \"d2/\")
                  (:name \"fourth\" :dir \""
                                  (file-name-as-directory absolute-root-dir)
                                  "\")
                  )))
)"))
      (find-file (concat base-test-dir "a/b/c/d/dfile1"))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base-test-dir "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (eq proviso-local-proj proviso-curr-proj))
      (should (equal (proviso-get proviso-local-proj :tags-alist)
                     (list (list (concat "^\\(.*\\)" base-test-dir "a/b/c/" "\\(.*\\)$")
                                 (concat base-test-dir "a/b/c/.tags/first-tags")
                                 (concat base-test-dir "a/b/c/.tags/second-tags")
                                 (concat base-test-dir "a/b/c/.tags/third-tags")
                                 (concat base-test-dir "a/b/c/.tags/fourth-tags"))
                           (list (concat "^\\(.*\\)"
                                         (file-name-as-directory absolute-root-dir)
                                         "\\(.*\\)$")
                                 (concat base-test-dir "a/b/c/.tags/first-tags")
                                 (concat base-test-dir "a/b/c/.tags/second-tags")
                                 (concat base-test-dir "a/b/c/.tags/third-tags")
                                 (concat base-test-dir "a/b/c/.tags/fourth-tags"))
                           )))
      (setq arg-contents (list
                          (concat "-c " tags-executable " -Re --kinds-c++=+l --extras=-F --tag-relative=no --exclude=boost -f "
                           base-test-dir "a/b/c/.tags/first-tags "
                           base-test-dir "a/b/c")
                          (concat "-c " tags-executable " -Re --kinds-c++=+l --extras=-F --tag-relative=no --exclude=asio --exclude=spirit -f "
                           base-test-dir "a/b/c/.tags/second-tags "
                           base-test-dir "a/b/c/d")
                          (concat "-c " tags-executable " -Re --kinds-c++=+l --extras=-F --tag-relative=no -f "
                           base-test-dir "a/b/c/.tags/third-tags "
                           base-test-dir "a/b/c/d2")
                          (concat "-c " tags-executable " -Re --kinds-c++=+l --extras=-F --tag-relative=no -f "
                           base-test-dir "a/b/c/.tags/fourth-tags "
                           absolute-root-dir)
                          ))
      (proviso-gentags-generate-tags)

      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-deftest proviso-gentags-test-tags-override-cpp-kinds ()
  (proviso-test-reset-all)
  (let ((proviso-gentags-ctags-cpp-kinds "+lw")
        file-contents arg-contents)
    (cl-letf (((symbol-function 'proviso--eval-file)
               (lambda (_)
                 (unless (string-empty-p (string-trim file-contents))
                   (car (read-from-string file-contents)))))
              ((symbol-function 'set-process-sentinel)
               (lambda (_ _2)
                 ))
              ((symbol-function 'start-file-process)
               (lambda (name buf prog &rest args)
                 (let ((str (mapconcat 'identity args " ")))
                   (should (string= (car arg-contents) str))
                   (pop arg-contents))))
              ((symbol-function 'proviso-gentags-exe)
               (lambda(_) tags-executable)))
      (should-error (proviso-gentags-generate-tags))
      ;; open file
      (setq file-contents (concat "(
:initfun (lambda (proj)
   (proviso-put proj :proj-alist
               '( (:name \"first\" :dir \"\")
                  (:name \"second\" :dir \"d\")
                  (:name \"third\" :dir \"d2\")
                  (:name \"fourth\" :dir \""
                                  absolute-root-dir
                                  "\")
                  )))
)"))
      (find-file (concat base-test-dir "a/b/c/d/dfile1"))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base-test-dir "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (eq proviso-local-proj proviso-curr-proj))
      (should (equal (proviso-get proviso-local-proj :tags-alist)
                     (list (list (concat "^\\(.*\\)" base-test-dir "a/b/c/" "\\(.*\\)$")
                                 (concat base-test-dir "a/b/c/.tags/first-tags")
                                 (concat base-test-dir "a/b/c/.tags/second-tags")
                                 (concat base-test-dir "a/b/c/.tags/third-tags")
                                 (concat base-test-dir "a/b/c/.tags/fourth-tags"))
                           (list (concat "^\\(.*\\)"
                                         absolute-root-dir
                                         "\\(.*\\)$")
                                 (concat base-test-dir "a/b/c/.tags/first-tags")
                                 (concat base-test-dir "a/b/c/.tags/second-tags")
                                 (concat base-test-dir "a/b/c/.tags/third-tags")
                                 (concat base-test-dir "a/b/c/.tags/fourth-tags"))
                           )))
      (setq arg-contents (list
                          (concat "-c " tags-executable " -Re --kinds-c++=+lw --extras=-F --tag-relative=no -f "
                           base-test-dir "a/b/c/.tags/first-tags "
                           base-test-dir "a/b/c")
                          (concat "-c " tags-executable " -Re --kinds-c++=+lw --extras=-F --tag-relative=no -f "
                           base-test-dir "a/b/c/.tags/second-tags "
                           base-test-dir "a/b/c/d")
                          (concat "-c " tags-executable " -Re --kinds-c++=+lw --extras=-F --tag-relative=no -f "
                           base-test-dir "a/b/c/.tags/third-tags "
                           base-test-dir "a/b/c/d2")
                          (concat "-c " tags-executable " -Re --kinds-c++=+lw --extras=-F --tag-relative=no -f "
                           base-test-dir "a/b/c/.tags/fourth-tags "
                           absolute-root-dir)
                          ))
      (proviso-gentags-generate-tags)

      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-deftest proviso-gentags-test-tags-override-gentags-exe ()
  (proviso-test-reset-all)
  (let (file-contents arg-contents)
    (cl-letf (((symbol-function 'proviso--eval-file)
               (lambda (_)
                 (unless (string-empty-p (string-trim file-contents))
                   (car (read-from-string file-contents)))))
              ((symbol-function 'set-process-sentinel)
               (lambda (_ _2)
                 ))
              ((symbol-function 'start-file-process)
               (lambda (name buf prog &rest args)
                 (let ((str (mapconcat 'identity args " ")))
                   (should (string= (car arg-contents) str))
                   (pop arg-contents))))
              ((symbol-function 'proviso-gentags-exe)
               (lambda(_) "myctags")))
      (should-error (proviso-gentags-generate-tags))
      ;; open file
      (setq file-contents (concat "(
:initfun (lambda (proj)
   (proviso-put proj :proj-alist
               '( (:name \"first\" :dir \"\")
                  (:name \"second\" :dir \"d\")
                  (:name \"third\" :dir \"d2\")
                  (:name \"fourth\" :dir \""
                                  absolute-root-dir
                                  "\")
                  )))
)"))
      (find-file (concat base-test-dir "a/b/c/d/dfile1"))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base-test-dir "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (eq proviso-local-proj proviso-curr-proj))
      (should (equal (proviso-get proviso-local-proj :tags-alist)
                     (list (list (concat "^\\(.*\\)" base-test-dir "a/b/c/" "\\(.*\\)$")
                                 (concat base-test-dir "a/b/c/.tags/first-tags")
                                 (concat base-test-dir "a/b/c/.tags/second-tags")
                                 (concat base-test-dir "a/b/c/.tags/third-tags")
                                 (concat base-test-dir "a/b/c/.tags/fourth-tags"))
                           (list (concat "^\\(.*\\)"
                                         absolute-root-dir
                                         "\\(.*\\)$")
                                 (concat base-test-dir "a/b/c/.tags/first-tags")
                                 (concat base-test-dir "a/b/c/.tags/second-tags")
                                 (concat base-test-dir "a/b/c/.tags/third-tags")
                                 (concat base-test-dir "a/b/c/.tags/fourth-tags"))
                           )))
      (setq arg-contents (list
                          (concat "-c myctags -Re --kinds-c++=+l --extras=-F --tag-relative=no -f "
                           base-test-dir "a/b/c/.tags/first-tags "
                           base-test-dir "a/b/c")
                          (concat "-c myctags -Re --kinds-c++=+l --extras=-F --tag-relative=no -f "
                           base-test-dir "a/b/c/.tags/second-tags "
                           base-test-dir "a/b/c/d")
                          (concat "-c myctags -Re --kinds-c++=+l --extras=-F --tag-relative=no -f "
                           base-test-dir "a/b/c/.tags/third-tags "
                           base-test-dir "a/b/c/d2")
                          (concat "-c myctags -Re --kinds-c++=+l --extras=-F --tag-relative=no -f "
                           base-test-dir "a/b/c/.tags/fourth-tags "
                           absolute-root-dir)
                          ))
      (proviso-gentags-generate-tags)

      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-deftest proviso-gentags-test-tags-override-settings ()
  (proviso-test-reset-all)
  (let (file-contents arg-contents)
    (cl-letf (((symbol-function 'proviso--eval-file)
               (lambda (_)
                 (unless (string-empty-p (string-trim file-contents))
                   (car (read-from-string file-contents)))))
              ((symbol-function 'set-process-sentinel)
               (lambda (_ _2)
                 ))
              ((symbol-function 'start-file-process)
               (lambda (name buf prog &rest args)
                 (let ((str (mapconcat 'identity args " ")))
                   (should (string= (car arg-contents) str))
                   (pop arg-contents))))
              ((symbol-function 'proviso-gentags-command)
               (lambda (_ args)
                 (list "totally new options"))))
      (should-error (proviso-gentags-generate-tags))
      ;; open file
      (setq file-contents (concat "(
:initfun (lambda (proj)
   (proviso-put proj :proj-alist
               '( (:name \"first\" :dir \"\")
                  (:name \"second\" :dir \"d\")
                  (:name \"third\" :dir \"d2\")
                  (:name \"fourth\" :dir \""
                                  absolute-root-dir
                                  "\")
                  )))
)"))
      (find-file (concat base-test-dir "a/b/c/d/dfile1"))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base-test-dir "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (eq proviso-local-proj proviso-curr-proj))
      (should (equal (proviso-get proviso-local-proj :tags-alist)
                     (list (list (concat "^\\(.*\\)" base-test-dir "a/b/c/" "\\(.*\\)$")
                                 (concat base-test-dir "a/b/c/.tags/first-tags")
                                 (concat base-test-dir "a/b/c/.tags/second-tags")
                                 (concat base-test-dir "a/b/c/.tags/third-tags")
                                 (concat base-test-dir "a/b/c/.tags/fourth-tags"))
                           (list (concat "^\\(.*\\)"
                                         absolute-root-dir
                                         "\\(.*\\)$")
                                 (concat base-test-dir "a/b/c/.tags/first-tags")
                                 (concat base-test-dir "a/b/c/.tags/second-tags")
                                 (concat base-test-dir "a/b/c/.tags/third-tags")
                                 (concat base-test-dir "a/b/c/.tags/fourth-tags"))
                           )))
      (setq arg-contents (list
                          (concat "-c totally new options -f "
                           base-test-dir "a/b/c/.tags/first-tags "
                           base-test-dir "a/b/c")
                          (concat "-c totally new options -f "
                           base-test-dir "a/b/c/.tags/second-tags "
                           base-test-dir "a/b/c/d")
                          (concat "-c totally new options -f "
                           base-test-dir "a/b/c/.tags/third-tags "
                           base-test-dir "a/b/c/d2")
                          (concat "-c totally new options -f "
                           base-test-dir "a/b/c/.tags/fourth-tags "
                           absolute-root-dir)
                          ))
      (proviso-gentags-generate-tags)

      ;; clean up buffers
      (kill-buffer "dfile1")
      )))


;;; test_proviso-gentags.el ends here
