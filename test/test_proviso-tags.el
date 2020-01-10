;;; test_proviso-tags.el --- test proviso tags functionality
;; Copyright (C) 2017-2020  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Thursday, April 13, 2017
;; Version: 1.0
;; Modified Time-stamp: <2020-01-09 08:23:05 Dan.Harms>
;; Modified by: Dan.Harms
;; Keywords: tools proviso project tags etags
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

(ert-deftest proviso-tags-test-tags-one ()
  (proviso-test-reset-all)
  (let ((base (file-name-directory load-name))
        file-contents)
    (cl-letf (((symbol-function 'proviso--eval-file)
               (lambda (_)
                 (unless (string-empty-p (string-trim file-contents))
                   (car (read-from-string file-contents))))))
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
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (equal (proviso-get proviso-local-proj :tags-alist)
                     (list (list (concat "^\\(.*\\)" base "a/b/c/" "\\(.*\\)$")
                                 (concat base "a/b/c/.tags/first-tags")
                                 (concat base "a/b/c/.tags/second-tags")
                                 (concat base "a/b/c/.tags/third-tags")
                                 (concat base "a/b/c/.tags/fourth-tags"))
                           (list (concat "^\\(.*\\)"
                                         (file-name-as-directory absolute-root-dir)
                                         "\\(.*\\)$")
                                 (concat base "a/b/c/.tags/first-tags")
                                 (concat base "a/b/c/.tags/second-tags")
                                 (concat base "a/b/c/.tags/third-tags")
                                 (concat base "a/b/c/.tags/fourth-tags"))
                                   )))

      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-deftest proviso-tags-test-empty-proviso ()
  (proviso-test-reset-all)
  (let ((base (file-name-directory load-name))
        file-contents)
    (cl-letf (((symbol-function 'proviso--eval-file)
               (lambda (_)
                 (unless (string-empty-p (string-trim file-contents))
                   (car (read-from-string file-contents))))))
      ;; open file
      (setq file-contents "")
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (equal (proviso-get proviso-local-proj :tags-alist)
                     (list (list (concat "^\\(.*\\)" base "a/b/c/" "\\(.*\\)$")
                                 (concat base "a/b/c/.tags/c-tags"))
                                   )))

      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

;; (ert-deftest proviso-tags-test-tags-dir-from-env-var ()
;;   (proviso-test-reset-all)
;;   (let ((base (file-name-directory load-name))
;;         file-contents)
;;     (cl-letf (((symbol-function 'proviso--eval-file)
;;                (lambda (_)
;;                  (proviso-eval-string file-contents))))
;;       ;; open file
;;       (setq file-contents "
;;  (defun do-init (proj)
;;    (proviso-put proj :proj-alist
;;                '( (:name \"first\" :dir \"\")
;;                   (:name \"second\" :dir \"d/\")
;;                   (:name \"third\" :dir \"d2/\")
;;                   (:name \"fourth\" :dir \"/home/\")
;;                   )))
;;  (proviso-define-project \"c\" :initfun 'do-init)
;; ")
;;       (setenv "EMACS_TAGS_DIR" "~/.mytags_dir/")
;;       (find-file (concat base "a/b/c/d/dfile1"))
;;       (should (proviso-name-p (proviso-get proviso-local-proj :project-name)))
;;       (should (string= (proviso-get proviso-local-proj :root-dir)
;;                        (concat base "a/b/c/")))
;;       (should (string= (proviso-get proviso-local-proj :project-name)
;;                        "c"))
;;       ;; todo: this case only works remotely
;;       (should (equal (proviso-get proviso-local-proj :tags-alist)
;;                      (list (concat base "a/b/c/" "\\(.*\\)$")
;;                            "~/.mytags_dir/fourth-tags"
;;                            "~/.mytags_dir/third-tags"
;;                            "~/.mytags_dir/second-tags"
;;                            "~/.mytags_dir/first-tags"
;;                                    )))
;;       (setenv "EMACS_TAGS_DIR")
;;       ;; clean up buffers
;;       (kill-buffer "dfile1")
;;       )))

(ert-deftest proviso-tags-test-tags-dir-explicit-with-trailing-slash ()
  (proviso-test-reset-all)
  (let ((base (file-name-directory load-name))
        file-contents)
    (cl-letf (((symbol-function 'proviso--eval-file)
               (lambda (_)
                 (unless (string-empty-p (string-trim file-contents))
                   (car (read-from-string file-contents))))))
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
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (equal (proviso-get proviso-local-proj :tags-alist)
                     (list (list (concat "^\\(.*\\)" base "a/b/c/" "\\(.*\\)$")
                                 (concat base "a/b/c/.tags/first-tags")
                                 (concat base "a/b/c/.tags/second-tags")
                                 (concat base "a/b/c/.tags/third-tags")
                                 (concat base "a/b/c/.tags/fourth-tags"))
                           (list (concat "^\\(.*\\)"
                                         (file-name-as-directory absolute-root-dir)
                                         "\\(.*\\)$")
                                 (concat base "a/b/c/.tags/first-tags")
                                 (concat base "a/b/c/.tags/second-tags")
                                 (concat base "a/b/c/.tags/third-tags")
                                 (concat base "a/b/c/.tags/fourth-tags"))
                                   )))
      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-deftest proviso-tags-test-tags-dir-explicit-without-trailing-slash ()
  (proviso-test-reset-all)
  (let ((base (file-name-directory load-name))
        file-contents)
    (cl-letf (((symbol-function 'proviso--eval-file)
               (lambda (_)
                 (unless (string-empty-p (string-trim file-contents))
                   (car (read-from-string file-contents))))))
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
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (equal (proviso-get proviso-local-proj :tags-alist)
                     (list (list (concat "^\\(.*\\)" base "a/b/c/" "\\(.*\\)$")
                                 (concat base "a/b/c/.tags/first-tags")
                                 (concat base "a/b/c/.tags/second-tags")
                                 (concat base "a/b/c/.tags/third-tags")
                                 (concat base "a/b/c/.tags/fourth-tags"))
                           (list (concat "^\\(.*\\)"
                                         (file-name-as-directory absolute-root-dir)
                                         "\\(.*\\)$")
                                 (concat base "a/b/c/.tags/first-tags")
                                 (concat base "a/b/c/.tags/second-tags")
                                 (concat base "a/b/c/.tags/third-tags")
                                 (concat base "a/b/c/.tags/fourth-tags"))
                                   )))
      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-deftest proviso-tags-test-tags ()
  (proviso-test-reset-all)
  (let ((base (file-name-directory load-name))
        file-contents)
    (cl-letf (((symbol-function 'proviso--eval-file)
               (lambda (_)
                 (unless (string-empty-p (string-trim file-contents))
                   (car (read-from-string file-contents))))))
      ;; open file
      (setq file-contents (concat "(
:initfun (lambda (proj)
   (proviso-put proj :proj-alist
               '( (:name \"first\" :dir \"\")
                  (:name \"second\" :dir \"d/\")
                  (:name \"third\" :dir \"d2/\")
                  (:name \"fourth\" :dir \""
                                  absolute-root-dir
                                  "/\")
                  )))
)"))
      (find-file (concat base "a/b/c/d/dfile1"))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (equal (proviso-get proviso-local-proj :tags-alist)
                     (list (list (concat "^\\(.*\\)" base "a/b/c/" "\\(.*\\)$")
                                 (concat base "a/b/c/.tags/first-tags")
                                 (concat base "a/b/c/.tags/second-tags")
                                 (concat base "a/b/c/.tags/third-tags")
                                 (concat base "a/b/c/.tags/fourth-tags"))
                           (list (concat "^\\(.*\\)"
                                         (file-name-as-directory absolute-root-dir)
                                         "\\(.*\\)$")
                                 (concat base "a/b/c/.tags/first-tags")
                                 (concat base "a/b/c/.tags/second-tags")
                                 (concat base "a/b/c/.tags/third-tags")
                                 (concat base "a/b/c/.tags/fourth-tags"))
                                   )))
      ;; open 2nd file, same project
      (find-file (concat base "a/b/c/d/dfile2"))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (equal (proviso-get proviso-local-proj :tags-alist)
                     (list (list (concat "^\\(.*\\)" base "a/b/c/" "\\(.*\\)$")
                                 (concat base "a/b/c/.tags/first-tags")
                                 (concat base "a/b/c/.tags/second-tags")
                                 (concat base "a/b/c/.tags/third-tags")
                                 (concat base "a/b/c/.tags/fourth-tags"))
                           (list (concat "^\\(.*\\)"
                                         (file-name-as-directory absolute-root-dir)
                                         "\\(.*\\)$")
                                 (concat base "a/b/c/.tags/first-tags")
                                 (concat base "a/b/c/.tags/second-tags")
                                 (concat base "a/b/c/.tags/third-tags")
                                 (concat base "a/b/c/.tags/fourth-tags"))
                                   )))
      ;; open 3rd file, new project
      (setq file-contents "(
:initfun (lambda (proj)
   (proviso-put proj :proj-alist
               '( (:name \"base\" :dir \"\")
                  (:name \"subdir\" :dir \"d2/\")
                  )))
)")
      (find-file (concat base "a/b/c2/d2/dfile3"))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c2/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c2"))
      (should (equal (proviso-get proviso-local-proj :tags-alist)
                     (list (list (concat "^\\(.*\\)" base "a/b/c2/" "\\(.*\\)$")
                                 (concat base "a/b/c2/.tags/base-tags")
                                 (concat base "a/b/c2/.tags/subdir-tags"))
                                   )))
      ;; switch back to initial buffer
      (switch-to-buffer "dfile1")
      (run-hooks 'post-command-hook)    ;simulate interactive use
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (eq proviso-local-proj proviso-curr-proj))
      (should (equal (proviso-get proviso-local-proj :tags-alist)
                     (list (list (concat "^\\(.*\\)" base "a/b/c/" "\\(.*\\)$")
                                 (concat base "a/b/c/.tags/first-tags")
                                 (concat base "a/b/c/.tags/second-tags")
                                 (concat base "a/b/c/.tags/third-tags")
                                 (concat base "a/b/c/.tags/fourth-tags"))
                           (list (concat "^\\(.*\\)"
                                         (file-name-as-directory absolute-root-dir)
                                         "\\(.*\\)$")
                                 (concat base "a/b/c/.tags/first-tags")
                                 (concat base "a/b/c/.tags/second-tags")
                                 (concat base "a/b/c/.tags/third-tags")
                                 (concat base "a/b/c/.tags/fourth-tags"))
                                   )))

      ;; clean up buffers
      (kill-buffer "dfile1")
      (kill-buffer "dfile2")
      (kill-buffer "dfile3")
      )))

;;; test_proviso-tags.el ends here
