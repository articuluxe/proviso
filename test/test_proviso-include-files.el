;;; test_proviso-include-files.el --- test proviso include files
;; Copyright (C) 2017-2021  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Thursday, March 30, 2017
;; Version: 1.0
;; Modified Time-stamp: <2021-03-19 11:27:31 dharms>
;; Modified by: Dan Harms
;; Keywords: proviso project include files test
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

(ert-deftest proviso-include-open-project-empty-dir ()
  (proviso-test-reset-all)
  (let (file-contents)
    (cl-letf (((symbol-function 'proviso--eval-file)
               (lambda (_)
                 (unless (string-empty-p (string-trim file-contents))
                   (car (read-from-string file-contents))))))
      ;; open file
      (setq file-contents "(
:initfun (lambda (proj)
   (proviso-put proj :proj-alist
               '( (:name \"base\" :dir \"\")
                  )))
)")
      (find-file (concat base-test-dir "a/b/c/d/dfile1"))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base-test-dir "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (equal (proviso-get proviso-local-proj :include-files)
                  (list (concat base-test-dir "a/b/c/"))))
      (should (equal (proviso-get proviso-local-proj :include-ff-files)
                     (list "."
                           (concat base-test-dir "a/b/c"))))
      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-deftest proviso-include-open-project-absolute-dir ()
  (proviso-test-reset-all)
  (let (file-contents)
    (cl-letf (((symbol-function 'proviso--eval-file)
               (lambda (_)
                 (unless (string-empty-p (string-trim file-contents))
                   (car (read-from-string file-contents))))))
      ;; open file
      (setq file-contents (concat "(
:initfun (lambda (proj)
   (proviso-put proj :proj-alist
               '( (:name \"base\" :dir \""
                                  (file-name-as-directory absolute-root-dir)
                                  "\")
                  )))
)"))
      (find-file (concat base-test-dir "a/b/c/d/dfile1"))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base-test-dir "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (equal (proviso-get proviso-local-proj :include-files)
                  (list (file-name-as-directory absolute-root-dir))))
      (should (equal (proviso-get proviso-local-proj :include-ff-files)
                     (list "." absolute-root-dir (concat base-test-dir "a/b/c"))))
      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-deftest proviso-include-open-project-extra-ff-search-dirs ()
  (proviso-test-reset-all)
  (let (file-contents)
    (cl-letf (((symbol-function 'proviso--eval-file)
               (lambda (_)
                 (unless (string-empty-p (string-trim file-contents))
                   (car (read-from-string file-contents))))))
      ;; open file
      (setq file-contents (concat "(
:initfun (lambda (proj)
   (proviso-put proj :proj-alist
               '( (:name \"base\" :dir \""
                                  (file-name-as-directory absolute-root-dir)
                                  "\")
                  ))
   (proviso-put proj :include-ff-files
                '(\"existing\")))
)"))
      (find-file (concat base-test-dir "a/b/c/d/dfile1"))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base-test-dir "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (equal (proviso-get proviso-local-proj :include-files)
                  (list (file-name-as-directory absolute-root-dir))))
      (should (equal (proviso-get proviso-local-proj :include-ff-files)
                     (list "." absolute-root-dir
                           (concat base-test-dir "a/b/c")
                            "existing")))
      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-deftest proviso-include-open-project-relative-dir ()
  (proviso-test-reset-all)
  (let (file-contents)
    (cl-letf (((symbol-function 'proviso--eval-file)
               (lambda (_)
                 (unless (string-empty-p (string-trim file-contents))
                   (car (read-from-string file-contents))))))
      ;; open file
      (setq file-contents "(
:initfun (lambda (proj)
   (proviso-put proj :proj-alist
               '( (:name \"base\" :dir \"d/\")
                  )))
)")
      (find-file (concat base-test-dir "a/b/c/d/dfile1"))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base-test-dir "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (equal (proviso-get proviso-local-proj :include-files)
                  (list (concat base-test-dir "a/b/c/d/"))))
      (should (equal (proviso-get proviso-local-proj :include-ff-files)
                     (list "." (concat base-test-dir "a/b/c/d")
                           (concat base-test-dir "a/b/c"))))
      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-deftest proviso-include-open-project-relative-dir-environment-var ()
  (proviso-test-reset-all)
  (let ((process-environment '("TEMP=d"))
        file-contents)
    (cl-letf (((symbol-function 'proviso--eval-file)
               (lambda (_)
                 (unless (string-empty-p (string-trim file-contents))
                   (car (read-from-string file-contents))))))
      ;; open file
      (setq file-contents "(
:proj-alist ((:name \"base\" :dir \"$TEMP/\"))
)")
      (find-file (concat base-test-dir "a/b/c/d/dfile1"))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base-test-dir "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (equal (proviso-get proviso-local-proj :include-files)
                  (list (concat base-test-dir "a/b/c/d/"))))
      (should (equal (proviso-get proviso-local-proj :include-ff-files)
                     (list "." (concat base-test-dir "a/b/c/d")
                           (concat base-test-dir "a/b/c"))))
      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-deftest proviso-include-open-project-no-def ()
  (proviso-test-reset-all)
  (let (file-contents)
    (cl-letf (((symbol-function 'proviso--eval-file)
               (lambda (_)
                 (unless (string-empty-p (string-trim file-contents))
                   (car (read-from-string file-contents))))))
      ;; open file
      (setq file-contents "")
      (find-file (concat base-test-dir "a/b/c/d/dfile1"))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base-test-dir "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (equal (proviso-get proviso-local-proj :include-files)
                  (list (concat base-test-dir "a/b/c/"))))
      (should (equal (proviso-get proviso-local-proj :include-ff-files)
                     (list "." (concat base-test-dir "a/b/c")
                           )))
      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-deftest proviso-include-open-project-dirs-without-trailing-slashes ()
  (proviso-test-reset-all)
  (let (file-contents)
    (cl-letf (((symbol-function 'proviso--eval-file)
               (lambda (_)
                 (unless (string-empty-p (string-trim file-contents))
                   (car (read-from-string file-contents))))))
      ;; open file
      (setq file-contents (concat "(
:initfun (lambda (proj)
   (proviso-put proj :proj-alist
               '( (:name \"one\" :dir \"\")
                  (:name \"two\" :dir \""
                                  absolute-root-dir
                                  "\")
                  (:name \"three\" :dir \"d\")
                  )))
)"))
      (find-file (concat base-test-dir "a/b/c/d/dfile1"))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base-test-dir "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (equal (proviso-get proviso-local-proj :include-files)
                     (list
                      (concat base-test-dir "a/b/c/d/")
                      (file-name-as-directory absolute-root-dir)
                      (concat base-test-dir "a/b/c/")
                      )))
      (should (equal (proviso-get proviso-local-proj :include-ff-files)
                     (list
                      "."
                      (concat base-test-dir "a/b/c/d")
                      absolute-root-dir
                      (concat base-test-dir "a/b/c")
                      )))
      ;; clean up buffers
      (kill-buffer "dfile1")
      )))

(ert-deftest proviso-include-open-project-dirs-switch-projects ()
  (proviso-test-reset-all)
  (let (file-contents)
    (cl-letf (((symbol-function 'proviso--eval-file)
               (lambda (_)
                 (unless (string-empty-p (string-trim file-contents))
                   (car (read-from-string file-contents))))))
      ;; open file
      (setq file-contents (concat "(
:initfun (lambda (proj)
   (proviso-put proj :proj-alist
               '( (:name \"one\" :dir \"\")
                  (:name \"two\" :dir \""
                                  absolute-root-dir
                                  "\")
                  (:name \"three\" :dir \"d\")
                  )))
)"))
      (find-file (concat base-test-dir "a/b/c/d/dfile1"))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base-test-dir "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (equal (proviso-get proviso-local-proj :include-files)
                     (list
                      (concat base-test-dir "a/b/c/d/")
                      (file-name-as-directory absolute-root-dir)
                      (concat base-test-dir "a/b/c/")
                      )))
      (should (equal (proviso-get proviso-local-proj :include-ff-files)
                     (list
                      "."
                      (concat base-test-dir "a/b/c/d")
                      absolute-root-dir
                      (concat base-test-dir "a/b/c")
                      )))
      (should (local-variable-p 'ff-search-directories (get-buffer "dfile1")))
      (should (equal ff-search-directories
                     (list "."
                           (concat base-test-dir "a/b/c/d")
                           absolute-root-dir
                           (concat base-test-dir "a/b/c"))))
      ;; open 2nd file, same project
      (find-file (concat base-test-dir "a/b/c/d/dfile2"))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base-test-dir "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (local-variable-p 'ff-search-directories (get-buffer "dfile2")))
      (should (equal ff-search-directories
                     (list "."
                           (concat base-test-dir "a/b/c/d")
                           absolute-root-dir
                           (concat base-test-dir "a/b/c"))))
      ;; open 3rd file, new project
      (setq file-contents "(
:initfun (lambda (proj)
   (proviso-put proj :proj-alist
               '( (:name \"base\" :dir \"d2/\")
                  )))
)")
      (find-file (concat base-test-dir "a/b/c2/d2/dfile3"))
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base-test-dir "a/b/c2/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c2"))
      (should (local-variable-p 'ff-search-directories (get-buffer "dfile3")))
      (should (equal ff-search-directories
                     (list "."
                           (concat base-test-dir "a/b/c2/d2")
                           (concat base-test-dir "a/b/c2"))))
      ;; switch back to initial buffer
      (switch-to-buffer "dfile1")
      (run-hooks 'post-command-hook)    ;simulate interactive use
      (should (string= (proviso-get proviso-local-proj :root-dir)
                       (concat base-test-dir "a/b/c/")))
      (should (string= (proviso-get proviso-local-proj :project-name)
                       "c"))
      (should (eq proviso-local-proj proviso-curr-proj))
      (should (local-variable-p 'ff-search-directories (get-buffer "dfile1")))
      (should (equal ff-search-directories
                     (list "."
                           (concat base-test-dir "a/b/c/d")
                           absolute-root-dir
                           (concat base-test-dir "a/b/c"))))

      ;; clean up buffers
      (kill-buffer "dfile1")
      (kill-buffer "dfile2")
      (kill-buffer "dfile3")
      )))


;;; test_proviso-include-files.el ends here
