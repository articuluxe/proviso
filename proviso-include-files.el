;;; proviso-include-files.el --- Support for project include files
;; Copyright (C) 2017-2019, 2021-2023, 2025  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Thursday, March 30, 2017
;; Version: 1.0
;; Modified Time-stamp: <2025-01-27 19:09:43 dharms>
;; Modified by: Dan Harms
;; Keywords: tools proviso project include files
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
;; Set settings having to do with include files.
;;

;;; Code:
(require 'proviso-core)
(require 'proviso-fulledit)
(require 'find-file)
(require 'flycheck)
(require 'cl-lib)
(require 'f)

(defvar proviso-cpp-language-standard "c++20"
  "Default c++ compiler language standard.")

(defun proviso--validate-include-files (proj)
  "Validate the set of include files of project PROJ."
  (let ((remote (proviso-get proj :remote-prefix))
        (root (proviso-get proj :root-dir))
        (lst (proviso-get proj :proj-alist))
        entry path)
    ;; in case there are no entries, add a default one (will resolve to root-dir)
    (when (zerop (length lst))
      (push (list :name (proviso-get proj :project-name)) lst))
    (setq proviso--ignore-load-errors nil)
    (setq lst
          (seq-filter (lambda (elt)
                        (setq entry (proviso-substitute-env-vars
                                     (plist-get elt :dir)))
                        (setq path
                              (concat
                               remote
                               (concat
                                (when (or (zerop (length entry))
                                          (f-relative? entry))
                                  root)
                                entry)))
                        (cond ((string-empty-p path) nil)
                              ((f-exists? path) path)
                              (proviso--ignore-load-errors nil)
                              (t
                               (proviso--query-error
                                proj
                                (format "%s does not exist!" path)))))
                      lst))
    (proviso-put proj :proj-alist lst)))

(defun proviso--set-include-files (proj)
  "Set include files according to PROJ's project definition."
  (let ((remote (proviso-get proj :remote-prefix))
        (root (proviso-get proj :root-dir))
        (lst (proviso-get proj :proj-alist))
        elt entry includes ff-includes)
    (dolist (element lst)
      (setq entry (proviso-substitute-env-vars (plist-get element :dir)))
      (setq elt (concat (when (or (null entry) (f-relative? entry)) root) entry))
      (push (file-name-as-directory elt) includes) ;ensure trailing slash
      (push (concat remote elt) ff-includes))
    (proviso-put proj :include-files (nreverse includes))
    ;; for ff-search-directories, prepend current dir and append root
    (proviso-put proj :include-ff-files
                 (append
                  (cl-remove-duplicates
                  ;; ff-search-directories doesn't want a trailing slash
                  (mapcar 'directory-file-name
                          (append '(".")
                                  (nreverse ff-includes)
                                  `,(list (concat remote root))))
                  :test 'string=)
                  (proviso-get proj :include-ff-files)))))

(defun proviso--gather-recursive-dirs (proj)
  "Recursively gather include files under the specified dirs of PROJ."
  (let ((remote (proviso-get proj :remote-prefix))
        (root (proviso-get proj :root-dir))
        (dirs (proviso-get proj :include-ff-recurse-dirs))
        elt entry lst)
    (dolist (element dirs)
      (setq entry (proviso-substitute-env-vars element))
      (setq elt (concat (when (or (null entry) (f-relative? entry)) root) entry))
      (push (concat remote (directory-file-name elt)) lst))
    (seq-mapcat #'proviso-fulledit-gather-dirs (nreverse lst))))

(defun proviso-gather-compiler-includes (compiler)
  "Return a list of include directories for COMPILER.  They will be absolute."
  (interactive)
  (let ((cmd (concat "echo | " compiler " -v -x c++ -E -MM - 2>&1 | "
                     "grep -A 20 starts | grep include | grep -v search")))
    (split-string (shell-command-to-string cmd))))

(defun proviso--include-on-file-opened (proj mode)
  "A file has been opened for project PROJ in mode MODE."
  (let* ((compiler (or (getenv "CXX") "g++"))
         (compiler-includes (proviso-gather-compiler-includes compiler))
         (recursive-dirs (proviso--gather-recursive-dirs proj)))
    (setq ff-search-directories
          (append
           (proviso-get proj :ff-search-directories)
           (proviso-get proj :include-ff-files)
           compiler-includes
           recursive-dirs))
    (when (bound-and-true-p c-buffer-is-cc-mode)
      (set (make-local-variable 'company-c-headers-path-user)
           (append
            (proviso-get proj :include-files)
            (list ".")
            recursive-dirs))
      (set (make-local-variable 'company-c-headers-path-system)
           (append
            compiler-includes
            (proviso-get proj :include-files)
            recursive-dirs))
      ;; set flymake for c++
      (when (eq major-mode 'c++-mode)
        ;; clang
        (set (make-local-variable 'flymake-collection-clang-include-path)
             (append
              compiler-includes
              (proviso-get proj :include-files)
              recursive-dirs))
        (add-to-list 'flymake-collection-clang-args
                     (concat "-std=" (or proviso-cpp-language-standard "c++20")))
        (add-to-list 'flymake-collection-clang-args
                     "-Wno-pragma-once-outside-header")
        ;; gcc
        (when (featurep 'flymake-collection-gcc)
          (set (make-local-variable 'flymake-collection-gcc-include-path)
               (append
                compiler-includes
                (proviso-get proj :include-files)
                recursive-dirs))
          (add-to-list 'flymake-collection-gcc-args
                       (concat "-std=" (or proviso-cpp-language-standard "c++20")))
          )
        ;; set compiler-include-dirs for flymake
    (when (executable-find "clang")
      (or (proviso-get proj :compiler-include-dirs)
          (proviso-put proj :compiler-include-dirs
                       (mapcar (lambda(x) (concat "-I" x))
                               compiler-includes))))
      (set (make-local-variable 'company-clang-arguments)
           (append
            (list (concat "-std=" (or proviso-cpp-language-standard "c++20")))
            ;; `(,(concat "-stdlib=" flycheck-clang-standard-library)
            ;;   ,(concat "-std=" flycheck-clang-language-standard)
            ;;   "-code-completion-macros" "-code-completion-patterns")
            (mapcar (lambda(x) (concat "-I" (expand-file-name x)))
                    (proviso-get proj :include-files))
            (list `,(concat "-I"
                            (proviso-get proj :remote-prefix)
                            (directory-file-name
                             (expand-file-name
                              (proviso-get proj :root-dir)))))
            (proviso-get proj :compiler-include-dirs)
            )))
    )))

(add-hook 'proviso-hook-on-project-init 'proviso--set-include-files)
;; add the validation last so it runs first
(add-hook 'proviso-hook-on-project-init 'proviso--validate-include-files)
(add-hook 'proviso-hook-on-file-opened 'proviso--include-on-file-opened)

(provide 'proviso-include-files)
;;; proviso-include-files.el ends here
