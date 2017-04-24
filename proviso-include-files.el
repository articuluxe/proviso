;;; proviso-include-files.el --- setup proviso project include files
;; Copyright (C) 2017  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Thursday, March 30, 2017
;; Version: 1.0
;; Modified Time-stamp: <2017-04-24 08:29:36 dharms>
;; Modified by: Dan Harms
;; Keywords: proviso project include files

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
(require 'auto-complete-c-headers)
(require 'find-file)

(defun proviso--validate-include-files (proj)
  "Validate the set of include files of project PROJ."
  (let ((remote (proviso-get proj :remote-prefix))
        (root (proviso-get proj :root-dir))
        (lst (proviso-get proj :proj-alist))
        entry path)
    (setq proviso--ignore-load-errors nil)
    (setq lst
          (seq-filter (lambda (elt)
                        (setq entry (plist-get elt :dir))
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
    ;; in case there are no entries, add a default one (will resolve to root-dir)
    (when (zerop (length lst))
      (push (list) lst))
    (dolist (element lst)
      (setq entry (plist-get element :dir))
      (setq elt (concat (when (or (null entry) (f-relative? entry)) root) entry))
      (push (file-name-as-directory elt) includes) ;ensure trailing slash
      (push (concat remote elt) ff-includes))
    (proviso-put proj :include-files includes)
    ;; for ff-search-directories, prepend current dir and append root
    (setq includes (append '(".") includes
                           `,(concat remote root)))
    ;; ff-search-directories doesn't want a trailing slash
    (proviso-put proj :include-ff-files (mapcar 'directory-file-name ff-includes))
    ))

(defun proviso--gather-compiler-includes (compiler)
  "Return a list of include directories for COMPILER.  They will be absolute."
  (let ((cmd (concat "echo | " compiler " -v -x c++ -E - 2>&1 | "
                     "grep -A 20 starts | grep include | grep -v search")))
    (split-string (shell-command-to-string cmd))))
(defvar proviso-clang-standard-version "c++14")
(defvar proviso-gcc-standard-version "c++14")

(defun proviso--include-on-file-opened (proj mode)
  "A file has been opened for project PROJ in mode MODE."
  (setq ff-search-directories (proviso-get proj :include-ff-files))
  (when (bound-and-true-p c-buffer-is-cc-mode)
    (set (make-local-variable 'achead:include-directories)
         (proviso-get proj :include-files))
    ;; set 'compiler-include-dirs for ac-clang
    (when (executable-find "clang")
      (or (proviso-get proj :compiler-include-dirs)
          (proviso-put proj :compiler-include-dirs
                       (mapcar (lambda(x) (concat "-I" x))
                               (proviso--gather-compiler-includes
                                (or (getenv "CXX") "g++")))))
      (set (make-local-variable 'ac-clang-flags)
           (append
            `(,(concat "-std=" proviso-clang-standard-version)
              "-code-completion-macros" "-code-completion-patterns")
            (mapcar (lambda(x) (concat "-I" (expand-file-name x)))
                    (proviso-get proj :include-files))
            (list `,(concat "-I"
                            (proviso-get proj :remote-prefix)
                            (directory-file-name
                             (expand-file-name
                              (proviso-get proj :root-dir)))))
            (proviso-get proj :compiler-include-dirs_
            ))))
    ;; set flycheck for c++
    (when (eq major-mode 'c++-mode)
      (if (executable-find "clang")
          (progn                        ;clang
            (set (make-local-variable 'flycheck-clang-language-standard)
                 proviso-clang-standard-version)
            (set (make-local-variable 'flycheck-clang-standard-library)
                 "libc++")
            (set (make-local-variable 'flycheck-clang-include-path)
                 (proviso-get :proj :include-files))
            (add-to-list 'flycheck-disabled-checkers 'c/c++-gcc)
            )
        ;; gcc
        (set (make-local-variable 'flycheck-gcc-language-standard)
             proviso-gcc-standard-version)
        (set (make-local-variable 'flycheck-gcc-include-path)
             (proviso-get proj :include-files))
        (add-to-list 'flycheck-disabled-checkers 'c/c++-clang)
        ))))

(add-hook 'proviso-hook-on-project-init 'proviso--set-include-files)
;; add the validation last so it runs first
(add-hook 'proviso-hook-on-project-init 'proviso--validate-include-files)
(add-hook 'proviso-hook-on-file-opened 'proviso--include-on-file-opened)

(provide 'proviso-include-files)
;;; proviso-include-files.el ends here
