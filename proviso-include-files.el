;;; proviso-include-files.el --- setup proviso project include files
;; Copyright (C) 2017-2018  Dan Harms (dharms)
;; Author: Dan Harms <enniomore@icloud.com>
;; Created: Thursday, March 30, 2017
;; Version: 1.0
;; Modified Time-stamp: <2018-10-18 15:21:09 dharms>
;; Modified by: Dan Harms
;; Keywords: tools proviso project include files
;; URL: https://github.com/articuluxe/proviso.git
;; Package-Requires: ((emacs "24.4"))

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
(require 'auto-complete-c-headers)
(require 'find-file)
(require 'flycheck)
(require 'auto-complete-clang)
(require 'cl-lib)
(require 'f)

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
                        (setq entry (plist-get elt :dir))
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
      (setq entry (plist-get element :dir))
      (setq elt (concat (when (or (null entry) (f-relative? entry)) root) entry))
      (push (file-name-as-directory elt) includes) ;ensure trailing slash
      (push (concat remote elt) ff-includes))
    (proviso-put proj :include-files includes)
    ;; for ff-search-directories, prepend current dir and append root
    (proviso-put proj :include-ff-files
                 (cl-remove-duplicates
                  ;; ff-search-directories doesn't want a trailing slash
                  (mapcar 'directory-file-name
                          (append '(".")
                                  ff-includes
                                  `,(list (concat remote root))))
                  :test 'string=))))

(defun proviso--gather-compiler-includes (compiler)
  "Return a list of include directories for COMPILER.  They will be absolute."
  (let ((cmd (concat "echo | " compiler " -v -x c++ -E - 2>&1 | "
                     "grep -A 20 starts | grep include | grep -v search")))
    (split-string (shell-command-to-string cmd))))

(defun proviso--include-on-file-opened (proj mode)
  "A file has been opened for project PROJ in mode MODE."
  (setq ff-search-directories (proviso-get proj :include-ff-files))
  (when (bound-and-true-p c-buffer-is-cc-mode)
    (set (make-local-variable 'company-c-headers-path-system)
         (proviso--gather-compiler-includes
          (or (getenv "CXX") "g++")))
    (set (make-local-variable 'company-c-headers-path-user)
         (proviso-get proj :include-files))
    ;; set flycheck for c++
    (when (eq major-mode 'c++-mode)
      ;; clang
      (when (proviso-get proj :clang-standard)
        (setq-local flycheck-clang-language-standard (proviso-get proj :clang-standard)))
      (set (make-local-variable 'flycheck-clang-standard-library)
           "libc++")
      (set (make-local-variable 'flycheck-clang-include-path)
           (proviso-get proj :include-files))
      ;; gcc
      (when (proviso-get proj :gcc-standard)
        (setq-local flycheck-gcc-language-standard (proviso-get proj :gcc-standard)))
      (set (make-local-variable 'flycheck-gcc-include-path)
           (proviso-get proj :include-files))
      ;; favor gcc over clang for now
      (add-to-list 'flycheck-disabled-checkers 'c/c++-clang)
      )
    ;; set 'compiler-include-dirs for ac-clang
    (when (executable-find "clang")
      (or (proviso-get proj :compiler-include-dirs)
          (proviso-put proj :compiler-include-dirs
                       (mapcar (lambda(x) (concat "-I" x))
                               (proviso--gather-compiler-includes
                                (or (getenv "CXX") "g++")))))
      (set (make-local-variable 'ac-clang-flags)
           (append
            `(,(concat "-stdlib=" flycheck-clang-standard-library)
              ,(concat "-std=" flycheck-clang-language-standard)
              "-code-completion-macros" "-code-completion-patterns")
            (mapcar (lambda(x) (concat "-I" (expand-file-name x)))
                    (proviso-get proj :include-files))
            (list `,(concat "-I"
                            (proviso-get proj :remote-prefix)
                            (directory-file-name
                             (expand-file-name
                              (proviso-get proj :root-dir)))))
            (proviso-get proj :compiler-include-dirs)
            )))
    ))

(add-hook 'proviso-hook-on-project-init 'proviso--set-include-files)
;; add the validation last so it runs first
(add-hook 'proviso-hook-on-project-init 'proviso--validate-include-files)
(add-hook 'proviso-hook-on-file-opened 'proviso--include-on-file-opened)

(provide 'proviso-include-files)
;;; proviso-include-files.el ends here
