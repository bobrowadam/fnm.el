;;; fnm.el --- Emacs Lisp FNM wrapper  -*- lexical-binding: t -*-

;; Author: Adam Bobrow
;; Maintainer: Adam Bobrow
;; Version: version
;; Package-Requires: ((s)
;;                    (cl-lib)
;;                    (exec-path-from-shell)
;;                    (project))

;; Homepage: homepage
;; Keywords: keywords


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; This package provides a wrapper around FNM, the node version manager.
;; It provides functions for using FNM to manage node versions from within Emacs.
;; It also provides functions for using the node version currently managed by FNM.
;; Currently, this package only supports MacOS and ZSH terminal but it should
;; be easy to add support for other operating systems and shells.

;;; Code:
(require 's)
(require 'cl-lib)
(require 'exec-path-from-shell)
(require 'project)
(require 'rx)

(defvar fnm/user-home-dir (expand-file-name "~/"))

(defun fnm-eval (eval-string)
  (shell-command-to-string (format "%s; eval \"$(fnm env --use-on-cd)\; %s\"; "
                                   shell-file-name
                                   eval-string)))

(defvar fnm-dir
  (cadr (s-match (rx "FNM_DIR=\"" (group (+ any)) "\"")
                 (fnm-eval "fnm env")))
  "The FNM directory")

(defun fnm-current-node-version()
  (s-trim-right (fnm-eval "fnm current")))

(defun fnm-default-node-version ()
  (cadr (s-match (rx bol (+ any) (group "v" (+ any)) space "default" eol)
                 (fnm-eval "fnm list"))))

(defun fnm-node-path (node-version)
  (car (s-match (rx (literal (format "%s%s" fnm/user-home-dir "Library/Caches/fnm_multishells"))
                    (+ any))
            (fnm-eval (format "fnm use %s\; which node" node-version)))))

(defun fnm-node-bin-path (node-version)
  "Return the bin path the given NODE-VERSION."
  (s-replace "/node" "" (fnm-node-path node-version)))

(defun fnm-node-modules-path (node-version)
  "Return the node modules path for the given NODE-VERSION."
  (s-replace "/bin/node" "/lib/node_modules/" (fnm-node-path node-version)))

;;;###autoload
(defun fnm-use (&optional maybe-node-version)
  "Use the given NODE-VERSION. If NODE-VERSION is nil, use the local
 project node version, if that's also nil, use the default global node version."
  (interactive (list (completing-read "sNode version: " (get-available-fnm-node-versions))))
  (let* ((node-version (assert-node-version (or maybe-node-version
                                                (fnm-current-project-node-version)
                                                (fnm-default-node-version))))
        (new-path (concat (fnm-node-bin-path (assert-node-version node-version))
                          ":"
                          (s-join ":" (cl-remove-if
                                       (lambda (s) (s-contains-p "fnm_multishells" s))
                                       (s-split ":" (getenv "PATH")))))))
    (setenv "PATH" new-path)
    (setenv "NODE_PATH" (fnm-node-modules-path node-version))
    (exec-path-from-shell-setenv "PATH"
                                 new-path)
    node-version))


(defun assert-node-version (node-version)
  (when (not (string-match (rx bol "v" (+ (or num ".")) eol)
                           node-version))
    (error "Not a valid node version: %s" node-version))
  node-version)

(defun get-available-fnm-node-versions ()
  "Return a list of available fnm node versions."
  (cl-remove-if 'nil
             (mapcar
              (lambda (s)
                (nth 1 (s-match (rx (group "v" (+? any))
                                    (group (or (: space "default")
                                               eol)))
                                s)))
              (s-split "\n"
                       (fnm-eval "fnm list;")))))

(defmacro with-temporary-node-version (node-version body)
  (declare (indent 1))
  `(let ((current-global-node-version (fnm-current-node-version)))
     (progn
       (message "Current global node version is %s. using node version %s for running in the current command"
                current-global-node-version
                ,node-version)
       (fnm-use ,node-version)
       ,body
       (fnm-use current-global-node-version))))

(defun fnm-current-project-node-version ()
  (if-let* ((project (project-current))
            (default-directory (project-root project))
            (fnm-use-output (fnm-eval "fnm use;")))
      (if (s-match "error: Can't find version in dotfiles. Please provide a version manually to the command."
                   fnm-use-output)
          (progn (message "No node version found in dotfiles in current directory. directory: %s" default-directory)
                 nil)
        (cadr (s-match (rx (+ any) (group "v" (+ any)))
                       fnm-use-output)))))

(provide 'fnm)
;;; fnm.el ends here
