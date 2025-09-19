;;; fnm.el --- Emacs Lisp FNM wrapper  -*- lexical-binding: t -*-

;; Author: Adam Bobrow
;; Maintainer: Adam Bobrow
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1")
;;                    (s)
;;                    (exec-path-from-shell))

;; Homepage: https://github.com/bobrowadam/fnm.el

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
(require 'exec-path-from-shell)
(require 'project)
(require 'rx)

(defun fnm-eval (eval-string)
  "Evaluate EVAL-STRING as a shell command after loading FNM environment."
  (shell-command-to-string (format "%s; eval \"$(fnm env --use-on-cd)\; %s\"; "
                                   shell-file-name
                                   eval-string)))

(defun fnm-current-node-version()
  "Return the current node version as a string."
  (s-trim-right (fnm-eval "fnm current")))

(defun fnm-default-node-version ()
  "Return the default node version specified by FNM."
  (cadr (s-match (rx bol (+ any) (group "v" (+ any)) space "default" eol)
                 (fnm-eval "fnm list"))))

(defun fnm-node-path (node-version)
  "Return the path to the node executable for NODE-VERSION."
  (seq-let (maybe-error path)
      (s-split "\n" (fnm-eval (format "fnm use %s\; which node" node-version)))
    (if (s-contains? "error" maybe-error)
        ;; TODO: maybe instead of throwing we can ask the user if he wants to install that version
        (error maybe-error)
      path)))

(defun fnm-node-bin-path (node-version)
  "Return the bin path the given NODE-VERSION."
  (s-replace "/node" "" (fnm-node-path node-version)))

(defun fnm-node-modules-path (node-version)
  "Return the node modules path for the given NODE-VERSION."
  (s-replace "/bin/node" "/lib/node_modules/" (fnm-node-path node-version)))

;;;###autoload
(defun fnm-use (&optional maybe-node-version)
  "Use the given MAYBE-NODE-VERSION.
If MAYBE-NODE-VERSION is nil, use the local project node version,
else, use the default global node version."
  (interactive (list (completing-read "Node version: " (get-available-fnm-node-versions))))
  (let* ((node-version (assert-node-version (or maybe-node-version
                                                (fnm-current-project-node-version)
                                                (fnm-default-node-version))))
         (new-path (concat (fnm-node-bin-path (assert-node-version node-version))
                           ":"
                           (s-join ":" (seq-remove
                                        (lambda (s) (s-contains-p "fnm_multishells" s))
                                        (s-split ":" (getenv "PATH")))))))
    (setenv "PATH" new-path)
    (setenv "NODE_PATH" (fnm-node-modules-path node-version))
    (exec-path-from-shell-setenv "PATH"
                                 new-path)
    node-version))

(defun assert-node-version (node-version)
  "Assert that NODE-VERSION is a valid node version string."
  (when (not (string-match (rx bol "v" (+ (or num ".")) eol)
                           node-version))
    (error "Not a valid node version: %s" node-version))
  node-version)

(defun get-available-fnm-node-versions ()
  "Return a list of available fnm node versions."
  (seq-remove 'null
              (mapcar
               (lambda (s)
                 (nth 1 (s-match (rx (group "v" (+? any))
                                     (group (or (: space "default")
                                                eol)))
                                 s)))
               (s-split "\n"
                        (fnm-eval "fnm list;")))))

(defmacro with-temporary-node-version (node-version body &optional verbose)
  "Use NODE-VERSION for the duration of BODY."
  (declare (indent 1))
  `(let ((current-global-node-version (fnm-current-node-version)))
     (progn
       (when ,verbose
         (message "Current global node version is %s. using node version %s for running in the current command"
                  current-global-node-version
                  ,node-version))
       (fnm-use ,node-version)
       ,body
       (fnm-use current-global-node-version))))

(defun fnm-current-project-node-version ()
  "Return the node version specified in the current project's dotfiles."
  (when-let* ((default-directory (locate-dominating-file (file-truename default-directory)
                                                         ".nvmrc"))
              (fnm-use-output (fnm-eval "fnm use;")))
    (if (s-match "error: Can't find version in dotfiles. Please provide a version manually to the command."
                 fnm-use-output)
        (progn (message "No node version found in dotfiles in current directory. directory: %s" default-directory)
               nil)
      (cadr (s-match (rx (+ any) (group "v" (+ any)))
                     fnm-use-output)))))

(provide 'fnm)

;;; fnm.el ends here
