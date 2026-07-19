;;; fnm.el --- Emacs integration for Fast Node Manager  -*- lexical-binding: t -*-

;; Author: Adam Bobrow
;; Maintainer: Adam Bobrow
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Homepage: https://github.com/bobrowadam/fnm.el
;; Keywords: tools, languages

;; This file is not part of GNU Emacs.

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

;; fnm.el selects Node.js versions managed by Fast Node Manager (FNM) and
;; updates Emacs's PATH, `exec-path', and NODE_PATH.  Version resolution is
;; delegated to `fnm exec', including .nvmrc, .node-version, and package.json
;; engines.node support.  `fnm-auto-use-mode' switches versions as files open.

;;; Code:

(require 'json)
(require 'seq)
(require 'subr-x)

(defgroup fnm nil
  "Fast Node Manager integration."
  :group 'tools
  :prefix "fnm-")

(defcustom fnm-executable "fnm"
  "Name or absolute path of the FNM executable."
  :type 'file
  :group 'fnm)

(define-error 'fnm-error "FNM command failed")

(defconst fnm--node-info-script
  (concat "process.stdout.write(JSON.stringify({"
          "nodePath: process.execPath, version: process.version"
          "}))")
  "JavaScript used to retrieve Node installation information.")

(defvar fnm--active-node-info nil
  "Node installation information most recently resolved through FNM.")

(defvar fnm--active-node-bin nil
  "Node bin directory most recently added to variable `exec-path'.")

(defvar fnm--active-directory nil
  "Directory most recently resolved by `fnm-auto-use-mode'.")

(defun fnm--call (&rest arguments)
  "Run FNM with ARGUMENTS and return its trimmed output.
Signal `fnm-error' when FNM cannot be found or exits unsuccessfully."
  (let ((program (executable-find fnm-executable)))
    (unless program
      (signal 'fnm-error
              (list (format "Cannot find FNM executable: %s"
                            fnm-executable))))
    (with-temp-buffer
      (let* ((default-directory
              (if (file-remote-p default-directory)
                  temporary-file-directory
                default-directory))
             (process-environment (cons "NO_COLOR=1" process-environment))
             (status (apply #'process-file program nil '(t t) nil arguments))
             (output (string-trim (buffer-string))))
        (unless (and (integerp status) (zerop status))
          (signal 'fnm-error
                  (list (if (string-empty-p output)
                            (format "FNM exited with status %s" status)
                          output))))
        output))))

(defun fnm--resolve-node (&optional selector)
  "Return an alist describing the Node installation for SELECTOR.
When SELECTOR is nil, let FNM resolve the current directory or default alias."
  (when (and selector
             (or (not (stringp selector))
                 (string-empty-p (string-trim selector))))
    (signal 'wrong-type-argument (list 'non-empty-string-p selector)))
  (let* ((arguments
          (append '("exec" "--version-file-strategy=recursive"
                    "--resolve-engines=true")
                  (when selector (list (concat "--using=" selector)))
                  (list "node" "--eval" fnm--node-info-script)))
         (output (apply #'fnm--call arguments))
         (info (condition-case err
                   (json-parse-string output :object-type 'alist)
                 (json-parse-error
                  (signal 'fnm-error
                          (list (format "Invalid response from FNM: %s"
                                        (error-message-string err)))))))
         (node-path (alist-get 'nodePath info))
         (version (alist-get 'version info)))
    (unless (and (stringp node-path) (file-name-absolute-p node-path)
                 (stringp version))
      (signal 'fnm-error
              (list (format "Incomplete Node information from FNM: %S"
                            info))))
    info))

(defun fnm--node-bin (info)
  "Return the Node bin directory from INFO."
  (directory-file-name
   (file-name-directory (alist-get 'nodePath info))))

(defun fnm--node-modules (info)
  "Return the global node_modules directory from INFO."
  (let* ((node-directory
          (file-name-directory (alist-get 'nodePath info)))
         (prefix (if (eq system-type 'windows-nt)
                     node-directory
                   (file-name-directory
                    (directory-file-name node-directory)))))
    (file-name-as-directory
     (expand-file-name (if (eq system-type 'windows-nt)
                           "node_modules"
                         "lib/node_modules")
                       prefix))))

(defun fnm--same-directory-p (left right)
  "Return non-nil when LEFT and RIGHT name the same directory."
  (and left right
       (string-equal (file-name-as-directory (expand-file-name left))
                     (file-name-as-directory (expand-file-name right)))))

(defun fnm--managed-path-p (path)
  "Return non-nil when PATH is an old FNM path managed by this package."
  (let* ((multishell-root (getenv "FNM_MULTISHELL_PATH"))
         (multishell-bin (and multishell-root
                              (expand-file-name "bin" multishell-root))))
    (and (not (string-empty-p path))
         (or (fnm--same-directory-p path fnm--active-node-bin)
             (fnm--same-directory-p path multishell-bin)
             (string-match-p
              (rx (or "/" "\\") "fnm_multishells" (or "/" "\\"))
              path)
             (string-match-p
              (rx (or "/" "\\") "node-versions" (or "/" "\\")
                  (+ (not (any "/" "\\"))) (or "/" "\\")
                  "installation" (opt (or "/" "\\") "bin") eos)
              path)))))

(defun fnm--set-node-environment (info)
  "Update Emacs to use the Node installation described by INFO."
  (let* ((node-bin (fnm--node-bin info))
         (separator (if (characterp path-separator)
                        (char-to-string path-separator)
                      path-separator))
         (current-path (split-string (or (getenv "PATH") "")
                                     (regexp-quote separator)))
         (remaining-path (seq-remove #'fnm--managed-path-p current-path))
         (new-path (mapconcat #'identity
                              (cons node-bin remaining-path)
                              separator)))
    (setenv "PATH" new-path)
    (setenv "NODE_PATH" (fnm--node-modules info))
    (setq exec-path (append (parse-colon-path new-path)
                            (list exec-directory)))
    (setq-default eshell-path-env new-path)
    (setq fnm--active-node-info info
          fnm--active-node-bin node-bin)))

(defun fnm--environment-current-p ()
  "Return non-nil when this buffer uses the active Node binary."
  (let ((node (executable-find "node"))
        (expected (alist-get 'nodePath fnm--active-node-info)))
    (and node expected (file-equal-p node expected))))

(defun fnm--select (selector)
  "Select SELECTOR and return the resolved Node version."
  (when (file-remote-p default-directory)
    (user-error "FNM cannot configure Node for a remote directory"))
  (let ((info (fnm--resolve-node selector)))
    (fnm--set-node-environment info)
    (message "Using Node %s via FNM" (alist-get 'version info))
    (alist-get 'version info)))

;;;###autoload
(defun fnm-use (&optional node-version)
  "Configure Emacs to use NODE-VERSION.
NODE-VERSION may be any selector accepted by FNM.  When nil, FNM resolves
version sources recursively and falls back to its default alias."
  (interactive)
  (setq fnm--active-directory nil)
  (fnm--select node-version))

(defun fnm--available-node-versions ()
  "Return installed semantic Node versions reported by FNM."
  (let ((output (fnm--call "list"))
        versions)
    (dolist (line (split-string output "\n" t))
      (when (string-match
             (rx (group "v" (+ digit) "." (+ digit) "."
                        (+ (or alnum "." "-"))))
             line)
        (push (match-string 1 line) versions)))
    (delete-dups (nreverse versions))))

;;;###autoload
(defun fnm-use-version (node-version)
  "Prompt for and use an installed NODE-VERSION."
  (interactive
   (list (completing-read "Node version: "
                          (fnm--available-node-versions)
                          nil nil)))
  (fnm-use node-version))

;;;###autoload
(defun fnm-use-on-project-change ()
  "Ask FNM to switch Node after opening a file in a new directory."
  (unless (file-remote-p default-directory)
    (let ((directory (file-truename default-directory)))
      (condition-case err
          (cond
           ((not (equal directory fnm--active-directory))
            (fnm--select nil)
            (setq fnm--active-directory directory))
           ((not (fnm--environment-current-p))
            (fnm--set-node-environment fnm--active-node-info)))
        (error
         (message "FNM auto-use failed: %s"
                  (error-message-string err)))))))

;;;###autoload
(define-minor-mode fnm-auto-use-mode
  "Automatically ask FNM to select Node while opening files."
  :global t
  :group 'fnm
  (if fnm-auto-use-mode
      (progn
        (setq fnm--active-directory nil)
        (add-hook 'find-file-hook #'fnm-use-on-project-change)
        (fnm-use-on-project-change))
    (remove-hook 'find-file-hook #'fnm-use-on-project-change)
    (setq fnm--active-directory nil)))

(provide 'fnm)

;;; fnm.el ends here
