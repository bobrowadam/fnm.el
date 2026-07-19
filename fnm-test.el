;;; fnm-test.el --- Tests for fnm.el  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Adam Bobrow

;; This file is not part of GNU Emacs.

;;; Commentary:

;; ERT tests for fnm.el.

;;; Code:

(require 'ert)
(require 'fnm)

(defconst fnm-test--node-info
  '((nodePath . "/tmp/fnm/node-versions/v20.20.0/installation/bin/node")
    (version . "v20.20.0")))

(ert-deftest fnm-test-resolve-node-delegates-resolution-to-fnm ()
  (dolist (case '((nil
                   "exec" "--version-file-strategy=recursive"
                   "--resolve-engines=true" "node" "--eval")
                  ("20"
                   "exec" "--version-file-strategy=recursive"
                   "--resolve-engines=true" "--using=20" "node" "--eval")))
    (let (received-arguments)
      (cl-letf (((symbol-function 'fnm--call)
                 (lambda (&rest arguments)
                   (setq received-arguments arguments)
                   "{\"nodePath\":\"/tmp/node\",\"version\":\"v20.0.0\"}")))
        (fnm--resolve-node (car case))
        (should (equal (butlast received-arguments)
                       (cdr case)))
        (should (equal (car (last received-arguments))
                       fnm--node-info-script))))))

(ert-deftest fnm-test-use-passes-selector-and-updates-environment ()
  (let (received-selector received-info)
    (cl-letf (((symbol-function 'fnm--resolve-node)
               (lambda (&optional selector)
                 (setq received-selector selector)
                 fnm-test--node-info))
              ((symbol-function 'fnm--set-node-environment)
               (lambda (info) (setq received-info info)))
              ((symbol-function 'message) #'ignore))
      (should (equal (fnm-use "20") "v20.20.0"))
      (should (equal received-selector "20"))
      (should (equal received-info fnm-test--node-info)))))

(ert-deftest fnm-test-use-lets-fnm-resolve-project-or-default ()
  (let (received-selector)
    (cl-letf (((symbol-function 'fnm--resolve-node)
               (lambda (&optional selector)
                 (setq received-selector selector)
                 fnm-test--node-info))
              ((symbol-function 'fnm--set-node-environment) #'ignore)
              ((symbol-function 'message) #'ignore))
      (fnm-use)
      (should-not received-selector))))

(ert-deftest fnm-test-set-environment-replaces-old-fnm-paths ()
  (let ((process-environment (copy-sequence process-environment))
        (exec-path nil)
        (fnm--active-node-info nil)
        (fnm--active-node-bin "/old/fnm/bin")
        (exec-directory "/emacs/libexec/"))
    (setenv "PATH" (mapconcat #'identity
                              '("/old/fnm/bin"
                                "/tmp/fnm_multishells/123/bin"
                                "/usr/bin")
                              (if (characterp path-separator)
                                  (char-to-string path-separator)
                                path-separator)))
    (fnm--set-node-environment fnm-test--node-info)
    (should (equal (car exec-path)
                   "/tmp/fnm/node-versions/v20.20.0/installation/bin/"))
    (should-not (string-match-p "fnm_multishells" (getenv "PATH")))
    (should (string-match-p "/usr/bin" (getenv "PATH")))
    (should (equal (getenv "NODE_PATH")
                   "/tmp/fnm/node-versions/v20.20.0/installation/lib/node_modules/"))))

(ert-deftest fnm-test-set-environment-preserves-empty-path-components ()
  (let ((process-environment (copy-sequence process-environment))
        (exec-path nil)
        (fnm--active-node-info nil)
        (fnm--active-node-bin nil)
        (exec-directory "/emacs/libexec/"))
    (setenv "PATH" (concat path-separator "/usr/bin" path-separator))
    (fnm--set-node-environment fnm-test--node-info)
    (should (string-suffix-p
             (concat path-separator "/usr/bin" path-separator)
             (getenv "PATH")))))

(ert-deftest fnm-test-auto-use-caches-only-successful-directories ()
  (let ((fnm--active-directory "old")
        (default-directory temporary-file-directory))
    (cl-letf (((symbol-function 'fnm--select)
               (lambda (_selector) (error "Switch failed")))
              ((symbol-function 'message) #'ignore))
      (fnm-use-on-project-change)
      (should (equal fnm--active-directory "old")))))

(ert-deftest fnm-test-auto-use-reuses-resolution-in-same-directory ()
  (let ((fnm--active-directory (file-truename temporary-file-directory))
        (fnm--active-node-info fnm-test--node-info)
        (default-directory temporary-file-directory)
        selected reapplied)
    (cl-letf (((symbol-function 'fnm--select)
               (lambda (_selector) (setq selected t)))
              ((symbol-function 'fnm--environment-current-p) (lambda () nil))
              ((symbol-function 'fnm--set-node-environment)
               (lambda (info) (setq reapplied info))))
      (fnm-use-on-project-change)
      (should-not selected)
      (should (equal reapplied fnm-test--node-info)))))

(ert-deftest fnm-test-available-versions-parses-semver-only ()
  (cl-letf (((symbol-function 'fnm--call)
             (lambda (&rest _arguments)
               "* v20.20.0\n* v24.15.0 default\n* system")))
    (should (equal (fnm--available-node-versions)
                   '("v20.20.0" "v24.15.0")))))

(provide 'fnm-test)

;;; fnm-test.el ends here
