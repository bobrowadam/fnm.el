;;; test-fnm.el --- Tests for fnm.el -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for fnm.el package using ERT (Emacs Lisp Regression Testing).
;; Run tests with: emacs -batch -l ert -l fnm.el -l test-fnm.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'fnm)

;;; Test utilities

(defvar test-fnm-mock-shell-commands nil
  "Alist of shell commands and their mocked outputs for testing.")

(defun test-fnm-mock-shell-command-to-string (command)
  "Mock implementation of `shell-command-to-string' for testing.
Returns the mocked output for COMMAND if it exists in `test-fnm-mock-shell-commands'."
  (let ((mock-output (cdr (assoc command test-fnm-mock-shell-commands))))
    (or mock-output
        (error "Unmocked shell command: %s" command))))

(defmacro test-fnm-with-mocked-commands (command-outputs &rest body)
  "Execute BODY with mocked shell commands.
COMMAND-OUTPUTS is an alist of (command . output) pairs."
  (declare (indent 1))
  `(let ((test-fnm-mock-shell-commands ,command-outputs))
     (cl-letf (((symbol-function 'shell-command-to-string) #'test-fnm-mock-shell-command-to-string))
       ,@body)))

;;; Tests for assert-node-version

(ert-deftest test-fnm-assert-node-version-valid ()
  "Test that valid node versions pass assertion."
  (should (equal "v18.17.0" (assert-node-version "v18.17.0")))
  (should (equal "v16.14.2" (assert-node-version "v16.14.2")))
  (should (equal "v14.21.3" (assert-node-version "v14.21.3")))
  (should (equal "v20.0.0" (assert-node-version "v20.0.0"))))

(ert-deftest test-fnm-assert-node-version-invalid ()
  "Test that invalid node versions throw errors."
  (should-error (assert-node-version "18.17.0"))  ; Missing 'v'
  (should-error (assert-node-version "v18"))       ; Missing patch version
  (should-error (assert-node-version "invalid"))   ; Invalid format
  (should-error (assert-node-version "v18.17"))    ; Missing patch version
  (should-error (assert-node-version "")))         ; Empty string

;;; Tests for fnm-eval

(ert-deftest test-fnm-eval ()
  "Test fnm-eval function."
  (test-fnm-with-mocked-commands
      '(("/bin/zsh; eval \"$(fnm env --use-on-cd)\; fnm current\"; " . "v18.17.0\n"))
    (should (equal "v18.17.0\n" (fnm-eval "fnm current")))))

;;; Tests for fnm-current-node-version

(ert-deftest test-fnm-current-node-version ()
  "Test fnm-current-node-version function."
  (test-fnm-with-mocked-commands
      '(("/bin/zsh; eval \"$(fnm env --use-on-cd)\; fnm current\"; " . "v18.17.0\n"))
    (should (equal "v18.17.0" (fnm-current-node-version)))))

(ert-deftest test-fnm-current-node-version-with-whitespace ()
  "Test fnm-current-node-version strips whitespace."
  (test-fnm-with-mocked-commands
      '(("/bin/zsh; eval \"$(fnm env --use-on-cd)\; fnm current\"; " . "  v18.17.0  \n\n"))
    (should (equal "v18.17.0" (fnm-current-node-version)))))

;;; Tests for fnm-default-node-version

(ert-deftest test-fnm-default-node-version ()
  "Test fnm-default-node-version function."
  (test-fnm-with-mocked-commands
      '(("/bin/zsh; eval \"$(fnm env --use-on-cd)\; fnm list\"; " . "* v16.14.2\n* v18.17.0 default\n* v20.0.0\n"))
    (should (equal "v18.17.0" (fnm-default-node-version)))))

(ert-deftest test-fnm-default-node-version-no-default ()
  "Test fnm-default-node-version when no default is set."
  (test-fnm-with-mocked-commands
      '(("/bin/zsh; eval \"$(fnm env --use-on-cd)\; fnm list\"; " . "* v16.14.2\n* v18.17.0\n* v20.0.0\n"))
    (should (equal nil (fnm-default-node-version)))))

;;; Tests for fnm-node-path

(ert-deftest test-fnm-node-path-success ()
  "Test fnm-node-path with successful command."
  (test-fnm-with-mocked-commands
      '(("/bin/zsh; eval \"$(fnm env --use-on-cd)\; fnm use v18.17.0\; which node\"; " . "Now using node v18.17.0\n/Users/test/.fnm/node-versions/v18.17.0/installation/bin/node\n"))
    (should (equal "/Users/test/.fnm/node-versions/v18.17.0/installation/bin/node"
                   (fnm-node-path "v18.17.0")))))

(ert-deftest test-fnm-node-path-error ()
  "Test fnm-node-path with error response."
  (test-fnm-with-mocked-commands
      '(("/bin/zsh; eval \"$(fnm env --use-on-cd)\; fnm use v99.99.99\; which node\"; " . "error: Requested version v99.99.99 is not currently installed\n"))
    (should-error (fnm-node-path "v99.99.99"))))

;;; Tests for fnm-node-bin-path

(ert-deftest test-fnm-node-bin-path ()
  "Test fnm-node-bin-path function."
  (test-fnm-with-mocked-commands
      '(("/bin/zsh; eval \"$(fnm env --use-on-cd)\; fnm use v18.17.0\; which node\"; " . "Now using node v18.17.0\n/Users/test/.fnm/node-versions/v18.17.0/installation/bin/node\n"))
    (should (equal "/Users/test/.fnm/node-versions/v18.17.0/installation/bin"
                   (fnm-node-bin-path "v18.17.0")))))

;;; Tests for fnm-node-modules-path

(ert-deftest test-fnm-node-modules-path ()
  "Test fnm-node-modules-path function."
  (test-fnm-with-mocked-commands
      '(("/bin/zsh; eval \"$(fnm env --use-on-cd)\; fnm use v18.17.0\; which node\"; " . "Now using node v18.17.0\n/Users/test/.fnm/node-versions/v18.17.0/installation/bin/node\n"))
    (should (equal "/Users/test/.fnm/node-versions/v18.17.0/installation/lib/node_modules/"
                   (fnm-node-modules-path "v18.17.0")))))

;;; Tests for get-available-fnm-node-versions

(ert-deftest test-get-available-fnm-node-versions ()
  "Test get-available-fnm-node-versions function."
  (test-fnm-with-mocked-commands
      '(("/bin/zsh; eval \"$(fnm env --use-on-cd)\; fnm list;\"; " . "* v16.14.2\n* v18.17.0 default\n* v20.0.0\n"))
    (should (equal '("v16.14.2" "v18.17.0" "v20.0.0")
                   (get-available-fnm-node-versions)))))

(ert-deftest test-get-available-fnm-node-versions-empty ()
  "Test get-available-fnm-node-versions with no versions installed."
  (test-fnm-with-mocked-commands
      '(("/bin/zsh; eval \"$(fnm env --use-on-cd)\; fnm list;\"; " . "\n"))
    (should (equal nil (get-available-fnm-node-versions)))))

;;; Tests for fnm-current-project-node-version

(ert-deftest test-fnm-current-project-node-version-found ()
  "Test fnm-current-project-node-version when .nvmrc is found."
  (let ((default-directory "/tmp/test-project/"))
    (cl-letf (((symbol-function 'locate-dominating-file)
               (lambda (_ file) (when (equal file ".nvmrc") "/tmp/test-project/"))))
      (test-fnm-with-mocked-commands
          '(("/bin/zsh; eval \"$(fnm env --use-on-cd)\; fnm use;\"; " . "Using node v18.17.0\n"))
        (should (equal "v18.17.0" (fnm-current-project-node-version)))))))

(ert-deftest test-fnm-current-project-node-version-not-found ()
  "Test fnm-current-project-node-version when no .nvmrc is found."
  (let ((default-directory "/tmp/test-project/"))
    (cl-letf (((symbol-function 'locate-dominating-file)
               (lambda (_ file) nil)))
      (should (equal nil (fnm-current-project-node-version))))))

(ert-deftest test-fnm-current-project-node-version-error ()
  "Test fnm-current-project-node-version when fnm returns error."
  (let ((default-directory "/tmp/test-project/"))
    (cl-letf (((symbol-function 'locate-dominating-file)
               (lambda (_ file) (when (equal file ".nvmrc") "/tmp/test-project/")))
              ((symbol-function 'message) (lambda (&rest _))))  ; Mock message to avoid output
      (test-fnm-with-mocked-commands
          '(("/bin/zsh; eval \"$(fnm env --use-on-cd)\; fnm use;\"; " . "error: Can't find version in dotfiles. Please provide a version manually to the command.\n"))
        (should (equal nil (fnm-current-project-node-version)))))))

;;; Tests for fnm-use (basic functionality without side effects)

(ert-deftest test-fnm-use-with-explicit-version ()
  "Test fnm-use with explicitly provided version."
  (let ((original-path (getenv "PATH"))
        (original-node-path (getenv "NODE_PATH")))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'exec-path-from-shell-setenv) (lambda (&rest _)))  ; Mock to avoid side effects
                    ((symbol-function 'completing-read) (lambda (&rest _) "v18.17.0")))
            (test-fnm-with-mocked-commands
                '(("/bin/zsh; eval \"$(fnm env --use-on-cd)\; fnm use v18.17.0\; which node\"; " . "Now using node v18.17.0\n/Users/test/.fnm/node-versions/v18.17.0/installation/bin/node\n"))
              (should (equal "v18.17.0" (fnm-use "v18.17.0"))))))
      ;; Restore environment
      (setenv "PATH" original-path)
      (setenv "NODE_PATH" original-node-path))))

;;; Integration tests (these require fnm to be actually installed)

(ert-deftest test-fnm-integration-eval ()
  "Integration test for fnm-eval (requires fnm installation)."
  :tags '(integration)
  (skip-unless (executable-find "fnm"))
  (let ((result (fnm-eval "echo 'test'")))
    (should (string-match-p "test" result))))

(ert-deftest test-fnm-integration-current-version ()
  "Integration test for fnm-current-node-version (requires fnm installation)."
  :tags '(integration)
  (skip-unless (executable-find "fnm"))
  (let ((version (fnm-current-node-version)))
    (should (or (null version) (string-match-p "^v[0-9]+\\.[0-9]+\\.[0-9]+$" version)))))

;;; Test runner helpers

(defun test-fnm-run-unit-tests ()
  "Run only unit tests (non-integration)."
  (interactive)
  (ert (lambda (test)
         (not (member 'integration (ert-test-tags test))))))

(defun test-fnm-run-integration-tests ()
  "Run only integration tests."
  (interactive)
  (ert '(tag integration)))

(defun test-fnm-run-all-tests ()
  "Run all tests."
  (interactive)
  (ert t))

(provide 'test-fnm)

;;; test-fnm.el ends here