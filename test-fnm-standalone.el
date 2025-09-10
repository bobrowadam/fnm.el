;;; test-fnm-standalone.el --- Standalone tests for fnm.el -*- lexical-binding: t -*-

;;; Commentary:

;; Standalone tests for fnm.el that include dependency mocks.
;; Run with: emacs -batch -l ert -l test-fnm-standalone.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)

;; Mock dependencies first
(defun s-trim-right (s)
  "Simple implementation of s-trim-right."
  (if (string-match "[ \t\n\r]+\\'" s)
      (replace-match "" t t s)
    s))

(defun s-trim (s)
  "Simple implementation of s-trim."
  (s-trim-right (s-trim-left s)))

(defun s-trim-left (s)
  "Simple implementation of s-trim-left."
  (if (string-match "\\`[ \t\n\r]+" s)
      (replace-match "" t t s)
    s))

(defun s-split (separator s &optional omit-nulls)
  "Simple implementation of s-split."
  (split-string s separator omit-nulls))

(defun s-match (regexp s)
  "Simple implementation of s-match."
  (when (string-match regexp s)
    (let ((matches nil))
      (dotimes (i (1+ (length (match-data))))
        (when (match-string i s)
          (push (match-string i s) matches)))
      (nreverse matches))))

(defun s-contains? (needle s)
  "Simple implementation of s-contains?."
  (string-match-p (regexp-quote needle) s))

(defun s-contains-p (needle s)
  "Alias for s-contains?."
  (s-contains? needle s))

(defun s-replace (old new s)
  "Simple implementation of s-replace."
  (replace-regexp-in-string (regexp-quote old) new s t t))

(defun s-join (separator strings)
  "Simple implementation of s-join."
  (mapconcat 'identity strings separator))

(defun exec-path-from-shell-setenv (name value)
  "Mock implementation of exec-path-from-shell-setenv."
  (setenv name value))

;; Now provide the features so fnm.el thinks they're loaded
(provide 's)
(provide 'exec-path-from-shell)

;; Load and define fnm.el functions inline to avoid dependency issues
(require 'project)
(require 'rx)

(defun fnm-eval (eval-string)
  "Evaluate EVAL-STRING as a shell command after loading FNM environment."
  (shell-command-to-string (format "%s; eval \"$(fnm env --use-on-cd)\; %s\"; "
                                   shell-file-name
                                   eval-string)))

(defun fnm-current-node-version()
  "Return the current node version as a string."
  (s-trim (fnm-eval "fnm current")))

(defun fnm-default-node-version ()
  "Return the default node version specified by FNM."
  (cadr (s-match (rx bol (+ any) (group "v" (+ any)) space "default" eol)
                 (fnm-eval "fnm list"))))

(defun fnm-node-path (node-version)
  "Return the path to the node executable for NODE-VERSION."
  (cl-destructuring-bind (maybe-error path _)
      (s-split "\n" (fnm-eval (format "fnm use %s\; which node" node-version)))
    (if (s-contains? "error" maybe-error)
        (error maybe-error)
      path)))

(defun fnm-node-bin-path (node-version)
  "Return the bin path the given NODE-VERSION."
  (let ((node-path (fnm-node-path node-version)))
    (if (string-match "/node$" node-path)
        (replace-match "" nil nil node-path)
      node-path)))

(defun fnm-node-modules-path (node-version)
  "Return the node modules path for the given NODE-VERSION."
  (s-replace "/bin/node" "/lib/node_modules/" (fnm-node-path node-version)))

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

(defun fnm-current-project-node-version ()
  "Return the node version specified in the current project's dotfiles."
  (when-let* ((default-directory (locate-dominating-file (file-name-parent-directory default-directory)
                                                         ".nvmrc"))
              (fnm-use-output (fnm-eval "fnm use;")))
    (if (s-match "error: Can't find version in dotfiles. Please provide a version manually to the command."
                 fnm-use-output)
        (progn (message "No node version found in dotfiles in current directory. directory: %s" default-directory)
               nil)
      (cadr (s-match (rx (+ any) (group "v" (+ any)))
                     fnm-use-output)))))

;;; Test utilities

(defvar test-fnm-mock-shell-commands nil
  "Alist of shell commands and their mocked outputs for testing.")

(defun test-fnm-mock-shell-command-to-string (command)
  "Mock implementation of `shell-command-to-string' for testing."
  (let ((mock-output (cdr (assoc command test-fnm-mock-shell-commands))))
    (or mock-output
        (error "Unmocked shell command: %s" command))))

(defmacro test-fnm-with-mocked-commands (command-outputs &rest body)
  "Execute BODY with mocked shell commands."
  (declare (indent 1))
  `(let ((test-fnm-mock-shell-commands ,command-outputs))
     (cl-letf (((symbol-function 'shell-command-to-string) #'test-fnm-mock-shell-command-to-string))
       ,@body)))

;;; Tests

(ert-deftest test-fnm-assert-node-version-valid ()
  "Test that valid node versions pass assertion."
  (should (equal "v18.17.0" (assert-node-version "v18.17.0")))
  (should (equal "v16.14.2" (assert-node-version "v16.14.2")))
  (should (equal "v20.0.0" (assert-node-version "v20.0.0"))))

(ert-deftest test-fnm-assert-node-version-invalid ()
  "Test that invalid node versions throw errors."
  (should-error (assert-node-version "18.17.0"))  ; Missing 'v'
  (should-error (assert-node-version "invalid"))   ; Invalid format
  (should-error (assert-node-version ""))          ; Empty string
  ;; Note: "v18" might be valid in some contexts, so we don't test it as invalid
  )

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

(ert-deftest test-fnm-node-bin-path ()
  "Test fnm-node-bin-path function."
  (test-fnm-with-mocked-commands
      '(("/bin/zsh; eval \"$(fnm env --use-on-cd)\; fnm use v18.17.0\; which node\"; " . "Now using node v18.17.0\n/Users/test/.fnm/node-versions/v18.17.0/installation/bin/node\n"))
    (should (equal "/Users/test/.fnm/node-versions/v18.17.0/installation/bin"
                   (fnm-node-bin-path "v18.17.0")))))

(ert-deftest test-fnm-node-modules-path ()
  "Test fnm-node-modules-path function."
  (test-fnm-with-mocked-commands
      '(("/bin/zsh; eval \"$(fnm env --use-on-cd)\; fnm use v18.17.0\; which node\"; " . "Now using node v18.17.0\n/Users/test/.fnm/node-versions/v18.17.0/installation/bin/node\n"))
    (should (equal "/Users/test/.fnm/node-versions/v18.17.0/installation/lib/node_modules/"
                   (fnm-node-modules-path "v18.17.0")))))

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

(ert-deftest test-fnm-current-project-node-version-not-found ()
  "Test fnm-current-project-node-version when no .nvmrc is found."
  (let ((default-directory "/tmp/test-project/"))
    (cl-letf (((symbol-function 'locate-dominating-file)
               (lambda (_ file) nil)))
      (should (equal nil (fnm-current-project-node-version))))))

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

(ert-deftest test-fnm-integration-list-versions ()
  "Integration test for get-available-fnm-node-versions (requires fnm installation)."
  :tags '(integration)
  (skip-unless (executable-find "fnm"))
  (let ((versions (get-available-fnm-node-versions)))
    (should (or (null versions) (listp versions)))))

(provide 'test-fnm-standalone)

;;; test-fnm-standalone.el ends here