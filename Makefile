# Makefile for fnm.el

EMACS ?= emacs
BATCH = $(EMACS) -batch -Q -L .

# Default target
.PHONY: all
all: test

# Run all tests (with dependencies, requires s and exec-path-from-shell packages)
.PHONY: test
test:
	$(BATCH) -l ert -l fnm.el -l test-fnm.el -f ert-run-tests-batch-and-exit

# Run unit tests only (excludes integration tests, with dependencies)
.PHONY: test-unit
test-unit:
	$(BATCH) -l ert -l fnm.el -l test-fnm.el \
		--eval "(ert-run-tests-batch-and-exit (lambda (test) (not (member 'integration (ert-test-tags test)))))"

# Run standalone tests (no external dependencies required)
.PHONY: test-standalone
test-standalone:
	$(BATCH) -l ert -l test-fnm-standalone.el -f ert-run-tests-batch-and-exit

# Run integration tests only (requires fnm to be installed)
.PHONY: test-integration
test-integration:
	$(BATCH) -l ert -l test-fnm-standalone.el \
		--eval "(ert-run-tests-batch-and-exit '(tag integration))"

# Byte compile the package
.PHONY: compile
compile:
	$(BATCH) -f batch-byte-compile fnm.el

# Clean compiled files
.PHONY: clean
clean:
	rm -f *.elc

# Check for byte compilation warnings
.PHONY: check
check: compile

# Install package dependencies (requires package.el setup)
.PHONY: install-deps
install-deps:
	$(BATCH) --eval "(progn \
		(require 'package) \
		(setq package-archives '((\"melpa\" . \"https://melpa.org/packages/\") \
		                         (\"gnu\" . \"https://elpa.gnu.org/packages/\"))) \
		(package-initialize) \
		(package-refresh-contents) \
		(package-install 's) \
		(package-install 'exec-path-from-shell))"

# Interactive test session
.PHONY: test-interactive
test-interactive:
	$(EMACS) -Q -L . -l fnm.el -l test-fnm.el \
		--eval "(progn (ert t) (switch-to-buffer \"*ert*\"))"

# Help target
.PHONY: help
help:
	@echo "Available targets:"
	@echo "  test             - Run all tests (requires dependencies)"
	@echo "  test-unit        - Run unit tests only (requires dependencies)"
	@echo "  test-standalone  - Run standalone tests (no external deps)"
	@echo "  test-integration - Run integration tests only (requires fnm)"
	@echo "  compile          - Byte compile the package"
	@echo "  check            - Check for compilation warnings"
	@echo "  clean            - Remove compiled files"
	@echo "  install-deps     - Install package dependencies"
	@echo "  test-interactive - Run tests in interactive Emacs session"
	@echo "  help             - Show this help message"