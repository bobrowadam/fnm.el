#!/bin/bash

# run-tests.sh - Test runner script for fnm.el

set -e

EMACS=${EMACS:-emacs}
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

cd "$SCRIPT_DIR"

echo "Running fnm.el tests..."
echo "Using Emacs: $(which $EMACS)"
echo "Emacs version: $($EMACS --version | head -n1)"
echo

# Check if required dependencies are available
if ! $EMACS -batch -Q --eval "(require 's)" 2>/dev/null; then
    echo "Warning: 's' package not found. Some tests may fail."
    echo "Install with: M-x package-install RET s RET"
    echo
fi

if ! $EMACS -batch -Q --eval "(require 'exec-path-from-shell)" 2>/dev/null; then
    echo "Warning: 'exec-path-from-shell' package not found. Some tests may fail."
    echo "Install with: M-x package-install RET exec-path-from-shell RET"
    echo
fi

# Run tests based on argument
case "${1:-standalone}" in
    "all")
        echo "Running all tests (unit + integration)..."
        make test
        ;;
    "unit")
        echo "Running unit tests only..."
        make test-unit
        ;;
    "standalone")
        echo "Running standalone tests (no external dependencies)..."
        make test-standalone
        ;;
    "integration")
        echo "Running integration tests only..."
        if ! command -v fnm &> /dev/null; then
            echo "Error: fnm not found. Integration tests require fnm to be installed."
            echo "Install fnm: https://github.com/Schniz/fnm#installation"
            exit 1
        fi
        make test-integration
        ;;
    "interactive")
        echo "Starting interactive test session..."
        make test-interactive
        ;;
    *)
        echo "Usage: $0 [all|unit|standalone|integration|interactive]"
        echo ""
        echo "  all         - Run all tests (requires dependencies)"
        echo "  unit        - Run unit tests only (requires dependencies)"
        echo "  standalone  - Run standalone tests (default, no external deps)"
        echo "  integration - Run integration tests (requires fnm)"
        echo "  interactive - Run tests in interactive Emacs session"
        exit 1
        ;;
esac

echo "Tests completed successfully!"