# AGENTS.md - fnm.el Development Guide

## Build/Test Commands
- **Byte compile**: `emacs -batch -f batch-byte-compile fnm.el`
- **Load/test interactively**: `emacs -Q -l fnm.el`
- **Check syntax**: `emacs -batch --eval "(checkdoc-file \"fnm.el\")"`
- **No formal tests exist** - consider adding ERT tests for future development

## Code Style Guidelines
- **File header**: Use standard Emacs Lisp package header with GPL-3.0+ license
- **Package requires**: Declare dependencies in header: `(emacs "28.1") (s) (exec-path-from-shell)`
- **Lexical binding**: Always use `;;; -*- lexical-binding: t -*-`
- **Function naming**: Use `fnm-` prefix for all public functions, descriptive names
- **Documentation**: All functions must have docstrings describing parameters and return values
- **Interactive functions**: Mark with `;;;###autoload` for package installation
- **Error handling**: Use `error` for validation failures, `when-let*` for safe access
- **Dependencies**: Use `require` statements, prefer established packages (s.el, dash.el)
- **Indentation**: 2-space indentation, align function parameters consistently
- **Macros**: Use `declare (indent 1)` for proper indentation of custom macros
- **Comments**: Use `;;; Commentary:` and `;;; Code:` sections, end with `;;; package.el ends here`
- **Regular expressions**: Use `rx` macro instead of string regexps for readability
- **String manipulation**: Prefer s.el functions over built-in string functions