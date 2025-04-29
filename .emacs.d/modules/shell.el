;;; shell.el --- Scripting languages support -*- lexical-binding: t; -*-

;; Author: Ricky Nelson <rickyn@socketwiz.com>

;;; Commentary:
;; Settings to make shell scripting easier.

;;; Code:

(use-package flymake-shellcheck
  :if (and config-enable-shell-mode (executable-find "shellcheck"))
  :commands (flymake-shellcheck-load)
  :hook (sh-mode . flymake-shellcheck-load))

(provide 'shell)
;;; shell.el ends here
