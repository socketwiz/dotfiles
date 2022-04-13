;;; init.el --- Scripting languages

;; Author: Ricky Nelson <rickyn@socketwiz.com>

;;; Commentary:
;; Settings to make shell scripting easier

;;; Code:
(use-package flymake-shellcheck
  :if config-enable-shell-mode
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

(provide 'shell)
;;; shell.el ends here
