;;; init.el --- Initialization file for Emacs

;; Author: Ricky Nelson <rickyn@socketwiz.com>

;;; Commentary:
;; Emacs Startup File --- initialization for Emacs

;;; Code:

(package-initialize)
(load "~/.emacs.d/settings.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(initial-major-mode (quote emacs-lisp-mode))
 '(package-selected-packages
   (quote
    (all-the-icons all-the-icons-dired anzu atomic-chrome cider command-log-mode company company-lsp dashboard diff-hl diminish dockerfile-mode doom-modeline doom-themes editorconfig elpy exec-path-from-shell expand-region evil evil-surround flycheck-irony flyspell-popup flx ghub helpful highlight highlight-numbers highlight-parentheses htmlize ido-completing-read+ ido-vertical-mode irony irony-eldoc js2-mode json-mode lsp-mode lsp-ui magit magit-popup markdown-mode org-agenda paredit parent-mode popup prescient proceed py-autopep8 rainbow-mode rg rjsx-mode rust-mode scss-mode smartparens tide undo-tree use-package web-mode which-key yasnippet))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(provide 'init)

;;; init.el ends here
