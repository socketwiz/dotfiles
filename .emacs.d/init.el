;;; init.el --- Initialization file for Emacs

;; Author: Ricky Nelson <rickyn@socketwiz.com>

;;; Commentary:
;; Emacs Startup File --- initialization for Emacs

;;; Code:

(load "~/.emacs.d/settings.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(gnutls-algorithm-priority "normal:-vers-tls1.3")
 '(initial-major-mode 'emacs-lisp-mode)
 '(package-selected-packages
   '(all-the-icons all-the-icons-dired anzu amx atomic-chrome cider command-log-mode company consult diff-hl diminish dockerfile-mode dired doom-modeline doom-themes editorconfig eglot embark embark-consult elpy exec-path-from-shell expand-region evil evil-surround flymake-eslint flymake-shellcheck flyspell-popup ghub helpful highlight highlight-numbers highlight-parentheses htmlize irony irony-eldoc jedi magit magit-popup marginalia markdown-mode orderless org-agenda org-bullets paredit parent-mode pipenv popup prescient proceed project rainbow-mode rg rust-mode scss-mode smartparens tide undo-tree use-package web-mode which-key xref yaml-mode yasnippet vertico))
 '(safe-local-variable-values
   '((eval set
           (make-local-variable 'projectile-project-root)
           (append projectile-project-root
                   (locate-dominating-file default-directory ".dir-locals.el"))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(provide 'init)

;;; init.el ends here
