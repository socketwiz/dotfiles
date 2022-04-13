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
   '(all-the-icons all-the-icons-dired anzu amx atomic-chrome cider command-log-mode company company-lsp consult diff-hl diminish dockerfile-mode dired doom-modeline doom-themes editorconfig embark embark-consult elpy eterm-256color exec-path-from-shell expand-region exwm exwm-randr evil evil-surround flymake-eslint flymake-shellcheck flyspell-popup flx flx-ido ghub helpful highlight highlight-numbers highlight-parentheses htmlize ido-completing-read+ ido-vertical-mode irony irony-eldoc jedi js2-mode json-mode lsp-mode lsp-ui magit magit-popup marginalia markdown-mode orderless org-agenda org-bullets paredit parent-mode pipenv popup prescient proceed project py-autopep8 rainbow-mode rg rjsx-mode rust-mode rustic scss-mode smartparens tide undo-tree use-package web-mode which-key xref yaml-mode yasnippet vertico))
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
