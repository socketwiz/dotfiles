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
 '(initial-major-mode 'emacs-lisp-mode)
 '(package-selected-packages
   '(pipenv jedi org-bullets all-the-icons all-the-icons-dired anzu amx atomic-chrome cider command-log-mode company company-lsp diff-hl diminish dockerfile-mode dired doom-modeline doom-themes editorconfig elpy eterm-256color exec-path-from-shell expand-region exwm exwm-randr evil evil-surround flymake-eslint flymake-shellcheck flyspell-popup flx flx-ido ghub helpful highlight highlight-numbers highlight-parentheses htmlize ido-completing-read+ ido-vertical-mode irony irony-eldoc js2-mode json-mode lsp-mode lsp-ui magit magit-popup markdown-mode org-agenda paredit parent-mode popup prescient proceed project projectile py-autopep8 rainbow-mode rg rjsx-mode rust-mode rustic scss-mode smartparens tide undo-tree use-package web-mode which-key xref yaml-mode yasnippet))
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
