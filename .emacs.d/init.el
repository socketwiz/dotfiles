;;; init.el --- Initialization file for Emacs

;; Author: Ricky Nelson <rickyn@socketwiz.com>

;;; Commentary:
;; Emacs Startup File --- initialization for Emacs

;;; Code:
(load "~/.emacs.d/variables.el")
(load "~/.emacs.d/settings.el")
(load "~/.emacs.d/packages.el")
(load "~/.emacs.d/javascript.el")
(load "~/.emacs.d/rust.el")
(load "~/.emacs.d/python.el")
(load "~/.emacs.d/shell.el")

(provide 'init)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(gnutls-algorithm-priority "normal:-vers-tls1.3")
 '(package-selected-packages
   '(eglot all-the-icons all-the-icons-dired atomic-chrome avy bind-key command-log-mode dap diff-hl diminish dockerfile-mode doom-modeline doom-themes editorconfig elpy embark embark-consult exec-path-from-shell expand-region evil evil-surround flymake-eslint flymake-python-pyflakes flymake-shellcheck helpful indium jedi marginalia markdown-mode mu4e orderless org-mode org-bullets pipenv rainbow-mode rg rust-mode scss-mode smartparens swiper tide tree-sitter-langs tree-sitter undo-tree use-package vertico web-mode which-key yasnippet yaml-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
