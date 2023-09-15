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
   '(all-the-icons all-the-icons-dired bind-key command-log-mode consult diff-hl diminish dockerfile-mode doom-modeline doom-themes editorconfig exec-path-from-shell evil evil-surround flymake-eslint flymake-shellcheck helpful graphql-mode indium marginalia markdown-mode orderless org-mode org-bullets prettier-js rainbow-mode rg smartparens swiper terraform-mode undo-tree vertico web-mode which-key yasnippet yaml-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
