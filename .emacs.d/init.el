;;; init.el --- Initialization file for Emacs

;; Author: Ricky Nelson <rickyn@socketwiz.com>

;;; Commentary:
;; Emacs Startup File --- initialization for Emacs

;; Set the path from the shell
;; call after (package-initialize)
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-copy-envs '("PATH" "NODE_PATH")))

(let ((node-bin (string-trim (shell-command-to-string "rtx which node"))))
  (when (and node-bin (not (string-empty-p node-bin)))
    (setenv "PATH" (concat (file-name-directory node-bin) ":" (getenv "PATH")))
    (add-to-list 'exec-path (file-name-directory node-bin))))


;;; Code:
(load "~/.emacs.d/variables.el")
(load "~/.emacs.d/settings.el")
(load "~/.emacs.d/packages.el")
(load "~/.emacs.d/javascript.el")
(load "~/.emacs.d/rust.el")
(load "~/.emacs.d/shell.el")
(load "~/.emacs.d/elisp.el")

(provide 'init)

(message "Current exec-path: %s" exec-path)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(gnutls-algorithm-priority "normal:-vers-tls1.3")
 '(package-selected-packages
   '(all-the-icons all-the-icons-dired bind-key casual command-log-mode consult copilot diff-hl diminish dockerfile-mode doom-modeline doom-themes editorconfig exec-path-from-shell evil evil-surround flymake-eslint flymake-shellcheck graphql-mode helpful indium marginalia markdown-mode orderless org-bullets org-mode paredit prettier-js quelpa quelpa-use-package rainbow-mode rg smartparens swiper terraform-mode tree-sitter tree-sitter-langs treesit treesitter undo-tree vertico web-mode which-key yaml-mode yasnippet)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
