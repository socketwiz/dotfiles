
(package-initialize)

(setq use-package-always-ensure t)
(setq gc-cons-threshold most-positive-fixnum)

(use-package exec-path-from-shell
  :defer t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(require 'org)
(org-babel-load-file
 (expand-file-name "settings.org"
                   user-emacs-directory))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(initial-major-mode (quote emacs-lisp-mode))
 '(package-selected-packages
   (quote
    (racer rust-mode race flycheck-rust cargo irony irony-eldoc flycheck-irony dracula-theme atomic-chrome evil evil-surround evil-escape anzu evil-anzu undo-tree highlight-parentheses all-the-icons rainbow-mode flx flx-ido ido-completing-read+ helpful dockerfile-mode yasnippet tide web-mode markdown-mode exec-path-from-shell which-key use-package pt popup paredit highlight flyspell-popup magit ghub diff-hl company platformio-mode highlight-numbers parent-mode proceed editorconfig diminish scss-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

