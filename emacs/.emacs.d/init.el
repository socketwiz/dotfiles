
(package-initialize)

(load-file (concat (file-name-directory user-emacs-directory)
		   "core/core-load-paths.el"))

(setq use-package-always-ensure t)
(setq gc-cons-threshold most-positive-fixnum)

(require 'core-my)
(require 'my-base)
(require 'my-langs)
(require 'my-utils)

(my/init)
(my/init-base)
(my/init-langs)
(my/init-utils)

(use-package exec-path-from-shell
  :defer t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(initial-major-mode (quote emacs-lisp-mode))
 '(package-selected-packages
   (quote
    (highlight-parentheses zerodark-theme all-the-icons rainbow-mode flx-ido ido-completing-read+ helpful dockerfile-mode yasnippet winum tide smex web-mode google-this markdown-mode linum-relative exec-path-from-shell which-key evil use-package pt popup paredit highlight flyspell-popup flx evil-surround magit diff-hl company platformio-mode highlight-numbers parent-mode evil-escape evil-anzu anzu proceed editorconfig diminish))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
