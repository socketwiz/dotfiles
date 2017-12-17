
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
    (paradox spaceline powerline moe-theme helpful dockerfile-mode smart-mode-line color-theme-modern swiper counsel counsel-projectile hydra ivy yasnippet winum tide smex web-mode google-this markdown-mode highlight-parentheses linum-relative exec-path-from-shell zerodark-theme which-key evil use-package skewer-mode pt popup paredit highlight numbers flyspell-popup flx evil-surround evil-magit diff-hl company platformio-mode highlight-numbers parent-mode evil-escape evil-anzu anzu proceed editorconfig diminish))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
