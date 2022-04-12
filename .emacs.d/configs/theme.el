
(use-package alect-themes
  :ensure t
  :config
  (load-theme 'alect-black-alt t))

;; An implementation of Powerline
(use-package smart-mode-line
  :ensure t
  :demand t
  :init
  (setq sml/no-confirm-load-theme t)
  (sml/setup))
