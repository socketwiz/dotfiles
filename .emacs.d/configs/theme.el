
(use-package alect-themes
  :ensure t
  :config
  (load-theme 'alect-black-alt t))

;; An implementation of Powerline
(use-package telephone-line
  :ensure t
  :demand t
  :init (telephone-line-mode 1))
