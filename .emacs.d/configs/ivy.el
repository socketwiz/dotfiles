
(use-package ivy
  :ensure t
  :diminish (ivy-mode . "")
  :config
  (ivy-mode 1)
  :init
  (setq projectile-completion-system 'ivy))
