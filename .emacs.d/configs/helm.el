
(use-package helm-core
  :ensure t)

(use-package helm
  :ensure t
  :config
  (setq helm-buffers-fuzzy-matching t)
  (helm-mode 1))

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

(use-package helm-git-grep
  :ensure t)
