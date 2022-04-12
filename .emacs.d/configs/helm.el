
(use-package helm-core
  :ensure t)

(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x C-r" . helm-recentf))
  :config
  (setq helm-buffers-fuzzy-matching t)
  (helm-mode 1))

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

(use-package helm-git-grep
  :ensure t)

(use-package helm-descbinds
  :bind (("C-h b" . helm-descbinds))
  :ensure t)
