
(use-package yasnippet
  :ensure t
  :bind (("TAB" . yas-expand))
  :config
  (yas-reload-all)
  :init
  (add-hook 'prog-mode-hook #'yas-minor-mode))
