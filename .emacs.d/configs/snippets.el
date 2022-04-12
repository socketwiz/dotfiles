
(use-package yasnippet
  :ensure t
  :bind (("TAB" . yas-expand))
  :config
  (yas-reload-all)
  :mode (("\\.js[x]?'" . web-mode))
  :init
  (add-hook 'web-mode-hook #'yas-minor-mode))
