
(use-package evil
  :ensure t

  :config
  (evil-mode 1)

  ;; disable this key sequence so we can use it in tide-mode
  (define-key evil-normal-state-map (kbd "M-.") nil)
  ;; disable this key so we can use it in web-mode
  (define-key evil-normal-state-map (kbd "C-n") nil)
  ;; disable this key so we can use it in yasnippets
  (define-key evil-motion-state-map (kbd "TAB") nil))

(use-package evil-surround
  :ensure t
  :config (global-evil-surround-mode 1))
