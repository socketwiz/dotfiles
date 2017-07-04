
(use-package evil
  :ensure t

  :config
  (evil-mode 1)

  (defvar my-leader-map (make-sparse-keymap)
    "Keymap for \"leader key\" shortcuts.")

  (define-key my-leader-map (kbd "SPC") 'counsel-M-x)
  (define-key my-leader-map (kbd "b b") 'ivy-switch-buffer)
  (define-key my-leader-map (kbd "e l") 'flycheck-list-errors)
  (define-key my-leader-map (kbd "e v") 'flycheck-verify-setup)
  (define-key my-leader-map (kbd "f f") 'counsel-find-file)
  (define-key my-leader-map (kbd "f r") 'counsel-recentf)
  (define-key my-leader-map (kbd "g s") 'magit-status)
  (define-key my-leader-map (kbd "p f") 'projectile-find-file)
  (define-key my-leader-map (kbd "p p") 'projectile-switch-project)
  (define-key my-leader-map "*" 'counsel-pt)

  ;; change the "leader" key to space
  (define-key evil-normal-state-map (kbd "SPC") my-leader-map)
  ;; disable this key sequence so we can use it in tide-mode
  (define-key evil-normal-state-map (kbd "M-.") nil)
  )

