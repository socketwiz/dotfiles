
(use-package evil
  :ensure t

  :config
  (evil-mode 1)

  (defvar my-leader-map (make-sparse-keymap)
    "Keymap for \"leader key\" shortcuts.")

  ;; Recreate the Spacemacs keybindings that I used most
  (define-key my-leader-map (kbd "SPC") 'helm-M-x)
  (define-key my-leader-map (kbd "b b") 'helm-buffers-list)
  (define-key my-leader-map (kbd "e l") 'flycheck-list-errors)
  (define-key my-leader-map (kbd "e v") 'flycheck-verify-setup)
  (define-key my-leader-map (kbd "f f") 'helm-find-files)
  (define-key my-leader-map (kbd "f e d") '(lambda() (interactive)(find-file "~/.emacs.d/init.el")))
  (define-key my-leader-map (kbd "f r") 'helm-recentf)
  (define-key my-leader-map (kbd "g s") 'magit-status)
  (define-key my-leader-map (kbd "p f") 'projectile-find-file)
  (define-key my-leader-map (kbd "p p") 'projectile-switch-project)
  (define-key my-leader-map "*" 'helm-git-grep)

  ;; change the "leader" key to space
  (define-key evil-normal-state-map (kbd "SPC") my-leader-map)
  ;; disable this key sequence so we can use it in tide-mode
  (define-key evil-normal-state-map (kbd "M-.") nil)
  ;; disable this key so we can use it in yasnippets
  (define-key evil-motion-state-map (kbd "TAB") nil)
  )

