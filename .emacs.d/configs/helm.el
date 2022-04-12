
(use-package helm-flx
  :ensure t
  :init
  (helm-flx-mode 1)
  ;; garbage collection
  (defun eos/minibuffer-setup-hook ()
    (setq gc-cons-threshold most-positive-fixnum))

  (defun eos/minibuffer-exit-hook ()
    ;; 20mb
    (setq gc-cons-threshold (* 20 1024 1024)))

  (add-hook 'minibuffer-setup-hook #'eos/minibuffer-setup-hook)
  (add-hook 'minibuffer-exit-hook #'eos/minibuffer-exit-hook))

(use-package helm-fuzzier
  :ensure t
  :init
  (helm-fuzzier-mode 1))

(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x C-f" . helm-find-files)
         ("C-x C-r" . helm-recentf))
  :config
  (setq
   ;; use CURL, not url-retrieve-synchronously
   helm-net-prefer-curl t
   ;; don't use recentf stuff in helm-ff, I use C-x C-r for this
   helm-ff-file-name-history-use-recentf nil
   ;; fuzzy matching
   helm-recentf-fuzzy-match t
   helm-buffers-fuzzy-matching t
   helm-M-x-fuzzy-match t
   helm-mode-fuzzy-match t
   helm-completion-in-region-fuzzy-match t)
  (helm-mode 1))

(use-package smex :ensure t)
(use-package helm-smex :ensure t)

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

(use-package helm-git-grep
  :ensure t)

(use-package helm-descbinds
  :ensure t
  :bind (("C-h b" . helm-descbinds)))

(use-package helm-pt
  :ensure t
  :bind (("C-c p *" . helm-projectile-pt)))

(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
