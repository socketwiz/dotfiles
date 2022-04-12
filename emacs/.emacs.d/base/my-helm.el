
(defun my/init-helm () 
  (use-package helm 
    :diminish 'helm-mode) 

  (helm-mode 1) 
  (helm-autoresize-mode 1) 

  (use-package pt) 

  (use-package helm-pt 
    :diminish 'helm-mode
    :bind (("C-c p *" . helm-projectile-pt)))

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

  (use-package smex :ensure t)
  (use-package helm-smex :ensure t)

  (use-package helm-fuzzier
    :ensure t)

  (helm-fuzzier-mode 1)

  (use-package helm-git-grep
    :ensure t)

  (use-package helm-descbinds
    :ensure t
    :bind (("C-h b" . helm-descbinds)))

  ;; Helm custom settings
  (setq helm-prevent-escaping-from-minibuffer t
        helm-bookmark-show-location 'bottom
        helm-display-header-line nil
        helm-split-window-in-side-p t
        helm-always-two-windows t
        helm-echo-input-in-header-line t
        helm-imenu-execute-action-at-once-if-one nil
        helm-display-function 'my/display-helm-window
        ;; use CURL, not url-retrieve-synchronously
        helm-net-prefer-curl t
        ;; fuzzy matching
        helm-recentf-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-mode-fuzzy-match t
        helm-completion-in-region-fuzzy-match t
        ;; Set the helm-mini sources
        helm-mini-default-sources '(helm-source-buffers-list
                                    helm-source-recentf
                                    helm-source-bookmarks
                                    helm-source-buffer-not-found))

  (my/helm-init-keybindings))

(defvar helm-display-buffer-regexp 
  `("*.*helm.**" (display-buffer-in-side-window) 
    (inhibit-same-window . t) 
    (side . ,'bottom) 
    (window-width . 0.6) 
    (window-height . 0.4)))

(defun my/display-helm-window (buffer) 
  "Display the Helm window always at bottom and full width."
  (let ((display-buffer-alist (list helm-display-buffer-regexp))) 
    (helm-default-display-buffer buffer)))

(defun my/helm-init-keybindings () 
  ;; tab to provide completation
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) 
  (define-key helm-map (kbd "C-j") 'helm-next-line) 
  (define-key helm-map (kbd "C-k") 'helm-previous-line) 
  (define-key helm-map (kbd "C-h") 'helm-next-source) 
  (define-key helm-map (kbd "C-l") (kbd "RET")) 
  (with-eval-after-load 'helm-files (dolist (keymap (list helm-find-files-map helm-read-file-map)) 
				      (define-key keymap (kbd "C-l")
					'helm-execute-persistent-action) 
				      (define-key keymap (kbd "C-h")
					'helm-find-files-up-one-level))))

(provide 'my-helm)
