
(defun my/init-emacs () 
  (setq initial-major-mode config-scratch-mode) 

  ;; init undo-tree
  (use-package undo-tree 
    :defer t 
    :diminish 'undo-tree-mode) 
  (setq undo-tree-visualizer-timestamps t) 
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-auto-save-history t)
  ;; save all undo histories to this location
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  (defadvice undo-tree-make-history-save-file-name
      (after undo-tree activate)
    (setq ad-return-value (concat ad-return-value ".gz")))
  (global-undo-tree-mode)

  ;; init company mode
  (use-package company 
    :diminish 'company-mode
    :config
    (add-hook 'after-init-hook 'global-company-mode))

  (use-package "eldoc"
    :diminish eldoc-mode
    :commands turn-on-eldoc-mode
    :defer t)

  ;; init flyspell
  (use-package flyspell 
    :config (add-hook 'prog-mode-hook 'flyspell-prog-mode) 
    :diminish 'flyspell-mode) 
  (use-package flyspell-popup 
    :config (define-key flyspell-mode-map (kbd "C-;") #'flyspell-popup-correct)) 
  (define-key popup-menu-keymap (kbd "C-j") 'popup-next) 
  (define-key popup-menu-keymap (kbd "C-k") 'popup-previous) 
  (define-key popup-menu-keymap (kbd "C-l") 'popup-select)

  ;; init flycheck
  (use-package flycheck
    :diminish flycheck-mode
    :ensure t)

  ;; init yasnippet
  (use-package yasnippet
    :ensure t
    :bind (("TAB" . yas-expand))
    :config
    (yas-reload-all))

  ;;init highlight numbers for prog modes
  (use-package highlight-numbers 
    :defer t 
    :init (add-hook 'prog-mode-hook 'highlight-numbers-mode))

  ;; keep backups
  (setq make-backup-files config-keep-backups)

  ;; Org mode
  (use-package org
    :ensure t
    :config (progn
              (setq org-directory "~/org")
              (setq org-startup-folded nil))
    )

  ;; Respect editor configs
  (use-package editorconfig
    :ensure t
    :diminish editorconfig-mode
    :config
    (editorconfig-mode 1))

  (use-package dockerfile-mode
    :defer t)

  (use-package helpful
    :ensure t)

  (use-package powerline
    :ensure t)

  (use-package spaceline
    :ensure t
    :init
    (require 'spaceline-config)
    :config
    (progn
      (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
      (spaceline-emacs-theme)))

  (use-package moe-theme
    :ensure t
    :config
    (moe-theme-set-color 'green)))

(provide 'my-emacs)
