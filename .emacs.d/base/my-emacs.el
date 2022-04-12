
(defun configure-web-mode-flycheck-checkers ()
  ;; in order to have flycheck enabled in web-mode, add an entry to this
  ;; cond that matches the web-mode engine/content-type/etc and returns the
  ;; appropriate checker.
  (-when-let (checker (cond
                       ((string= web-mode-content-type "jsx")
                        'javascript-eslint)))
    (flycheck-mode)
    (flycheck-select-checker checker)))

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
    :diminish 'company-mode) 

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
    :ensure t

    :config
    ;; use eslint with web-mode for js[x]? files
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    (add-hook 'web-mode-hook #'configure-web-mode-flycheck-checkers))

  ;; init yasnippet
  (use-package yasnippet
    :ensure t
    :bind (("TAB" . yas-expand))
    :config
    (yas-reload-all)
    :mode (("\\.js[x]?'" . web-mode))
    :init
    (add-hook 'web-mode-hook #'yas-minor-mode))

  ;;init highlight numbers for prog modes
  (use-package highlight-numbers 
    :defer t 
    :init (add-hook 'prog-mode-hook 'highlight-numbers-mode))

  ;; keep backups
  (setq make-backup-files config-keep-backups))

(provide 'my-emacs)
