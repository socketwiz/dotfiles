
;; Automatically install missing packages
(setq package-list '(use-package)) ;; Bootstrap use-package so it can install the rest
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(load "~/.emacs.d/configs/ivy")
(load "~/.emacs.d/configs/web-mode")
(load "~/.emacs.d/configs/flycheck")
(load "~/.emacs.d/configs/tide")
(load "~/.emacs.d/configs/projectile")
(load "~/.emacs.d/configs/evil")

(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-x g") 'magit-status))

(use-package linum-relative
  :ensure t
  :config
  ;; display line numbers for all files
  (add-hook 'find-file-hook 'linum-relative-mode)
  :init
  ;; display real line number instead of 0
  (setq linum-relative-current-symbol ""))

(use-package alect-themes
  :ensure t
  :config
  (load-theme 'alect-black-alt t))
;; Text completions
(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))
;; Command Emacs commands that are customized to make the best use of Ivy
(use-package counsel :ensure t)
;; Create customized short keybinds
(use-package hydra :ensure t)
(use-package ivy-hydra :ensure t)
;; Show an indicator when a buffer has been modified
(use-package ivy-rich
  :ensure t
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer))
(use-package org :ensure t)
(use-package yasnippet
  :ensure t
  :bind (("TAB" . yas-expand))
  :config
  (yas-reload-all)
  :init
  (add-hook 'prog-mode-hook #'yas-minor-mode))
;; Provide MRU data to file lists
(use-package smex :ensure t)
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  :init
  (setq undo-tree-auto-save-history t)
  ;; Save ALL undo histories to this location
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

;; Open up our wiki-page
(defalias 'open-wiki '(lambda() (interactive)(find-file "~/org/wiki/index.org")))

;; Disable startup screen
(setq inhibit-startup-screen t)
;; Display column number
(setq column-number-mode t)
;; Save ALL backup files to this location
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (tide web-mode use-package smex projectile magit linum-relative ivy-rich ivy-hydra flycheck evil counsel alect-themes)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:slant normal :weight normal :height 130 :width normal :family "Source Code Pro")))))
