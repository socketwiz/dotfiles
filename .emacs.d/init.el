
;; Automatically install missing packages
(setq package-list '(use-package)) ;; Bootstrap use-package so it can install the rest
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/"))
(unless package-archive-contents
  (package-refresh-contents))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(load "~/.emacs.d/configs/helm")
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
(use-package org :ensure t)
(use-package yasnippet
  :ensure t
  :bind (("TAB" . yas-expand))
  :config
  (yas-reload-all)
  :init
  (add-hook 'prog-mode-hook #'yas-minor-mode))
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  :init
  (setq undo-tree-auto-save-history t)
  ;; Save ALL undo histories to this location
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Open up our wiki-page
(defalias 'open-wiki '(lambda() (interactive)(find-file "~/org/wiki/index.org")))
;; package-list-packages, then U, then filter-packages-to-update to see what will be updated
(defun filter-packages-to-update()
  "Find packages marked for action in *Packages*."
  (interactive)
  (occur "^[A-Z]"))
(define-key package-menu-mode-map "a" #'package-menu-find-marks)

;; Disable startup screen
(setq inhibit-startup-screen t)
;; Display column number
(setq column-number-mode t)
;; Save ALL backup files to this location
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
;; Don't display the toolbar
(tool-bar-mode -1)
;; Set default font
(set-face-attribute 'default nil
                    :weight 'normal
                    :height 130
                    :width 'normal
                    :family "Source Code Pro")
