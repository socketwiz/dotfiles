
(defun my/init-makeup ()
  ;; hide column numbers
  (setq column-number-mode t)

  ;; always hightlight current line
  (global-hl-line-mode t)
  (setq evil-normal-state-cursor '("DarkGoldenrod2" box))
  (setq evil-insert-state-cursor '("charteuse3"  box))
  (setq evil-visual-state-cursor '("gray" box))
  (setq evil-operator-state-cursor '("cyan" box))
  (setq evil-replace-state-cursor '("chocolate" box))
  (setq evil-motion-state-cursor '("plum3" box))
  (setq evil-emacs-state-cursor  '("SkyBlue2" box))

  ;; Y or n is enough for me
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; draw underline lower
  (setq x-underline-at-descent-line t)

  ;; hide ui elements
  (setq inhibit-startup-message t)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (toggle-scroll-bar -1)

  ;; font
  (set-face-attribute 'default nil
		      :family config-font-family
		      :height config-font-height)

  ;; theme
  (use-package powerline
    :ensure t)

  (use-package moe-theme
    :after (powerline)
    :ensure t
    :config
    (progn
      (moe-light)
      (powerline-moe-theme)
      (show-paren-mode 1)
      (setq show-paren-style 'expression)))

  ;; window numbers
  (use-package winum)
  (winum-mode)
  (setq winum-auto-setup-mode-line nil)

  ;; evil-anzu for improving search result rendering
  (use-package evil-anzu
    :config (global-anzu-mode +1)
    :diminish 'anzu-mode)

  ;; relative line numbers + centered mode FTW
  (use-package linum-relative
    :diminish 'linum-relative-mode
    :init (setq linum-relative-current-symbol "")
    :config (linum-relative-global-mode))

  ;; Frequently accessed files (C-x r j <letter>)
  (set-register ?i '(file . "~/.emacs.d/init.el"))
  (set-register ?w '(file . "~/org/wiki/index.org"))

  ;; Save ALL backup files to this location
  (setq backup-directory-alist `(("." . "~/.emacs.d/backups")))

  ;; Disable re-center of the cursor to the middle of page when scroll hits top or bottom of the page
  (setq scroll-conservatively 101)

  ;; Automatically scroll the compilation buffer
  (setq compilation-scroll-output t))

(provide 'core-makeup)
