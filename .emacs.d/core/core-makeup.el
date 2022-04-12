
(defun my/init-makeup ()
  ;; hide column numbers
  (setq column-number-mode nil)

  ;; always hightlight current line
  (global-hl-line-mode t)

  ;; y or n is enough for me
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
  (set-frame-parameter nil 'fullscreen 'fullboth)

  ;; theme
  ;; This required some fonts to be downloaded, run `all-the-icons-install-fonts` manually
  (use-package zerodark-theme
    :config (zerodark-setup-modeline-format))

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

  (use-package centered-cursor-mode
    :config (global-centered-cursor-mode)
    :diminish 'centered-cursor-mode)

  ;; parenthesis
  (show-paren-mode 1)
  (use-package highlight-parentheses
    :diminish 'highlight-parentheses-mode
    :config (add-hook 'prog-mode-hook #'highlight-parentheses-mode))

  ;; Frequently accessed files (C-x r j <letter>)
  (set-register ?i '(file . "~/.emacs.d/init.el"))
  (set-register ?w '(file . "~/org/wiki/index.org"))

  ;; package-list-packages, then U, then filter-packages-to-update to see what will be updated
  (defun filter-packages-to-update()
    "Find packages marked for action in *Packages*."
    (interactive)
    (occur "^[A-Z]"))
  (define-key package-menu-mode-map "a" #'filter-packages-to-update)

  ;; Save ALL backup files to this location
  (setq backup-directory-alist `(("." . "~/.emacs.d/backups")))

  ;; Disable re-center of the cursor to the middle of page when scroll hits top or bottom of the page
  (setq scroll-conservatively 101))

(provide 'core-makeup)
