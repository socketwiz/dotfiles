
(defun switch-theme (theme)
  "Switch between light and dark themes"
  (interactive "slight or dark theme? ")
  (cond ((string= theme "light")
         (use-package moe-theme
           :ensure t)

         (load-theme 'moe-light t)
         (moe-theme-set-color 'green))
        ((string= theme "dark")
         (use-package zerodark-theme
           :ensure t)

         (load-theme 'zerodark t)
         (zerodark-setup-modeline-format))
        (t (message "Choices are only light or dark")))
  )

(defun my/init-makeup ()
  ;; Set regex syntax to string for re-builder
  (setq reb-re-syntax 'string)

  ;; Hide column numbers
  (setq column-number-mode t)

  ;; Always hightlight current line
  (global-hl-line-mode t)

  ;; Y or n is enough for me
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; Draw underline lower
  (setq x-underline-at-descent-line t)

  ;; Hide ui elements
  (setq inhibit-startup-message t)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (toggle-scroll-bar -1)

  ;; Font
  (set-face-attribute 'default nil
		      :family config-font-family
		      :height config-font-height)

  ;; Rainbow mode - displays color codes in their color
  (use-package rainbow-mode
    :delight
    :ensure t)

  ;; Theme
  ;; This required some fonts to be downloaded, run `all-the-icons-install-fonts` manually
  ;; https://github.com/emacs-jp/replace-colorthemes
  (use-package all-the-icons
    :ensure t)

  (switch-theme "dark")

  ;; Window numbers
  (use-package winum)
  (winum-mode)
  (setq winum-auto-setup-mode-line nil)

  ;; Relative line numbers + centered mode FTW
  (use-package linum-relative
    :diminish 'linum-relative-mode
    :init (setq linum-relative-current-symbol "")
    :config (linum-relative-global-mode))

  ;; parenthesis
  (show-paren-mode 1)
  (use-package highlight-parentheses
    :diminish 'highlight-parentheses-mode
    :config (add-hook 'prog-mode-hook #'highlight-parentheses-mode))

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
