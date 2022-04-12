
(defun my/init-makeup ()
  ;; hide column numbers
  (setq column-number-mode t)

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

  ;; theme
  ;; This required some fonts to be downloaded, run `all-the-icons-install-fonts` manually
  ;; https://github.com/emacs-jp/replace-colorthemes
  (use-package color-theme-modern
    :ensure t
    :init
    (progn (load-theme 'tty-dark t t)
           (enable-theme 'tty-dark)))

  ;; ;; override red with magenta
  (custom-theme-set-faces
   'tty-dark
   '(cursor ((t (:background "magenta"))))
   '(diary-face ((t (:foreground "magenta"))))
   '(font-lock-keyword-face ((t (:foreground "magenta"))))
   '(italic ((t (:underline t :background "magenta"))))
   '(message-cited-text-face ((t (:foreground "magenta"))))
   '(modeline-buffer-id ((t (:background "white" :foreground "magenta"))))
   '(show-paren-match-face ((t (:background "magenta")))))

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
