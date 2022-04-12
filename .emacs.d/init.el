
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
(package-initialize)

;; General configuration
(load "~/.emacs.d/configs/packages")
(load "~/.emacs.d/configs/theme")
(load "~/.emacs.d/configs/evil")
(load "~/.emacs.d/configs/line-num")
(load "~/.emacs.d/configs/projectile")
(load "~/.emacs.d/configs/helm")
(load "~/.emacs.d/configs/undo")
(load "~/.emacs.d/configs/markdown")
(load "~/.emacs.d/configs/org")
(load "~/.emacs.d/configs/which-key")

;; JavaScript development
(load "~/.emacs.d/configs/web")
(load "~/.emacs.d/configs/flycheck")
(load "~/.emacs.d/configs/tide")

;; Lisp development
(load "~/.emacs.d/configs/paredit")

;; General development
(load "~/.emacs.d/configs/magit")
(load "~/.emacs.d/configs/completions")
(load "~/.emacs.d/configs/snippets")

;; Open up our wiki-page
(defalias 'open-wiki '(lambda() (interactive)(find-file "~/org/wiki/index.org")))
;; package-list-packages, then U, then filter-packages-to-update to see what will be updated
(defun filter-packages-to-update()
  "Find packages marked for action in *Packages*."
  (interactive)
  (occur "^[A-Z]"))
(define-key package-menu-mode-map "a" #'filter-packages-to-update)

;; Display column number
(setq column-number-mode t)
;; Save ALL backup files to this location
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
;; Disable re-center of the cursor to the middle of page when scroll hits top or bottom of the page
(setq scroll-conservatively 101)

;; Disable startup screen
(setq inhibit-startup-screen t)
;; Don't display the menu, toolbar or scroll-bar
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)

;; Set default font
(set-face-attribute 'default nil
                    :weight 'normal
                    :height 130
                    :width 'normal
                    :family "Source Code Pro")
;; Start the server every time on startup
(server-start)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(package-selected-packages
   (quote
    (telephone-line telephone-line-config helm-descbinds yasnippet web-mode use-package tide smex markdown-mode magit linum-relative ivy-rich ivy-hydra helm-projectile helm-git-grep flycheck-color-mode-line evil counsel company alect-themes smex helm-smex helm-flx helm-fuzzier powerline helm-pt paredit which-key evil-surround diff-hl))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
