;;; settings.el --- General settings for Emacs

;; Author: Ricky Nelson <rickyn@socketwiz.com>

;;; Commentary:
;; Settings not required by any particular package

;;; Code:

;; Hide ui elements
(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1)

;; y or n is enough for me
(fset 'yes-or-no-p 'y-or-n-p)

;; Always highlight current line
(global-hl-line-mode t)

;; Turn on line numbers
(global-display-line-numbers-mode)
;; Make line numbers relative
(menu-bar-display-line-numbers-mode 'relative)

;; Put these documents in current buffer so they can be read and exited with minimum effort
(add-to-list 'display-buffer-alist '("*Apropos*" display-buffer-same-window))
(add-to-list 'display-buffer-alist '("*Buffer List*" display-buffer-same-window))
(add-to-list 'display-buffer-alist '("*Help*" display-buffer-same-window))
(add-to-list 'display-buffer-alist '("*Info*" display-buffer-same-window))

(add-hook 'emacs-lisp-mode-hook 'show-smartparens-mode)

;; Font
(set-face-attribute 'default nil
                    :family config-font-family
                    :height config-font-height)

;; Enable narrow to region functionality
(put 'narrow-to-region 'disabled nil)

;; Frequently accessed files (C-x r j <letter>)
;; jump-to-register
(set-register ?b '(file . "~/org/blog/index.org"))
(set-register ?i '(file . "~/org/agenda/inbox.org"))
(set-register ?k '(file . "~/org/work.org"))
(set-register ?s '(file . "~/.emacs.d/init.el"))
(set-register ?w '(file . "~/org/wiki/index.org"))

;; * Keybindings that replace defaults (except F5)
(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "C-c C-e") 'mu4e)
(global-set-key (kbd "C-h f") 'helpful-callable)
(global-set-key (kbd "C-h k") 'helpful-key)
(global-set-key (kbd "C-h v") 'helpful-variable)
(global-set-key (kbd "C-s") 'occur)
(global-set-key (kbd "C-x C-e") 'pp-eval-last-sexp)
(global-set-key (kbd "C-x C-z") 'nil)

(when (not (featurep 'evil-mode))
  ;; In evil-mode <C-z> swaps between normal and emacs states
  ;; In terminal <C-z> suspends Emacs to the background, but doesn't work so well in GUI
  (if (not (string= window-system nil))
      (global-unset-key (kbd "C-z"))))

(with-eval-after-load 'replace
  ;; Make occur focus results
  (add-hook #'occur-hook
            #'(lambda ()
               (switch-to-buffer-other-window "*Occur*")))
  ;; After a selection is made, close the occur buffer
  (defun after-occur-mode-goto-occurrence (&optional event)
   (delete-windows-on "*Occur*"))
  (advice-add 'occur-mode-goto-occurrence :filter-return 'after-occur-mode-goto-occurrence))


(provide 'settings)
;;; settings.el ends here
