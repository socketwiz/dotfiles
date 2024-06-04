;;; elips.el --- Language JavaScript/TypeScript

;; Author: Ricky Nelson <rickyn@socketwiz.com>

;;; Commentary:
;; Settings for emacs lisp

;; Paredit configuration
(use-package paredit
  :ensure t
  :hook (emacs-lisp-mode . paredit-mode))

;; Smartparens configuration
(use-package smartparens
  :ensure t
  :hook (emacs-lisp-mode . smartparens-mode))

;; Automatically indent elisp code on save
(defun my-indent-buffer ()
  "Indent the entire buffer."
  (indent-region (point-min) (point-max)))

(defun my-setup-emacs-lisp-mode ()
  "Custom configurations for Emacs Lisp mode."
  (add-hook 'before-save-hook 'my-indent-buffer nil t))

;; Automatically enable prettify-symbols-mode in emacs-lisp-mode
(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)
(add-hook 'emacs-lisp-mode-hook 'my-setup-emacs-lisp-mode)


(provide 'elisp)
;;; elisp.el ends here
