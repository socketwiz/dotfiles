;;; elisp.el --- Emacs Lisp language settings -*- lexical-binding: t; -*-

;; Author: Ricky Nelson <rickyn@socketwiz.com>

;;; Commentary:
;; Settings for working with Emacs Lisp.

;;; Code:

;; Paredit configuration
(use-package paredit
  :hook (emacs-lisp-mode . paredit-mode))

;; Smartparens configuration
(use-package smartparens
  :hook (emacs-lisp-mode . smartparens-mode))

;; Automatically indent elisp code on save
(defun my-indent-buffer ()
  "Indent the entire buffer."
  (indent-region (point-min) (point-max)))

(defun my-setup-emacs-lisp-mode ()
  "Custom configurations for Emacs Lisp mode."
  (prettify-symbols-mode 1)
  (add-hook 'before-save-hook #'my-indent-buffer nil t))

;; Set up Emacs Lisp environment
(add-hook 'emacs-lisp-mode-hook #'my-setup-emacs-lisp-mode)

(provide 'elisp)
;;; elisp.el ends here
