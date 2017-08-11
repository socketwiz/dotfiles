
(use-package irony
  :ensure t
  :commands (irony-mode))

(use-package company-irony
  :ensure t)

(use-package flycheck-irony
  :ensure t
  :init (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(use-package irony-eldoc
  :ensure t
  :commands (irony-eldoc))

(use-package platformio-mode
  :ensure t
  :commands (platformio-conditionally-enable)
  :mode (("\\.ino\\'" . c++-mode))
  :init
  (add-to-list 'company-backends 'company-irony)

  ;; Setup irony for flycheck.
  (add-hook 'flycheck-mode-hook 'flycheck-irony-setup))

(defun irony-and-platformio-hook ()
  (irony-mode)
  (irony-eldoc)
  (platformio-conditionally-enable))

(add-hook 'c-mode-hook 'irony-and-platformio-hook)
(add-hook 'c++-mode-hook 'irony-and-platformio-hook)
