
(defun my/init-cpp ()
  (use-package flycheck-irony
    :ensure t)

  (use-package irony-eldoc
    :ensure t
    :init
    (add-hook 'irony-mode-hook #'irony-eldoc))

  (use-package irony
    :ensure t
    :commands irony-mode ; need to install the server on first run (M-x irony-install-server)
    :init
    (add-hook 'c++-mode-hook 'irony-mode)
    (add-hook 'c-mode-hook 'irony-mode)
    (defun my-irony-mode-hook ()
      (setq irony-additional-clang-options '("-std=c++14")))
    (add-hook 'irony-mode-hook 'my-irony-mode-hook)
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

  (use-package platformio-mode
    :ensure t
    :commands (platformio-conditionally-enable)
    :mode (("\\.ino\\'" . c++-mode))
    :init)

  (defun platformio-hook ()
    (platformio-conditionally-enable))

  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

  (add-hook 'c++-mode-hook 'platformio-hook)
  (add-hook 'irony-mode-hook
            (lambda ()
              (irony-cdb-autosetup-compile-options)))
  (add-hook 'c++-mode-hook 'flycheck-mode))

(provide 'my-cpp)
