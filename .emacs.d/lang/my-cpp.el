
(defun my/init-cpp ()
  (use-package platformio-mode
    :ensure t
    :commands (platformio-conditionally-enable)
    :mode (("\\.ino\\'" . c++-mode))
    :init)

  (defun platformio-hook ()
    (platformio-conditionally-enable))

  (add-hook 'c-mode-hook 'platformio-hook)
  (add-hook 'c++-mode-hook 'platformio-hook))

(provide 'my-cpp)
