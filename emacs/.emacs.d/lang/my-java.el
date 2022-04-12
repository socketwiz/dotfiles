
(defun my/init-java ()
  (use-package groovy-mode
    :ensure t
    :mode "\\.groovy\\'"
    :interpreter "groovy"))

(provide 'my-java)
