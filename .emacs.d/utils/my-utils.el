
(defun my/init-utils ()
  (my/init-google-this))

(defun my/init-google-this ()
  (use-package google-this
    :config (google-this-mode 1)))

(provide 'my-utils)
