
(defun my/init-ivy ()
  (use-package hydra)

  (use-package ivy
    :diminish ivy-mode
    :config
    (setq ivy-re-builders-alist
          '((t . ivy--regex-fuzzy)))
    :bind
    (("C-c k" . counsel-pt))  ; search for regexp in git repo using pt
    :init
    (ivy-mode)))

(provide 'my-ivy)
