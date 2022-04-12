
(defun my/init-projectile ()
  (setq projectile-cache-file (concat user-emacs-directory ".cache/projectile.cache")
        projectile-known-projects-file (concat user-emacs-directory
                                               ".cache/projectile-bookmarks.eld"))
  (add-hook 'find-file-hook (lambda () 
                              (unless recentf-mode (recentf-mode) 
                                      (recentf-track-opened-file)))) 

  (use-package pt
    :ensure t)

  (use-package projectile 
    :after (pt)
    :bind (("C-c p *" . projectile-pt))
    :diminish 'projectile-mode) 

  (setq projectile-sort-order 'recentf
        projectile-globally-ignored-directories (append '(".cache")))
  (projectile-global-mode))

(provide 'my-projectile)
