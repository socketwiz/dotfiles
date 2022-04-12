
(defun my/init-projectile ()
  (setq projectile-cache-file (concat user-emacs-directory ".cache/projectile.cache")
        projectile-known-projects-file (concat user-emacs-directory
                                               ".cache/projectile-bookmarks.eld"))
  (add-hook 'find-file-hook (lambda () 
                              (unless recentf-mode (recentf-mode) 
                                      (recentf-track-opened-file)))) 

  (setq recentf-save-file (concat user-emacs-directory ".cache/recentf")
        recentf-max-saved-items 1000
        recentf-auto-cleanup 'never
        recentf-auto-save-timer (run-with-idle-timer 600 t 'recentf-save-list))

  ;; (add-to-list 'recentf-exclude (expand-file-name package-user-dir))
  ;; (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
  ;; (add-to-list 'recentf-exclude (expand-file-name (concat user-emacs-directory ".cache/")))

  (use-package projectile 
    :bind (("C-c p *" . projectile-pt))
    :diminish 'projectile-mode) 

  (use-package counsel-projectile)

  (setq projectile-completion-system 'ivy
        projectile-sort-order 'recentf
        projectile-globally-ignored-directories (append '(".cache")))
  (projectile-global-mode))

(provide 'my-projectile)
