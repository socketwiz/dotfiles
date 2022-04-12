
(defun my/init-ido ()
  (use-package ido
    :init
    (ido-mode 1)
    :defer t)

  (use-package ido-completing-read+
    :init
    (ido-ubiquitous-mode 1)
    :config
    (setq ido-cr+-max-items 3000)
    :defer t)

  (use-package flx-ido
    :init
    (flx-ido-mode 1)
    :defer t)

  (use-package recentf
    :config
    (progn
      (setq recentf-max-saved-items 10
            recentf-max-menu-items 5
            recentf-save-file (concat user-emacs-directory ".cache/recentf")
            recentf-auto-cleanup 'never)
      (recentf-mode 1)

      (add-to-list 'recentf-exclude (expand-file-name package-user-dir))
      (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
      (add-to-list 'recentf-exclude (expand-file-name (concat user-emacs-directory ".cache/")))
      (add-hook 'delete-terminal-functions (lambda (terminal) (recentf-save-list))))))

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(provide 'my-ido)
