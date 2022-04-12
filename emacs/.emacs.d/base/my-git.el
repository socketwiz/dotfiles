
(defun my/init-git ()
  (use-package magit
    :config
    (setq magit-refresh-status-buffer nil)
    :diminish 'auto-revert-mode
    :defer t)

  ;; Show diffs in the gutter
  (use-package diff-hl
    :ensure t
    :config
    (progn
      (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
      (global-diff-hl-mode t)
      (diff-hl-flydiff-mode t))))

(provide 'my-git)
