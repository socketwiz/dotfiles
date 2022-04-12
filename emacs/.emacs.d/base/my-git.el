
(defun my/init-git ()
  (use-package magit
    :init
    (setq vc-handled-backends nil)
    :config
    (setq magit-refresh-status-buffer nil)
    :diminish 'auto-revert-mode
    :bind (("C-x g" . magit-status))
    :defer t)

  ;; Show diffs in the gutter
  (use-package diff-hl
    :ensure t
    :config
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
    (set-face-attribute 'fringe nil :background "#000000")
    :init
    (global-diff-hl-mode t)
    (diff-hl-flydiff-mode t)))

(provide 'my-git)
