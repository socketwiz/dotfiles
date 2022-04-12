
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

;; Show diffs in the gutter
(use-package diff-hl
  :ensure t
  :config
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (set-face-attribute 'fringe nil :background "#000000")
  :init
  (global-diff-hl-mode t)
  (diff-hl-flydiff-mode t))
