
(defun my/init-ido ()
  (use-package ido
    :init
    (ido-mode 1)
    :defer t)

  (use-package ido-completing-read+
    :init
    (ido-ubiquitous-mode 1)
    :defer t)

  (use-package flx-ido
    :init
    (flx-ido-mode 1)
    (progn (setq ido-enable-flex-matching t))
    :defer t)

  (use-package recentf
    :config
    (setq recentf-max-saved-items 50
          recentf-max-menu-items 15)
    (recentf-mode 1)))

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(provide 'my-ido)
