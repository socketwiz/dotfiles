
(use-package undo-tree
  :ensure t
  :config
  (progn
    (global-undo-tree-mode 1)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)
    (setq undo-tree-auto-save-history t)
    ;; save all undo histories to this location
    (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
    (defadvice undo-tree-make-history-save-file-name
        (after undo-tree activate)
      (setq ad-return-value (concat ad-return-value ".gz")))))
