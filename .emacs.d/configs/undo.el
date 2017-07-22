
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  :init
  (setq undo-tree-auto-save-history t)
  ;; Save ALL undo histories to this location
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))
