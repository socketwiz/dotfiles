
(use-package linum-relative
  :ensure t
  :config
  ;; display line numbers for all files
  (add-hook 'find-file-hook 'linum-relative-mode)
  :init
  ;; display real line number instead of 0
  (setq linum-relative-current-symbol ""))
