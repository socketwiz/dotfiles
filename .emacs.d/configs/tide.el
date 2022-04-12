
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1))

(use-package tide
  :ensure t
  :defer 1
  :bind (("M-?" . tide-documentation-at-point)
         ("M-/" . tide-jump-to-definition))
  :config
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-match-p "js[x]?" (file-name-extension buffer-file-name))
                (setup-tide-mode)))))
