
(defun my/init-evil-base () 
  (my/init-evil-escape) 
  (use-package evil
    :config
    (evil-mode 1)

    ;; disable this key sequence so we can use it in ivy
    (define-key evil-normal-state-map (kbd "C-n") nil))
  (use-package evil-surround 
    :config (global-evil-surround-mode 1)))

(defun my/init-evil-escape () 
  (use-package evil-escape 
    :diminish 'evil-escape-mode) 
  (setq-default evil-escape-key-sequence "fd") 
  (evil-escape-mode)
  ;; esc should escape everything possible
  (require 'evil) 
  (define-key evil-normal-state-map [escape] 'keyboard-quit) 
  (define-key evil-visual-state-map [escape] 'keyboard-quit) 
  (define-key minibuffer-local-map [escape] 'abort-recursive-edit) 
  (define-key minibuffer-local-ns-map [escape] 'abort-recursive-edit) 
  (define-key minibuffer-local-completion-map [escape] 'abort-recursive-edit) 
  (define-key minibuffer-local-must-match-map [escape] 'abort-recursive-edit) 
  (define-key minibuffer-local-isearch-map [escape] 'abort-recursive-edit))

(provide 'my-evil)
