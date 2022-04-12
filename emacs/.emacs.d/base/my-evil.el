
(defun my/init-evil-base () 
  (my/init-evil-escape) 
  (use-package evil
    :init
    ;; don't let modes override any states (!)
    (setq evil-overriding-maps nil
          evil-intercept-maps nil
          evil-pending-intercept-maps nil
          evil-pending-overriding-maps nil)
    :config
    (progn
      (evil-mode 1)

      (use-package evil-surround
        :config (global-evil-surround-mode 1))

      ;; evil-anzu for improving search result rendering
      (use-package evil-anzu
        :config (global-anzu-mode +1)
        :diminish 'anzu-mode)

      ;; set cursor color according to mode
      (setq evil-normal-state-cursor '("DarkGoldenrod2" box))
      (setq evil-insert-state-cursor '("chartreuse3"  box))
      (setq evil-visual-state-cursor '("gray" box))
      (setq evil-operator-state-cursor '("cyan" box))
      (setq evil-replace-state-cursor '("chocolate" box))
      (setq evil-motion-state-cursor '("plum3" box))
      (setq evil-emacs-state-cursor  '("SkyBlue2" box))

      ;; disable this key sequence so we can use it in ivy
      (define-key evil-normal-state-map (kbd "C-n") nil)
      ;; disable this key sequence so we can use it in tide
      (define-key evil-normal-state-map (kbd "M-.") nil)

      ;; disable evil for these modes
      (add-to-list 'evil-emacs-state-modes 'dired-mode)
      (add-to-list 'evil-emacs-state-modes 'flycheck-error-list-mode)
      (add-to-list 'evil-emacs-state-modes 'help-mode)
      (add-to-list 'evil-emacs-state-modes 'Info-mode)
      (add-to-list 'evil-emacs-state-modes 'magit-status-mode)
      (add-to-list 'evil-emacs-state-modes 'pt-search-mode)

      ;; subvert evil-operation.el overrides (dired, ibuffer etc.)
      (advice-add 'evil-make-overriding-map :override #'ignore)
      (advice-add 'evil-make-intercept-map  :override #'ignore)
      (advice-add 'evil-add-hjkl-bindings   :override #'ignore))))

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
