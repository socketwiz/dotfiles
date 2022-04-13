;;; init.el --- Language Rust

;; Author: Ricky Nelson <rickyn@socketwiz.com>

;;; Commentary:
;; Settings to make Rust development easier

;;; Code:
(defun setup-rust ()
  "Do these things after \"rust-mode\" is enabled."
  ;; To enable the eglot backend:
  ;; rustup component add rls rust-analysis rust-src
  (eglot-ensure)
  (when (and (bound-and-true-p evil-mode) (eq evil-state 'normal))
    ;; Setup find-definitions when in rust-mode
    (define-key evil-normal-state-map (kbd "M-.") 'xref-find-definitions))
  (yas-minor-mode)
  (smartparens-mode))

;; Syntax highlighting, indentation, etc..
(use-package rust-mode
  :if config-enable-rust-mode
  :hook (rust-mode . setup-rust)
  :mode ("\\.rs\\'" . rust-mode)
  :bind (:map rust-mode-map
              (("C-c C-c" . rust-compile)
               ("C-c C-k" . rust-check)
               ("C-c C-r" . rust-run))))

(provide 'rust)
