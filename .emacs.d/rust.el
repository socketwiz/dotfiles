;;; init.el --- Language Rust

;; Author: Ricky Nelson <rickyn@socketwiz.com>

;;; Commentary:
;; Settings to make Rust development easier

;;; Code:
;; Debug with rust-llb (installed with cargo)
;; rust-lldb target/debug/deps/BIN-HASH -- arg1 arg2 ...
;;
;; Help
;; help <command>
;;
;; Breakpoints
;; breakpoint set --name main
;; breakpoint set --file main.rs --line 20
;;
;; Execution commands
;; run      // launch process
;; step     // single step currently selected thread
;; next     // step over
;; finish   // step out
;; continue // run to next breakpoint
;; quit     // end session
;;
;; Examine program state
;; frame variable      // show the arguments and local variables for the current frame
;; frame variable bar  // show contents of local variable "bar"
;; target variable bar // show global variable "bar"
;;
;; To enable the eglot backend:
;; rustup component add rls rust-analysis rust-src
;;
(defun setup-rust ()
  "Custom Rust setup for rust-ts-mode."
  (when (and (bound-and-true-p evil-mode))
    (define-key evil-normal-state-map (kbd "M-.") 'xref-find-definitions))
  (yas-minor-mode)
  (smartparens-mode)
  (prettify-symbols-mode))

(setq-default eglot-workspace-configuration
              '((:rust-analyzer . (:rustfmt (:enableRangeFormatting t)
                                            :checkOnSave (:command "clippy")))))

(add-hook 'rust-ts-mode-hook #'setup-rust)
(add-hook 'rust-ts-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'eglot-format-buffer -10 t)))

(provide 'rust)
;;; rust.el ends here
