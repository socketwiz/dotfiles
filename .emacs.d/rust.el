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
  "Do these things after \"rust-mode\" is enabled."
  ;; reset eglot-stay-out-of in case we've run a javascript file which adds flymake
  (setq eglot-stay-out-of '())
  (eglot-ensure)
  ;;(add-to-list 'eglot-server-programs '(rust-mode . "rust-analyzer"))
  (when (and (bound-and-true-p evil-mode))
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
               ("C-c C-r" . rust-run)
               ("C-c C-t" . rust-test))))

(provide 'rust)
;;; rust.el ends here
