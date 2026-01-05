;;; settings.el --- General settings for Emacs

;; Author: Ricky Nelson <rickyn@socketwiz.com>

;;; Commentary:
;; Settings not required by any particular package

;;; Code:
(setq debug-on-error t)

;; Prevent the startup window
(setq inhibit-startup-message t)

;; Keep backups
(setq make-backup-files config-keep-backups)

(dolist (dir '("~/.emacs.d/backups" "~/.emacs.d/undo"))
  (make-directory dir t))
;; Save ALL backup files to this location
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
;; Undo history
(setq undo-tree-history-directory-alist `(("." . "~/.emacs.d/undo")))

;; Disable re-center of the cursor to the middle of page when scroll hits top or bottom of the page
(setq scroll-conservatively 101)

;; Give focus to new help windows
(setq help-window-select t)

;; Hide ui elements
(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1)

;; y or n is enough for me
(fset 'yes-or-no-p 'y-or-n-p)

;; Always highlight current line
(global-hl-line-mode t)

;; Turn on line numbers
(global-display-line-numbers-mode)

;; Turn on column number
(setq column-number-mode t)
;; Make line numbers relative
(setq display-line-numbers-type 'relative)

;; Put these documents in current buffer so they can be read and exited with minimum effort
(add-to-list 'display-buffer-alist '("*Apropos*" display-buffer-same-window))
(add-to-list 'display-buffer-alist '("*Buffer List*" display-buffer-same-window))
(add-to-list 'display-buffer-alist '("*Help*" display-buffer-same-window))
(add-to-list 'display-buffer-alist '("*Info*" display-buffer-same-window))

;; Treesitter grammars
;; M-x treesit-install-language-grammar
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
	(cmake "https://github.com/uyha/tree-sitter-cmake")
	(css "https://github.com/tree-sitter/tree-sitter-css")
	(elisp "https://github.com/Wilfred/tree-sitter-elisp")
	(html "https://github.com/tree-sitter/tree-sitter-html")
	(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
	(json "https://github.com/tree-sitter/tree-sitter-json")
	(make "https://github.com/alemuller/tree-sitter-make")
	(markdown "https://github.com/ikatyang/tree-sitter-markdown")
	(python "https://github.com/tree-sitter/tree-sitter-python")
	(toml "https://github.com/tree-sitter/tree-sitter-toml")
	(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
	(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
	(yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; Font
(set-face-attribute 'default nil
                    :family config-font-family
                    :height config-font-height)

;; On mac, make the cmd key the meta key in emacs
;; Conditional configuration for macOS
(cond
 ((eq system-type 'darwin)
  ;; Remap keys on macOS
  (setq mac-command-modifier 'meta)  ;; Command key -> Meta
  (setq mac-option-modifier 'super)  ;; Option key -> Super
  (setq mac-control-modifier 'control)  ;; Control key -> Control
  (setq ns-function-modifier 'hyper)  ;; Function key -> Hyper

  ;; Get ripgrep to work on macOS
  (setq shell-file-name "/bin/bash")
  (setq explicit-shell-file-name "/bin/bash"))
 (t
  ;; Settings for other systems
  (setq shell-file-name "/usr/bin/bash")
  (setq explicit-shell-file-name "/usr/bin/bash")))


;; Enable narrow to region functionality
(put 'narrow-to-region 'disabled nil)

;; Frequently accessed files (C-x r j <letter>)
;; jump-to-register
(set-register ?b '(file . "~/Documents/org/blog/index.org"))
(set-register ?i '(file . "~/Documents/org/agenda/inbox.org"))
(set-register ?k '(file . "~/Documents/org/work.org"))
(set-register ?n '(file . "~/Documents/org/notes.org"))
(set-register ?s '(file . "~/.emacs.d/init.el"))
(set-register ?w '(file . "~/Documents/org/wiki/index.org"))

;; * Keybindings that replace defaults (except F5)
(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "C-h f") 'helpful-callable)
(global-set-key (kbd "C-h k") 'helpful-key)
(global-set-key (kbd "C-h v") 'helpful-variable)
(global-set-key (kbd "C-x u") 'undo-tree-visualize)
(global-set-key (kbd "C-x C-e") 'pp-eval-last-sexp)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
(global-set-key (kbd "C-x C-z") 'nil)

(when (not (featurep 'evil-mode))
  ;; In evil-mode <C-z> swaps between normal and emacs states
  ;; In terminal <C-z> suspends Emacs to the background, but doesn't work so well in GUI
  (if (not (string= window-system nil))
      (global-unset-key (kbd "C-z"))))

(use-package replace
  :ensure nil ;; don't try to install, just configure
  :defer t
  :init
  ;; Hooks and helpers for occur behavior
  (defun my/occur-focus-on-results ()
    "Focus occur results in other window."
    (switch-to-buffer-other-window "*Occur*"))

  (defun my/after-occur-goto-occurrence (&rest _)
    "After jumping to an occurrence, close the *Occur* buffer."
    (delete-windows-on "*Occur*"))

  :config
  ;; Focus occur results immediately
  (add-hook 'occur-hook #'my/occur-focus-on-results)
  
  ;; After jumping to a result, close *Occur*
  (advice-add 'occur-mode-goto-occurrence :after #'my/after-occur-goto-occurrence))

;; convert major modes to use the new treesitter tech
(setq major-mode-remap-alist
      '((bash-mode . bash-ts-mode)
        (rust-mode . rust-ts-mode)
        (js-mode . js-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (python-mode . python-ts-mode)
        (yaml-mode . yaml-ts-mode)))

(provide 'settings)
;;; settings.el ends here
