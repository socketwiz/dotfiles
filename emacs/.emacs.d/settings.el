;;; settings.el --- Emacs settings

;; Author: Ricky Nelson <rickyn@socketwiz.com>

;;; Commentary:
;; This package provides customized personal settings

;;; Code:

;; * Core
(package-initialize)

(defvar config-which-key-delay 0.4
  "How long before which-key displays.")

(defvar config-keep-backups t
  "Whether or not to make a backup of a file when it is saved.")

(defvar config-font-family "Fira Code"
  "Font to use in each buffer.")
(defvar config-font-height 180
  "The font-height is 1/10pt so 160 == 160/10 == 16pt.")

(defvar config-indent-web-mode-spaces 2
  "How many spaces to indent in web-mode.")
(defvar js-switch-indent-offset 2
  "How many spaces to indent in \"js-mode\".")

(defvar config-enable-evil-mode nil
  "Whether or not to enable evil-mode.")
(defvar config-enable-elpy-mode t
  "Whether or not to enable elpy-mode.")
(defvar config-enable-cider-mode nil
  "Whether or not to enable cider-mode.")
(defvar config-enable-rustic-mode t
  "Whether or not to enable rustic-mode.")
(defvar config-enable-markdown-mode nil
  "Whether or not to enable markdown-mode.")
(defvar config-enable-web-mode t
  "Whether or not to enable web, js, css.")
(defvar config-enable-c-mode nil
  "Whether or not to enable c, c++.")
(defvar config-enable-undo-tree t
  "Whether or not to enable undo-tree.")
(defvar config-enable-command-log-mode nil
  "Whether or not to enable command-log-mode.")

;; Hide column numbers
(setq column-number-mode t)

;; Draw underline lower
(setq x-underline-at-descent-line t)

;; Prevent the startup window
(setq inhibit-startup-message t)

;; Run the debugger on an error
(setq debug-on-error t)

;; Keep backups
(setq make-backup-files config-keep-backups)

;; Save ALL backup files to this location
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))

;; Disable re-center of the cursor to the middle of page when scroll hits top or bottom of the page
(setq scroll-conservatively 101)

;; Give focus to new help windows
(setq help-window-select t)

;; Add /usr/local/bin to the path
(setq exec-path (append exec-path '("/usr/local/bin")))

;; Hide ui elements
(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1)

;; y or n is enough for me
(fset 'yes-or-no-p 'y-or-n-p)

;; Parenthesis
(show-paren-mode 1)

;; Automatic pairing (, [, ", etc...
(electric-pair-mode 1)

;; Always highlight current line
(global-hl-line-mode t)

;; Turn on line numbers
(global-display-line-numbers-mode)
;; Make line numbers relative
(menu-bar-display-line-numbers-mode 'relative)

;; Put these documents in current buffer so they can be read and exited with minimum effort
(add-to-list 'display-buffer-alist
             '("*Apropos*" display-buffer-same-window)
             '("*Info*" display-buffer-same-window))

;; Package management
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Font
(set-face-attribute 'default nil
                    :family config-font-family
                    :height config-font-height)
;; Font ligatures
(load "~/.emacs.d/fira-code.el")
(declare-function fira-code-mode--setup "~/.emacs.d/fira-code.el")
(declare-function fira-code-mode--enable "~/.emacs.d/fira-code.el")
(fira-code-mode--setup)
(fira-code-mode--enable)

;; Enable narrow to region functionality
(put 'narrow-to-region 'disabled nil)

;; Frequently accessed files (C-x r j <letter>)
;; jump-to-register
(set-register ?i '(file . "~/.emacs.d/settings.el"))
(set-register ?o '(file . "~/org/agenda/organizer.org"))
(set-register ?w '(file . "~/org/wiki/index.org"))

;; When on MacOS, change meta to cmd key
(when (eq system-type 'darwin)
  ;; These 2 lines do not trigger flycheck warnings when on MacOS
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil))


;; * Core keybindings
(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c C-.") 'helpful-at-point)
(global-set-key (kbd "C-c r") 'counsel-recentf)
(global-set-key (kbd "C-h f") 'helpful-callable)
(global-set-key (kbd "C-h k") 'helpful-key)
(global-set-key (kbd "C-h v") 'helpful-variable)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-x d") 'counsel-dired)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-e") 'pp-eval-last-sexp)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch)
(global-set-key (kbd "M-i") 'counsel-imenu)
(global-set-key (kbd "M-s s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)


;; * Core packages
(use-package diminish)

;; Construct a regex interactively
(use-package re-builder
  :config
  ;; Set regex syntax to string for re-builder
  (setq reb-re-syntax 'string))

;; Flymake mode - syntax checking
(use-package flymake
  :config
  ;; Disable flymake-mode
  (setq flymake-start-on-flymake-mode nil))


(defun compilation-filter-init ()
  "Initialize \"compilation-mode\" settings."
  ;; Colorize compilation-mode
  (declare-function ansi-color-apply-on-region "ansi-color.el.gz")
  (when (eq major-mode 'compilation-mode)
    (ansi-color-apply-on-region compilation-filter-start (point-max))))

;; Compilation mode - compilation of log buffers
(use-package compile
  :config
  ;; Automatically scroll the compilation buffer
  (setq compilation-scroll-output t)
  :hook (compilation-filter . compilation-filter-init))

;; Rainbow mode - displays color codes in their color
(use-package rainbow-mode
  :delight)

;; Theme
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-acario-dark t)
  ;; This theme makes the selections too dark, lighten them up
  (set-face-background 'hl-line "#1F2324")
  (set-face-background 'region "#585F61")

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; This required some fonts to be downloaded, run `all-the-icons-install-fonts` manually
;; https://github.com/emacs-jp/replace-colorthemes
(use-package all-the-icons)
(use-package all-the-icons-dired
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;; Parenthesis
(use-package highlight-parentheses
  :diminish 'highlight-parentheses-mode
  :config
  (add-hook 'prog-mode-hook 'highlight-parentheses-mode))

;; Undo-tree
(use-package undo-tree
  :if config-enable-undo-tree
  :diminish 'undo-tree-mode
  :config
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-auto-save-history t)
  ;; save all undo histories to this location
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  (defadvice undo-tree-make-history-save-file-name
      (after undo-tree activate)
    (setq ad-return-value (concat ad-return-value ".gz")))
  (global-undo-tree-mode))

;; A text completion framework
(use-package company
  :diminish 'company-mode
  :config
  (setq company-tooltip-align-annotations t)
  :init (global-company-mode))

;; Show the argument list of a function in the echo area
(use-package eldoc
  :diminish 'eldoc-mode
  :commands turn-on-eldoc-mode)

;; Spell checker
(use-package flyspell
  :diminish 'flyspell-mode
  :config
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))
;; Correct the misspelled word in a popup menu
(use-package flyspell-popup
  :bind (:map flyspell-mode-map ("C-;" . flyspell-popup-correct)))

;; Syntax checker
(use-package flycheck
  :diminish 'flycheck-mode
  :init (global-flycheck-mode))

;; Snippets, a template system for emacs
(use-package yasnippet
  :bind (("TAB" . yas-expand))
  :config
  (declare-function yas-reload-all "yasnippet.el")
  (yas-reload-all))

;; Language Server Protocol support for Emacs
(use-package lsp-mode)

;; Display available keybindings in a popup
(use-package which-key
  :diminish 'which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay config-which-key-delay))

;; Highlight numbers for prog modes
(use-package highlight-numbers
  :init
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

;; Org mode, for keeping notes, todo lists, etc... in plain text
(use-package org
  :config
  (setq org-todo-keywords '("TODO" "STARTED" "WAITING" "DONE"))
  (defvar org-agenda-include-diary t)
  (defvar org-src-fontify-natively t)
  (setq org-agenda-files (directory-files-recursively "~/org/agenda" "org$"))
  (setq org-default-notes-file "~/org/agenda/organizer.org")

  ;; So we can execute these language blocks in org-mode
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)))
  (defvar python-shell-completion-native-disabled-interpreters "python3"))

;; Convert buffer to text and decorations to HTML
(use-package htmlize
  :mode (("\\.org\\'" . org-mode)))

;; Respect editor configs
(use-package editorconfig
  :diminish 'editorconfig-mode
  :config
  (editorconfig-mode 1))

;; Syntax highlighting for docker files
(use-package dockerfile-mode
  :defer t)

;; A better "help" buffer
(use-package helpful)

;; Edit text area in chrome with emacs
(use-package atomic-chrome
  :config
  (atomic-chrome-start-server)
  (setq atomic-chrome-buffer-open-style 'frame))

;; Builds a list of recently opened files
(use-package recentf
  :config
  (setq recentf-max-saved-items 10
        recentf-max-menu-items 5
        recentf-save-file (concat user-emacs-directory ".cache/recentf")
        recentf-auto-cleanup 'never)
  (recentf-mode 1)

  (add-to-list 'recentf-exclude (expand-file-name package-user-dir))
  (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
  (add-to-list 'recentf-exclude (expand-file-name (concat user-emacs-directory ".cache/")))
  (declare-function recentf-save-list "recentf.el.gz")
  (add-hook 'delete-terminal-functions (lambda (terminal) (recentf-save-list))))

;; Display used hotkeys in another window
(use-package command-log-mode
  :if config-enable-command-log-mode
  :diminish 'command-log-mode
  :hook (after-init . global-command-log-mode))

;; Minor mode for dealing with pairs, such as quotes
(use-package smartparens-config
  :ensure smartparens
  :config
  (show-smartparens-global-mode t))

;; Expand selected region by semantic units
(use-package expand-region
  :config
  (pending-delete-mode t))

;; Modeline theme, bottom of each window
(use-package doom-modeline
  ;; (setq doom-modeline-icon (display-graphic-p))
  :hook (after-init . doom-modeline-mode))

;; * git
;; A git interface for emacs
(use-package magit
  :config
  (setq magit-refresh-status-buffer nil)
  :diminish 'auto-revert-mode)

;; Show diffs in the gutter
(use-package diff-hl
  :config
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (global-diff-hl-mode t)
  (diff-hl-flydiff-mode t))

;; * ivy
;; Generic completion frontend
(use-package counsel)
(use-package counsel-projectile
  :config
  (counsel-projectile-mode t)
  :bind (:map projectile-mode-map ("C-c p" . 'projectile-command-map)))

;; Fuzzy matching for Emacs
(use-package flx)
;; Sorting and filtering
(use-package prescient)
(use-package ivy-prescient
  :after (ivy prescient)
  :config (ivy-prescient-mode))
(use-package ivy-hydra)
(use-package ivy
  :diminish 'ivy-mode
  :config
  (ivy-mode t)
  ;; make everything fuzzy except swiper
  (setq ivy-re-builders-alist
        '((counsel-M-x . ivy--regex-fuzzy)
          ;; To create a new file when a similar name is being fuzzy
          ;; matched <C-M j> instead of RET (ivy-immediate-done)
          ;; to temporarily turn off fuzzy matching
          (t . ivy--regex-plus))))


;; * projectile
;; ripgrep
(use-package rg)

;; A project interaction library
(use-package projectile
  :after (rg)
  :config
  (setq projectile-project-search-path '("~/dev"))
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (projectile-mode)
  :init
  (setq projectile-cache-file (concat user-emacs-directory ".cache/projectile.cache")
        projectile-known-projects-file (concat user-emacs-directory
                                               ".cache/projectile-bookmarks.eld"))
  (declare-function recentf-track-opened-file "recentf.el.gz")
  (add-hook 'find-file-hook (lambda ()
                              (unless recentf-mode (recentf-mode)
                                      (recentf-track-opened-file))))
  :bind
  (:map projectile-mode-map ("C-c p s p" . rg-project))
  :diminish 'projectile-mode)

;; Extensible vi layer for Emacs
(use-package evil
  :if config-enable-evil-mode
  :config
  ;; Put vim bindings everywhere
  (evil-mode)
  ;; Except in these modes where I just want emacs proper
  (evil-set-initial-state 'debugger-mode 'emacs)
  (evil-set-initial-state 'diff-mode 'emacs)
  (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'emacs-lisp-mode 'emacs)
  (evil-set-initial-state 'fundamental-mode 'emacs)
  (evil-set-initial-state 'helpful-mode 'emacs)
  (evil-set-initial-state 'help-mode 'emacs)
  (evil-set-initial-state 'Info-mode 'emacs)
  (evil-set-initial-state 'org-mode 'emacs)
  (evil-set-initial-state 'rustic-popup-mode 'emacs)
  (evil-set-initial-state 'rustic-cargo-outdated-mode 'emacs)
  (evil-set-initial-state 'markdown-view-mode 'emacs)
  ;; For some reason python mode is starting in emacs state, set it to normal
  (declare-function evil-set-initial-state "evil-core.el")
  (evil-set-initial-state 'python-mode 'normal))

;; Surround text objects with characters
(use-package evil-surround
  :if config-enable-evil-mode
  :after (evil)
  :config
  (global-evil-surround-mode 1))

(when (not (featurep 'evil-mode))
  ;; In evil-mode <C-z> swaps between normal and emacs states
  ;; In terminal <C-z> suspends Emacs to the background, but doesn't work so well in GUI
  (if (not (string= window-system nil))
      (global-unset-key (kbd "C-z"))))

;; For macOS import PATH when launched as GUI
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))


;; * Language cpp
;; A flycheck checker for C/C++
(use-package flycheck-irony
  :if config-enable-c-mode
  :after (irony)
  :defer t)

;; Irony support for C/C++
(use-package irony-eldoc
  :if config-enable-c-mode
  :after (irony)
  :defer t
  :init
  (add-hook 'irony-mode-hook 'irony-eldoc))

;; C++ minor mode, completion, syntax checking
(use-package irony
  :if config-enable-c-mode
  ;; Need to install the server on first run (M-x irony-install-server)
  :commands irony-mode
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (defun my-irony-mode-hook ()
    (setq irony-additional-clang-options '("-std=c++14")))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

;; Embedded platform development
(use-package platformio-mode
  :if config-enable-c-mode
  :commands (platformio-conditionally-enable)
  :mode (("\\.ino\\'" . c++-mode))
  :init
  (defun platformio-hook ()
    (platformio-conditionally-enable))

  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

  (add-hook 'c++-mode-hook 'platformio-hook)
  (add-hook 'irony-mode-hook
            (lambda ()
              (irony-cdb-autosetup-compile-options)))
  (add-hook 'c++-mode-hook 'flycheck-mode))

;; * Language elisp
;; Minor mode for performing structured editing of S-expression data
(use-package paredit
  :init
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
  (add-hook 'ielm-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
  (add-hook 'scheme-mode-hook 'enable-paredit-mode)
  :config
  (eldoc-add-command
   'paredit-backward-delete
   'paredit-close-round))


;; * Language javascript
(defun configure-web-mode-flycheck-checkers ()
  "Configure flycheck for web JavaScript and TypeScript."
  (flycheck-mode)

  ;; See if there is a node_modules directory
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (or (and root
                          ;; Try the locally installed eslint
                          (expand-file-name "node_modules/eslint/bin/eslint.js" root))

                     ;; Try the global installed eslint
                     (concat (string-trim (shell-command-to-string "npm config get prefix")) "/bin/eslint"))))

    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint)))

  (declare-function flycheck-select-checker "flycheck.el")
  (if (boundp 'eslint)
      (flycheck-select-checker 'javascript-eslint)))

(defun setup-javascript ()
  "When js2-mode is loaded setup linters, yas and such."
  (tide-setup)
  (configure-web-mode-flycheck-checkers)
  (yas-minor-mode)
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1))

(defun setup-typescript ()
  "Setup jump-to-definition when in tide-mode."
  (when (featurep 'evil-mode)
    (define-key evil-normal-state-map (kbd "M-.") 'tide-jump-to-definition))
  (setup-javascript))

;; JavaScript editing mode
(use-package js2-mode
  :if config-enable-web-mode
  :hook (js2-mode . setup-javascript))

;; TypeScript Interactive Development Environment
(use-package tide
  :if config-enable-web-mode
  :hook (typescript-mode . setup-typescript))

;; JSX editing mode
(use-package rjsx-mode
  :if config-enable-web-mode
  :mode ("\\.js\\'" . rjsx-mode)
  :config
  :bind (:map rjsx-mode-map ("<" . nil)))


;; * Language HTML, css
(defun web-mode-init ()
  "Setup yas when in web-mode."
  (interactive)
  (yas-minor-mode)
  ;; disable auto-pairing just in web-mode so in django templates
  ;; you can do {% %} without it becoming {% %}}
  (electric-pair-mode -1))

;; Major mode for editing web templates
(use-package web-mode
  :if config-enable-web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.css\\'" . web-mode))
  :config
  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "js")
        (let ((web-mode-enable-part-face nil))
          ad-do-it)
      ad-do-it))

  ;; Disable lining up the args
  (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))
  :init
  (setq web-mode-engines-alist
        '(("django" . "\\.html\\'")))

  (setq-default indent-tabs-mode nil)
  ;; Disable auto-quoting
  (setq web-mode-enable-auto-quoting nil)
  (setq web-mode-markup-indent-offset config-indent-web-mode-spaces)
  (setq web-mode-css-indent-offset config-indent-web-mode-spaces)
  (setq web-mode-code-indent-offset config-indent-web-mode-spaces)
  ;; Don't lineup element attributes
  (setq web-mode-attr-indent-offset config-indent-web-mode-spaces)
  ;; Automatically close tag
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t)
  
  (add-hook 'web-mode-hook 'web-mode-init))

;; SASS
(use-package scss-mode
  :if config-enable-web-mode
  :mode ("\\.scss\\'" . scss-mode))


;; * Language Markdown
;; Major mode for editing Markdown formatted text
(use-package markdown-mode
  :if config-enable-markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))


;; * Language Rust
(defun setup-rustic ()
  "Setup find-definitions when in rustic-mode."
  (when (featurep 'evil-mode)
    (define-key evil-normal-state-map (kbd "M-.") 'xref-find-definitions))
  (yas-minor-mode))

(use-package rustic
  :if config-enable-rustic-mode
  :hook (rustic-mode . setup-rustic)
  :mode ("\\.rs\\'" . rustic-mode))

;; * Language Clojure
(use-package cider
  :if config-enable-cider-mode
  :hook (clojure-mode . enable-paredit-mode))


;; * Language Python
(use-package elpy
  :if config-enable-elpy-mode
  :init
  (elpy-enable))

(load "~/.emacs.d/dashboard.el")
(declare-function dashboard "~/.emacs.d/dashboard.el")
(dashboard)

(provide 'settings)

;;; settings.el ends here
