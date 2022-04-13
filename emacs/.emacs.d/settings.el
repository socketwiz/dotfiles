
;; * Core
(package-initialize)

(defvar config-which-key-delay 0.4)

(defvar config-keep-backups t)

;; Font settings
(defvar config-font-family "Fira Code")
(defvar config-font-height 160
  "font-height is 1/10pt so 160 == 160/10 == 16pt")

(defvar config-indent-web-mode-spaces 2)

(setq use-package-always-ensure t)
(setq gc-cons-threshold most-positive-fixnum)

;; Set regex syntax to string for re-builder
(setq reb-re-syntax 'string)

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

;; Automatically scroll the compilation buffer
(setq compilation-scroll-output t)

;; Give focus to new help windows
(setq help-window-select t)

;; Add /usr/local/bin to the path
(setq exec-path (append exec-path '("/usr/local/bin")))

;; Disable flymake-mode
(setq flymake-start-on-flymake-mode nil)

;; Hide ui elements
(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1)

;; Y or n is enough for me
(fset 'yes-or-no-p 'y-or-n-p)

;; Parenthesis
(show-paren-mode 1)

;; Wrap selection with (, [, ", etc...
(electric-pair-mode 1)

;; Always hightlight current line
(global-hl-line-mode t)

;; Turn on line numbers
(global-display-line-numbers-mode)
(menu-bar-display-line-numbers-mode 'relative)

;; Put apropos in current buffer so it can be read and exited with minimum effort
(add-to-list 'display-buffer-alist
            '("*Apropos*" display-buffer-same-window)
            '("*Info*" display-buffer-same-window))

;; Package management
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Font
(set-face-attribute 'default nil
                    :family config-font-family
                    :height config-font-height)
;; Font ligatures
(load "~/.emacs.d/fira-code.el")
(fira-code-mode--setup)
(fira-code-mode--enable)

;; Enable narrow to region functionality
(put 'narrow-to-region 'disabled nil)

;; Frequently accessed files (C-x r j <letter>)
;; jump-to-register
(set-register ?i '(file . "~/.emacs.d/settings.el"))
(set-register ?o '(file . "~/org/agenda/organizer.org"))
(set-register ?w '(file . "~/org/wiki/index.org"))

;; Colorize compilation-mode
(defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
        (ansi-color-apply-on-region compilation-filter-start (point-max))))

(add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer)

;; When on OSX, change meta to cmd key
;; Amethyst, an OSX app I use is mucking around with Option-Shift
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil))


;; * Core keybindings
(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c C-.") 'helpful-at-point)
(global-set-key (kbd "C-c r") 'counsel-recentf)
(global-set-key (kbd "C-h b") 'describe-bindings)
(global-set-key (kbd "C-h f") 'helpful-callable)
(global-set-key (kbd "C-h k") 'helpful-key)
(global-set-key (kbd "C-h v") 'helpful-variable)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-e") 'pp-eval-last-sexp)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch)
(global-set-key (kbd "M-i") 'imenu)


;; * Core packages
(use-package diminish)

;; Rainbow mode - displays color codes in their color
(use-package rainbow-mode
  :delight)

;; Theme
(use-package cyberpunk-theme
  :config
  (load-theme 'cyberpunk t))

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
  (add-hook 'prog-mode-hook #'highlight-parentheses-mode))

;; Undo-tree
(use-package undo-tree 
  :config
  (setq undo-tree-visualizer-timestamps t) 
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-auto-save-history t)
  ;; save all undo histories to this location
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  (defadvice undo-tree-make-history-save-file-name
      (after undo-tree activate)
    (setq ad-return-value (concat ad-return-value ".gz")))
  (global-undo-tree-mode)
  :defer t 
  :diminish 'undo-tree-mode)

;; Company mode
(use-package company 
  :diminish 'company-mode
  :config
  (setq company-tooltip-align-annotations t)
  (add-hook 'after-init-hook 'global-company-mode))

;; Show the argument list of a function in the echo area
(use-package eldoc
  :diminish eldoc-mode
  :commands turn-on-eldoc-mode
  :defer t)

;; Flyspell
(use-package flyspell 
  :config
  (add-hook 'prog-mode-hook 'flyspell-prog-mode) 
  :diminish 'flyspell-mode) 
;; Correct the misspelled word in a popup menu
(use-package flyspell-popup 
  :config
  (define-key flyspell-mode-map (kbd "C-;") 'flyspell-popup-correct)
  (define-key popup-menu-keymap (kbd "C-j") 'popup-next)
  (define-key popup-menu-keymap (kbd "C-k") 'popup-previous)
  (define-key popup-menu-keymap (kbd "C-l") 'popup-select))

;; Flycheck
(use-package flycheck
  :diminish flycheck-mode
  :config
  (add-hook 'sh-mode-hook 'flycheck-mode))

;; Yasnippet, a template system for emacs
(use-package yasnippet
  :bind (("TAB" . yas-expand))
  :config
  (yas-reload-all))

;; Language Server Protocol support for Emacs
(use-package lsp-mode)

;; Display available keybindings in a popup
(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-idle-delay config-which-key-delay)
  :diminish which-key-mode)

;; Highlight numbers for prog modes
(use-package highlight-numbers 
  :defer t 
  :init
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

;; Org mode, for keeping notes, todo lists, etc... in plain text
(use-package org
  :config
  (setq org-todo-keywords '("TODO" "STARTED" "WAITING" "DONE"))
  (setq org-agenda-include-diary t)
  (setq org-src-fontify-natively t)
  (setq org-agenda-files (list "~/org/agenda"
                               "~/org/agenda/projects/"))
  (setq org-default-notes-file "~/org/agenda/organizer.org"))

;; Respect editor configs
(use-package editorconfig
  :diminish editorconfig-mode
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
  (add-hook 'delete-terminal-functions (lambda (terminal) (recentf-save-list))))

;; Display used hotkeys in another window
(use-package command-log-mode
  :diminish command-log-mode)

;; Minor mode for dealing with pairs, such as quotes
(use-package smartparens-config
  :ensure smartparens
  :config
  (show-smartparens-global-mode t))

;; Expand selected region by semantic units
(use-package expand-region
  :config
  (pending-delete-mode t))

;; Doom modeline
(use-package doom-modeline
  ;; (setq doom-modeline-icon (display-graphic-p))
  :ensure t
  :hook (after-init . doom-modeline-mode))

;; Convert buffer to text and decorations to HTML
(use-package htmlize
  :mode (("\\.org\\'" . org-mode)))

;; * git
;; A git interface for emacs
(use-package magit
  :config
  (setq magit-refresh-status-buffer nil)
  :diminish 'auto-revert-mode
  :defer t)

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

(use-package flx)
(use-package ivy-hydra)
(use-package ivy
  :diminish 'ivy-mode
  :config
  (ivy-mode t)
  ;; make everything fuzzy except swiper
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          ;; To create a new file when a similar name is being fuzzy
          ;; matched <M-r> (ivy-toggle-regexp-quote)
          ;; to temporarily turn off fuzzy matching
          (t . ivy--regex-fuzzy))))


;; * projectile
;; ripgrep
(use-package rg)

;; A project interaction library
(use-package projectile
  :after (rg)
  :config
  (setq projectile-project-search-path '("~/dev"))
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (projectile-global-mode)
  :init
  (setq projectile-cache-file (concat user-emacs-directory ".cache/projectile.cache")
        projectile-known-projects-file (concat user-emacs-directory
                                               ".cache/projectile-bookmarks.eld"))
  (add-hook 'find-file-hook (lambda ()
                              (unless recentf-mode (recentf-mode)
                                      (recentf-track-opened-file))))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :bind
  (:map projectile-mode-map ("C-c p s p" . rg-project))
  :diminish 'projectile-mode)


;; * Language cpp
;; A flycheck checker for C/C++
(use-package flycheck-irony
  :after (irony)
  :defer t)

;; Irony support for C/C++
(use-package irony-eldoc
  :after (irony)
  :defer t
  :init
  (add-hook 'irony-mode-hook 'irony-eldoc))

;; C++ minor mode, completion, syntax checking
(use-package irony
  :defer t
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
  :defer t
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

;; Extensible vi layer for emacs
(use-package evil
  :config
  ;; Put vim bindings everywhere
  (evil-mode)
  ;; Except in these modes where I just want emacs proper
  (evil-set-initial-state 'debugger-mode 'emacs)
  (evil-set-initial-state 'diff-mode 'emacs)
  (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'emacs-lisp-mode 'emacs)
  (evil-set-initial-state 'helpful-mode 'emacs)
  (evil-set-initial-state 'help-mode 'emacs)
  (evil-set-initial-state 'Info-mode 'emacs)
  (evil-set-initial-state 'org-mode 'emacs)
  :bind (:map evil-normal-state-map ("M-." . 'tide-jump-to-definition)))

;; Surround text objects with characters
(use-package evil-surround
  :after (evil)
  :config
  (global-evil-surround-mode 1))


;; For macOS import PATH when launched as GUI
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)))


;; * Language javascript
(defun configure-web-mode-flycheck-checkers ()
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

  (if (boundp 'eslint)
      (flycheck-select-checker 'javascript-eslint)))

(defun setup-javascript ()
  (tide-setup)
  (configure-web-mode-flycheck-checkers)
  (yas-minor-mode)
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1))

(defun setup-js2 ()
  (setq js-switch-indent-offset 2)
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (setup-javascript))

(defun setup-typescript ()
  (flycheck-add-mode 'javascript-eslint 'typescript-mode)
  (setup-javascript))

;; TypeScript Interactive Development Environment
(use-package tide
  :config
  :hook (typescript-mode . setup-typescript))

;; JavaScript editing mode
(use-package js2-mode
  :mode "\\.js\\'"
  :config
  :hook (js2-mode . setup-js2))

(use-package rjsx-mode)


;; * Language HTML, css
(defun setup-template ()
  (interactive)
  (yas-minor-mode))


;; Major mode for editing web templates
(use-package web-mode
  :defer t
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
  (setq web-mode-content-types-alist
        '(("js" . "\\.js\\'")))
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
  
  (add-hook 'web-mode-hook 'setup-template))

;; SASS
(use-package scss-mode
  :defer t)


;; * Language markdown
;; Major mode for editing Markdown formatted text
(use-package markdown-mode
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))


;; * Language rust
(defun setup-rustic ()
  (yas-minor-mode))

(use-package rustic
  :ensure t
  :hook (rustic-mode . setup-rustic)
  :mode ("\\.rs\\'" . rustic-mode))

;; * Language clojure
(use-package cider
  :defer t
  :hook (clojure-mode . enable-paredit-mode))


;; * Language python
(use-package elpy
  :init
  (elpy-enable))
