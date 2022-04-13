;;; settings.el --- Emacs settings

;; Author: Ricky Nelson <rickyn@socketwiz.com>

;;; Commentary:
;; This package provides customized personal settings

;;; Code:

;; * Core
(package-initialize)

(defvar config-which-key-delay 0.0
  "How long before which-key displays.")

(defvar config-keep-backups t
  "Whether or not to make a backup of a file when it is saved.")

(defvar config-font-family "Fira Code"
  "Font to use in each buffer.")
(defvar config-font-height 180
  "The font-height is 1/10pt so 160 == 160/10 == 16pt.")

(defvar config-indent-web-mode-spaces 2
  "How many spaces to indent in \"web-mode\".")
(defvar js-switch-indent-offset 2
  "How many spaces to indent in \"js-mode\".")

(defvar config-enable-c-mode nil
  "Whether or not to enable c, c++.")
(defvar config-enable-command-log-mode nil
  "Whether or not to enable \"command-log-mode\".")
(defvar config-enable-elpy-mode t
  "Whether or not to enable \"elpy-mode\".")
(defvar config-enable-evil-mode t
  "Whether or not to enable \"evil-mode\".")
(defvar config-enable-markdown-mode t
  "Whether or not to enable \"markdown-mode\".")
(defvar config-enable-rust-mode t
  "Whether or not to enable \"rust-mode\".")
(defvar config-enable-undo-tree t
  "Whether or not to enable undo-tree.")
(defvar config-enable-web-mode t
  "Whether or not to enable web, js, css.")
(defvar config-enable-shell-mode t
  "Whether or not to enable \"shell-mode\".")
(defvar config-enable-dockerfile-mode t
  "Whether or not to enable \"dockerfile-mode\".")
(defvar config-enable-yaml-mode t
  "Whether or not to enable \"yaml-mode\".")


;; Hide column numbers
(setq column-number-mode t)

;; Draw underline lower
(setq x-underline-at-descent-line t)

;; Prevent the startup window
(setq inhibit-startup-message t)

;; Run the debugger on an error
;; (setq debug-on-error t)

;; Keep backups
(setq make-backup-files config-keep-backups)

;; Save ALL backup files to this location
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))

;; Disable locking of files (files with a # prefix)
;; ... at least until I can figure out how to move where they get saved
(setq create-lockfiles nil)

;; Disable re-center of the cursor to the middle of page when scroll hits top or bottom of the page
(setq scroll-conservatively 101)

;; Give focus to new help windows
(setq help-window-select t)

;; Set default indent to 4 spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; Recommendation from flx, tweak garbage collection
(setq gc-cons-threshold 20000000)

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
(add-to-list 'display-buffer-alist '("*Apropos*" display-buffer-same-window))
(add-to-list 'display-buffer-alist '("*Help*" display-buffer-same-window))
(add-to-list 'display-buffer-alist '("*Info*" display-buffer-same-window))

;; Package management
(custom-set-variables
 '(gnutls-algorithm-priority "normal:-vers-tls1.3"))
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
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
(set-register ?b '(file . "~/org/blog/index.org"))
(set-register ?i '(file . "~/org/agenda/inbox.org"))
(set-register ?s '(file . "~/.emacs.d/settings.el"))
(set-register ?w '(file . "~/org/wiki/index.org"))

;; when on MacOS, change meta to cmd key
(when (eq system-type 'darwin)
  ;; These 2 lines do not trigger flycheck warnings when on MacOS
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super))
;; When on MacOS, fix dired
(when (eq system-type 'darwin)
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil))

;; * Core keybindings
(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c r") 'consult-ripgrep)
(global-set-key (kbd "C-c C-.") 'helpful-at-point)
(global-set-key (kbd "C-h f") 'helpful-callable)
(global-set-key (kbd "C-h k") 'helpful-key)
(global-set-key (kbd "C-h v") 'helpful-variable)
(global-set-key (kbd "C-x C-b") 'consult-buffer)
(global-set-key (kbd "C-x C-e") 'pp-eval-last-sexp)
(global-set-key (kbd "C-x C-r") 'consult-recent-file)
(global-set-key (kbd "C-x C-z") 'nil)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch)

;; * Core packages
(use-package diminish)

;; Import PATH
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Construct a regex interactively
(use-package re-builder
  :config
  ;; Set regex syntax to string for re-builder
  (setq reb-re-syntax 'string))

;; Flymake mode - syntax checking
(use-package flymake
  :config
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  ;; Enable flymake for all programming modes
  (add-hook 'prog-mode-hook 'flymake-mode)
  ;; Tell which key what to display for the documentation keybind rather than display "lambda"
  (which-key-add-key-based-replacements "C-c ! i" "flymake-documentation")
  (which-key-add-key-based-replacements "C-c ! l" "flymake-list-errors")
  :bind (("C-c ! ?" . flymake-running-backends)
         ("C-c ! i" . (lambda ()
                        "Display Flymake documentation."
                        (interactive)
                        (info-display-manual "flymake")))
         ("C-c ! l" . flymake-show-diagnostics-buffer)
         ("C-c ! n" . flymake-goto-next-error)
         ("C-c ! p" . flymake-goto-prev-error)))

;; Compilation mode - compilation of log buffers
(use-package compile
  :config
  (defun socketwiz/compilation-filter-init ()
    "Initialize \"compilation-mode\" settings."
    ;; Colorize compilation-mode
    (declare-function ansi-color-apply-on-region "ansi-color.el.gz")
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  ;; Automatically scroll the compilation buffer
  :custom
  (compilation-scroll-output t)
  :hook (compilation-filter . socketwiz/compilation-filter-init))

;; Rainbow mode - displays color codes in their color
(use-package rainbow-mode
  :delight)

;; Theme
(use-package doom-themes
  :config
  (load-theme 'doom-acario-dark t)
  ;; This theme makes the selections too dark, lighten them up
  (set-face-background 'hl-line "#1F2324")
  (set-face-background 'region "#585F61")

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  :custom
  ;; Global settings (defaults)
  (doom-themes-enable-bold nil)
  (doom-themes-enable-italic nil))

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

;; dired (directory editor)
(use-package dired
  :ensure nil ;; needed for some built-in packages
  :commands (dired dired-jump)
  :custom ((dired-listing-switches "-ahgo --group-directories-first"))
  :bind (("C-x C-j" . dired-jump)))

;; Undo-tree
(use-package undo-tree
  :if config-enable-undo-tree
  :diminish 'undo-tree-mode
  :config
  (defadvice undo-tree-make-history-save-file-name
      (after undo-tree activate)
    (setq ad-return-value (concat ad-return-value ".gz")))
  (global-undo-tree-mode)
  :custom
  ;; save all undo histories to this location
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-diff t)
  (undo-tree-auto-save-history t))

;; A text completion framework
(use-package company
  :diminish 'company-mode
  :custom
  (company-tooltip-align-annotations t)
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.2)
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

;; Snippets, a template system for emacs
(use-package yasnippet
  :bind ("TAB" . yas-expand)
  :config
  (declare-function yas-reload-all "yasnippet.el")
  (yas-reload-all))

;; Language Server Protocol support for Emacs
(use-package eglot)

;; Display available keybindings in a popup
(use-package which-key
  :diminish 'which-key-mode
  :config
  (which-key-mode)
  :custom
  (which-key-idle-delay config-which-key-delay))

;; Highlight numbers for prog modes
(use-package highlight-numbers
  :init
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

;; Org mode, for keeping notes, todo lists, etc... in plain text
(use-package org
  :custom
  (org-todo-keywords '("TODO" "STARTED" "WAITING" "DONE"))
  (org-agenda-files (directory-files-recursively "~/org/agenda" "org$"))
  (org-capture-templates
   (quote (("a" "agenda" entry (file "~/org/agenda/inbox.org")
            "* TODO %?\n%U\n%a\n")
           ("n" "note" entry (file "~/org/notes.org")
            "* %? :NOTE:\n%U\n%a\n")
           ("r" "read-later" entry (file "~/org/read-later.org")
            "* %? :NOTE:\n%U\n%a\n"))))
  (org-hide-emphasis-markers t)
  :config
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  ;; So we can execute these language blocks in org-mode
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)))
  (defvar python-shell-completion-native-disabled-interpreters "python3")
  ;; tell org-open-at-point to open files in a new window
  (add-to-list 'org-link-frame-setup '(file . find-file)))

(use-package org-bullets
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; Convert Org buffer to text and decorations to HTML
(use-package htmlize
  :mode (("\\.org\\'" . org-mode)))

;; Respect editor configs
(use-package editorconfig
  :diminish 'editorconfig-mode
  :config
  (editorconfig-mode t))

;; A better "help" buffer
(use-package helpful)

;; Edit text area in chrome with emacs
(use-package atomic-chrome
  :config
  (atomic-chrome-start-server)
  :custom
  (atomic-chrome-buffer-open-style 'frame))

;; Builds a list of recently opened files
(use-package recentf
  :custom
  (recentf-max-saved-items 10)
  (recentf-max-menu-items 5)
  (recentf-save-file (concat user-emacs-directory ".cache/recentf"))
  (recentf-auto-cleanup 'never)
  :config
  (recentf-mode t)

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
  :custom
  (magit-refresh-status-buffer nil)
  :diminish 'auto-revert-mode)

;; Show diffs in the gutter
(use-package diff-hl
  :config
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (global-diff-hl-mode t)
  (diff-hl-flydiff-mode t))

;; Completion package that divides the pattern into space-separated components
(use-package orderless
  :ensure t
  :custom (completion-styles '(orderless)))

;; Switch between buffers and visit files
(use-package vertico
  :init
  (vertico-mode))

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  :init
  (marginalia-mode))

(use-package embark
  :ensure t

  :bind (("C-." . embark-act)         ;; pick some comfortable binding
         ("C-;" . embark-dwim)))      ;; good alternative: M-.

(use-package embark-consult
  :ensure t
  :after (embark consult))

(use-package consult
  :config
  (setq consult-project-root-function 'vc-root-dir))

;; ripgrep
(use-package rg
  :config
  (rg-enable-default-bindings))

;; Extensible vi layer for Emacs
(declare-function evil-set-initial-state "evil-core.el")
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
  (evil-set-initial-state 'finder-mode 'emacs)
  (evil-set-initial-state 'flymake-diagnostics-buffer-mode 'emacs)
  (evil-set-initial-state 'helpful-mode 'emacs)
  (evil-set-initial-state 'help-mode 'emacs)
  (evil-set-initial-state 'Info-mode 'emacs)
  (evil-set-initial-state 'markdown-view-mode 'emacs)
  (evil-set-initial-state 'messages-buffer-mode 'emacs)
  (evil-set-initial-state 'org-mode 'emacs)
  (evil-set-initial-state 'reb-mode 'emacs)
  (evil-set-initial-state 'rg-mode 'emacs)
  (evil-set-initial-state 'snippet-mode 'emacs)
  (evil-set-initial-state 'special-mode 'emacs)
  (evil-set-initial-state 'compilation-mode 'emacs)
  (evil-set-initial-state 'messages-buffer-mode 'emacs)

  ;; For some reason these modes are starting in emacs state, set them to normal
  (evil-set-initial-state 'python-mode 'normal)

  (define-key evil-normal-state-map (kbd "C-r") 'undo-tree-redo)
  (define-key evil-normal-state-map (kbd "u") 'undo-tree-undo)

  (define-key evil-normal-state-map (kbd "C-c C-/") 'comment-dwim))

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


;; * Language cpp
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


;; * Language JavaScript/TypeScript
(defun setup-typescript ()
  "When \"tide-mode\" is loaded setup linters, yas and such."
  ;; To enable the eglot backend:
  ;; npm install -g typescript-language-server
  (eglot-ensure)
  (define-key evil-normal-state-map (kbd "M-.") 'tide-jump-to-definition)
  (tide-hl-identifier-mode)
  (tide-setup)
  (yas-minor-mode)
  (setq js-indent-level config-indent-web-mode-spaces))

;; Flymake eslint backend
(use-package flymake-eslint
  :if config-enable-web-mode)

;; TypeScript Interactive Development Environment
;; M-x tide-restart-server Restart tsserver. This would come in handy after you edit tsconfig.json or checkout a different branch.
;; M-x tide-documentation-at-point Show documentation for the symbol at point.
;; M-x tide-references List all references to the symbol at point in a buffer. References can be navigated using n and p. Press enter to open the file.
;; M-x tide-project-errors List all errors in the project. Errors can be navigated using n and p. Press enter to open the file.
;; M-x tide-error-at-point Show the details of the error at point.
;; M-x tide-rename-symbol Rename all occurrences of the symbol at point.
;; M-x tide-rename-file Rename current file and all it's references in other files.
;; M-x tide-format Format the current region or buffer.
;; M-x tide-fix Apply code fix for the error at point. When invoked with a prefix arg, apply code fix for all the errors in the file that are similar to the error at point.
;; M-x tide-add-tslint-disable-next-line If the point is on one or more tslint errors, add a tslint:disable-next-line flag on the previous line to silence the errors. Or, if a flag already exists on the previous line, modify the flag to silence the errors.
;; M-x tide-refactor Refactor code at point or current region.
;; M-x tide-jsdoc-template Insert JSDoc comment template at point.
;; M-x tide-verify-setup Show the version of tsserver.
;; M-x tide-organize-imports Organize imports in the file.
;; M-x tide-list-servers List the tsserver processes launched by tide.
(use-package tide
  :if config-enable-web-mode
  :after (typescript-mode flycheck))

(add-hook 'typescript-mode-hook 'setup-typescript)
(add-hook 'js-mode-hook 'setup-typescript)


;; * Language HTML, CSS
(defun web-mode-init ()
  "Setup yas when in \"web-mode\"."
  (interactive)
  (yas-minor-mode)
  ;; disable auto-pairing just in web-mode so in django templates
  ;; you can do {% %} without it becoming {% %}}
  (electric-pair-mode -1))

;; Major mode for editing web templates
(use-package web-mode
  :if config-enable-web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.css\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)
         ("\\.json\\'" . web-mode))
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
  (setq-default indent-tabs-mode nil)
  (add-hook 'web-mode-hook 'web-mode-init)
  :custom
  (web-mode-engines-alist
   '(("django" . "\\.html\\'")))

  ;; Disable auto-quoting
  (web-mode-enable-auto-quoting nil)
  (web-mode-markup-indent-offset config-indent-web-mode-spaces)
  (web-mode-css-indent-offset config-indent-web-mode-spaces)
  (web-mode-code-indent-offset config-indent-web-mode-spaces)
  ;; Don't lineup element attributes
  (web-mode-attr-indent-offset config-indent-web-mode-spaces)
  ;; Automatically close tag
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-css-colorization t))

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
  :custom (markdown-command "multimarkdown"))


;; * Language Rust
(defun setup-rust ()
  "Do these things after \"rust-mode\" is enabled."
  ;; To enable the eglot backend:
  ;; rustup component add rls rust-analysis rust-src
  (eglot-ensure)
  (when (and (bound-and-true-p evil-mode) (eq evil-state 'normal))
    ;; Setup find-definitions when in rust-mode
    (define-key evil-normal-state-map (kbd "M-.") 'xref-find-definitions))
  (yas-minor-mode))

;; Syntax highlighting, indentation, etc..
(use-package rust-mode
  :if config-enable-rust-mode
  :hook (rust-mode . setup-rust)
  :mode ("\\.rs\\'" . rust-mode)
  :bind (:map rust-mode-map
              (("C-c C-c" . rust-compile)
               ("C-c C-k" . rust-check)
               ("C-c C-r" . rust-run))))


;; * Language Python
;; Need to activate pipenv
;; C-c C-p a
(defun setup-python ()
  "Do these things after \"python-mode\" is enabled."
  ;; To enable the eglot backend:
  ;; pip install 'python-lsp-server[all]'
  ;; pip install pyls-flake8
  (eglot-ensure)
  (jedi:setup))

(use-package elpy
  :after jedi
  :if config-enable-elpy-mode
  :hook (python-mode . setup-python)
  :init
  (setq python-shell-interpreter "python3"
      python-shell-interpreter-args "-i")
  (elpy-enable))
(use-package jedi)
(use-package pipenv
  :hook (python-mode . pipenv-mode))


;; Syntax highlighting for docker files
(use-package dockerfile-mode
  :if config-enable-dockerfile-mode
  :defer t)


;; Language YAML
(use-package yaml-mode
  :if config-enable-yaml-mode
  :mode ("\\.yml\\'" . yaml-mode))


;; Language Shell
;; Flymake shell backend
(use-package flymake-shellcheck
  :if config-enable-shell-mode
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))


(provide 'settings)

;;; settings.el ends here
