;;; init.el --- General packages

;; Author: Ricky Nelson <rickyn@socketwiz.com>

;;; Commentary:
;; Package setup, configuration, and general packages (not language specific)

;;; Code:
(require 'package)
(setq package-archives '(("elpa" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Install quelpa
(unless (package-installed-p 'quelpa)
  (package-refresh-contents)
  (package-install 'quelpa))

(use-package quelpa-use-package
  :ensure t)

;; * Core packages
(use-package diminish)

;; Flymake mode - syntax checking
(use-package flymake
  :diminish flymake
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
         :map flymake-mode-map
         ("C-c ! l" . consult-flymake)
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
(use-package rainbow-mode)

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

;; https://github.com/emacs-jp/replace-colorthemes
(use-package all-the-icons
  :ensure t
  :config
  (defun my/all-the-icons-fonts-installed-p ()
    "Check if all-the-icons fonts are installed in the user's font directory."
    (let ((font-dir (cond
                     ((eq system-type 'darwin) "~/Library/Fonts/") ;; macOS
                     ((eq system-type 'gnu/linux) "~/.local/share/fonts/") ;; Linux
                     ((eq system-type 'windows-nt) "~/AppData/Local/Microsoft/Windows/Fonts/") ;; Windows
                     (t nil))) ;; Unknown system
          (fonts '("all-the-icons.ttf" "file-icons.ttf" "fontawesome.ttf"
                   "material-design-icons.ttf" "octicons.ttf" "weathericons.ttf")))
      (and font-dir
           (cl-every (lambda (font)
                       (file-exists-p (expand-file-name font font-dir)))
                     fonts))))

  (unless (my/all-the-icons-fonts-installed-p)
    (all-the-icons-install-fonts t)))

(use-package nerd-icons
  :ensure t
  :config
  (defun my/nerd-icons-fonts-installed-p ()
    "Check if nerd-icons fonts are installed in the user's font directory."
    (let ((font-dir (cond
                     ((eq system-type 'darwin) "~/Library/Fonts/") ;; macOS
                     ((eq system-type 'gnu/linux) "~/.local/share/fonts/") ;; Linux
                     ((eq system-type 'windows-nt) "~/AppData/Local/Microsoft/Windows/Fonts/") ;; Windows
                     (t nil))) ;; Unknown system
          (fonts '("NerdFontsSymbolsOnly-Regular.ttf")))
      (and font-dir
           (cl-every (lambda (font)
                       (file-exists-p (expand-file-name font font-dir)))
                     fonts))))

  (unless (my/nerd-icons-fonts-installed-p)
    (nerd-icons-install-fonts t)))

(use-package all-the-icons-dired
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;; dired (directory editor)
(use-package dired
  :ensure nil ;; needed for some built-in packages
  :commands (dired dired-jump)
  :custom
  ((dired-listing-switches (if (eq system-type 'darwin)
                               "-alh" ;; Use macOS-compatible flags
                             "-ahgo --group-directories-first"))) ;; Use GNU ls flags elsewhere
  :bind (("C-x C-j" . dired-jump)))

;; Snippets, a template system for emacs
(use-package yasnippet
  :bind ("TAB" . yas-expand)
  :config
  (declare-function yas-reload-all "yasnippet.el")
  (yas-reload-all))

;; Display available keybindings in a popup
(use-package which-key
  :diminish 'which-key-mode
  :config
  (which-key-mode)
  :custom
  (which-key-idle-delay config-which-key-delay))

;; Org mode, for keeping notes, todo lists, etc... in plain text
(use-package org
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :custom
  (org-todo-keywords '("TODO" "STARTED" "DONE"))
  (org-agenda-files (directory-files-recursively "~/org/agenda" "org$"))
  (org-capture-templates
   (quote (("a" "agenda" entry (file "~/org/agenda/inbox.org")
            "* TODO %?\n%U\n%a\n")
           ("n" "note" entry (file "~/org/notes.org")
            "* %? :NOTE:\n%U\n%a\n")
           ("r" "read-later" entry (file "~/org/read-later.org")
            "* %? :NOTE:\n%U\n%a\n")
           ("w" "work" entry (file "~/org/work.org")
            "* %? :NOTE:\n%U\n%a\n"))))
  (org-hide-emphasis-markers t)
  :config
  (add-hook 'org-mode-hook (lambda () (yas-minor-mode)))
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

;; Respect editor configs
(use-package editorconfig
  :diminish 'editorconfig-mode
  :config
  (editorconfig-mode t))

;; A better "help" buffer
(use-package helpful
  :bind ("C-c C-." . helpful-at-point))

;; Builds a list of recently opened files
(use-package recentf
  :custom
  (recentf-max-saved-items 10)
  (recentf-max-menu-items 5)
  ;; look for a .cache directory under ~/.emacs.d and create it if it
  ;; doesn't exist
  (setq recentf-cache-dir (concat user-emacs-directory ".cache"))
  (if (file-directory-p recentf-cache-dir)
      (setq recentf-save-file (concat recentf-cache-dir "/recentf"))
    (mkdir recentf-cache-dir)
    (setq recentf-save-file (concat recentf-cache-dir "/recentf")))

  (recentf-auto-cleanup 'never)
  :config
  (recentf-mode t)

  (add-to-list 'recentf-exclude (expand-file-name package-user-dir))
  (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
  (add-to-list 'recentf-exclude (expand-file-name (concat user-emacs-directory ".cache/")))
  (declare-function recentf-save-list "recentf.el.gz")
  (add-hook 'delete-terminal-functions (lambda (terminal) (recentf-save-list))))

;; Minor mode for dealing with pairs, such as parenthesis and quotes
(use-package smartparens
  :config
  (smartparens-global-mode t)
  (add-hook 'emacs-lisp-mode-hook 'show-smartparens-mode)
  (add-hook 'js-ts-mode-hook 'show-smartparens-mode)
  (add-hook 'typescript-ts-mode-hook 'show-smartparens-mode)
  (add-hook 'rust-mode-hook 'show-smartparens-mode)
  (add-hook 'json-mode-hook 'show-smartparens-mode))

;; Modeline theme, bottom of each window
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

;; Completion package that divides the pattern into space-separated components
(use-package orderless
  :ensure t
  :custom (completion-styles '(orderless)))

;; Switch between buffers and visit files
;; 
(use-package vertico
  :init
  (vertico-mode))

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  :init
  (marginalia-mode))

;; Upgraded completing-read, allows you to quickly select from a list
;; of candidates
(use-package consult
  :bind (("C-x b" . consult-buffer))
  :config
  (setq consult-project-root-function 'vc-root-dir))

;; Wrapp for ripgrep, a grep with features for programmers, like
;; respecting .gitignore
(use-package rg
  :config
  ;; <C-c s>
  (rg-enable-default-bindings))

;; Major mode for editing Markdown formatted text
(use-package markdown-mode
  :if config-enable-markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom (markdown-command "multimarkdown"))

;; Major mode for docker files
(use-package dockerfile-mode
  :if config-enable-dockerfile-mode
  :defer t)

;; Major mode for editing YAML documents
(use-package yaml-mode
  :if config-enable-yaml-mode
  :mode ("\\.yml\\'" . yaml-mode))

;; Major mode for GraphQL
(use-package graphql-mode)

;; Majro mode for terraform
(use-package terraform-mode)

;; Extensible vi layer for Emacs
(declare-function evil-set-initial-state "evil-core.el")
(use-package evil
  :if config-enable-evil-mode
  :config
  ;; Put vim bindings everywhere
  (evil-mode)
  ;; Except in these modes where I just want emacs proper
  (evil-set-initial-state 'apropos-mode 'emacs)
  (evil-set-initial-state 'Buffer-menu-mode 'emacs)
  (evil-set-initial-state 'compilation-mode 'emacs)
  (evil-set-initial-state 'debugger-mode 'emacs)
  (evil-set-initial-state 'diff-mode 'emacs)
  (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'emacs-lisp-mode 'emacs)
  (evil-set-initial-state 'finder-mode 'emacs)
  (evil-set-initial-state 'flymake-diagnostics-buffer-mode 'emacs)
  (evil-set-initial-state 'helpful-mode 'emacs)
  (evil-set-initial-state 'help-mode 'emacs)
  (evil-set-initial-state 'image-mode 'emacs)
  (evil-set-initial-state 'Info-mode 'emacs)
  (evil-set-initial-state 'markdown-view-mode 'emacs)
  (evil-set-initial-state 'messages-buffer-mode 'emacs)
  (evil-set-initial-state 'outline-mode 'emacs)
  (evil-set-initial-state 'org-mode 'emacs)
  (evil-set-initial-state 'reb-mode 'emacs)
  (evil-set-initial-state 'rg-mode 'emacs)
  (evil-set-initial-state 'snippet-mode 'emacs)
  (evil-set-initial-state 'special-mode 'emacs)
  (evil-set-initial-state 'text-mode 'emacs)
  (evil-set-initial-state 'fundamental-mode 'emacs)

  ;; Set emacs state on message buffer with this special code since it loads
  ;; so early and therefore can't be set with `(evil-set-initial-state)`
  (with-eval-after-load 'evil
    (add-hook 'after-init-hook
              (lambda (&rest _)
                (when-let ((messages-buffer (get-buffer "*Messages*")))
                  (with-current-buffer messages-buffer
                    (evil-emacs-state))))))

  ;; Set undo-tree as the undo/redo system. I thought one could set
  ;; the variable evil-undo-system to enable this, but I can't seem to
  ;; get that to work :( evil-set-undo-system seems to work, though
  ;; I'm not sure if this is recommended
  (evil-set-undo-system 'undo-tree)

  ;; For some reason these modes are starting in emacs state, set them to normal
  (evil-set-initial-state 'markdown-mode 'normal)
  (evil-set-initial-state 'python-mode 'normal)
  (evil-set-initial-state 'yaml-mode 'normal)

  (define-key evil-normal-state-map (kbd "C-c C-/") 'comment-dwim)
  (define-key evil-motion-state-map (kbd "/") 'swiper)

  (unless (fboundp 'shell-completion-vars)
    (defun shell-completion-vars () nil)))

;; Surround text objects with characters
(use-package evil-surround
  :if config-enable-evil-mode
  :after (evil)
  :config
  (global-evil-surround-mode 1))

;; Advanced undo/redo system
(use-package undo-tree
  :config
  (setq undo-tree-auto-save-history t)
  :init
  (global-undo-tree-mode))

;; An alternative to isearch
(use-package swiper
  :bind (("C-s" . swiper)
         ("C-r" . swiper-backward)))

;; In-buffer completion framework
(use-package company
  :init
  (global-company-mode))

;; Highlights uncommited changes on the left side of the window
(use-package diff-hl
  :init
  (global-diff-hl-mode)
  (diff-hl-margin-mode))

;; Syntax tree parser, used for coloring languages among other things
;; Tree-sitter configuration
(use-package tree-sitter
  :ensure t
  :hook (prog-mode . global-tree-sitter-mode)
  :config
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))
(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

(use-package eglot
  :init
  (setq eglot-stay-out-of '(flymake))
  :config
  (add-to-list 'eglot-server-programs '(web-mode . ("vls")))
  :hook (prog-mode . eglot-ensure)
  :bind (("M-TAB" . completion-at-point)
         ("M-g i" . imenu)
         ("C-h ." . display-local-help)
         ("M-." . xref-find-definitions)
         ("M-," . xref-go-back)
         :map
         eglot-mode-map
         ("C-c c a" . eglot-code-actions)
         ("C-c c o" . eglot-code-actions-organize-imports)
         ("C-c c r" . eglot-rename)
	 ("C-c c f" . eglot-format)))

(use-package eldoc
  :init
  (global-eldoc-mode))

;; AI pair programmer
;; M-x copilot-install-server
(use-package copilot
  :quelpa (copilot :fetcher github
                   :repo "copilot-emacs/copilot.el"
                   :branch "main"
                   :files ("*.el"))
  :bind (("C-j" . copilot-accept-completion))
  :hook (prog-mode . copilot-mode))

;; Provides a transient over info pages to make them easier to navigate
(use-package transient
  :ensure t)
(use-package casual
  :ensure t
  :after transient ;; Ensure transient is loaded first
  :bind (:map Info-mode-map
              ("C-o" . casual-info-tmenu))
  :config
  (require 'casual-info))

(provide 'packages)
;;; packages.el ends here
