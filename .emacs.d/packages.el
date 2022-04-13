;;; init.el --- General packages

;; Author: Ricky Nelson <rickyn@socketwiz.com>

;;; Commentary:
;; Package setup, configuration, and general packages (not language specific)

;;; Code:
(package-initialize)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-ensure t)

;; Set the path from the shell
;; call after (package-initialize)
(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

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
  (load-theme 'doom-vibrant t)
  ;; This theme makes the selections too dark, lighten them up
  ;;(set-face-background 'hl-line "#1F2324")
  ;;(set-face-background 'region "#585F61")

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

;; dired (directory editor)
(use-package dired
  :ensure nil ;; needed for some built-in packages
  :commands (dired dired-jump)
  :custom ((dired-listing-switches "-ahgo --group-directories-first"))
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
  (org-todo-keywords '("TODO" "STARTED" "WAITING" "DONE"))
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

;; Minor mode for dealing with pairs, such as parenthesis and quotes
(use-package smartparens
  :config
  (smartparens-global-mode t))

;; Expand selected region by semantic units
(use-package expand-region
  :bind (("C-=" . er/expand-region))
  :config
  (pending-delete-mode t))

;; Modeline theme, bottom of each window
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

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

;; Provide actions on targets
(use-package embark
  :ensure t
  :bind (("C-." . embark-act)         ;; pick some comfortable binding
         ("C-;" . embark-dwim)))      ;; good alternative: M-.
(use-package embark-consult
  :ensure t
  :after (embark consult))

;; Upgraded completing-read, allows you to quickly select from a list
;; of candidates
(use-package consult
  :bind (("M-s r" . consult-ripgrep)
         ("C-x b" . consult-buffer)
         ("C-x C-r" . consult-recent-file))
  :config
  (setq consult-project-root-function 'vc-root-dir))

;; Wrapp for ripgrep, a grep with features for programmers, like
;; respecting .gitignore
(use-package rg
  :config
  (rg-enable-default-bindings))

;; Major mode for editing Markdown formatted text
(use-package markdown-mode
  :if config-enable-markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom (markdown-command "multimarkdown"))

;; Syntax highlighting for docker files
(use-package dockerfile-mode
  :if config-enable-dockerfile-mode
  :defer t)

;; Major mode for editing YAML documents
(use-package yaml-mode
  :if config-enable-yaml-mode
  :mode ("\\.yml\\'" . yaml-mode))

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
  ;; I'm not sure if this is the recommended
  (evil-set-undo-system 'undo-tree)

  ;; For some reason these modes are starting in emacs state, set them to normal
  (evil-set-initial-state 'python-mode 'normal)

  (define-key evil-normal-state-map (kbd "C-c C-/") 'comment-dwim)
  (define-key evil-motion-state-map (kbd "/") 'swiper))


;; Surround text objects with characters
(use-package evil-surround
  :if config-enable-evil-mode
  :after (evil)
  :config
  (global-evil-surround-mode 1))

;; Language Server Protocol support for Emacs
(use-package eglot)

;; Jump to visible text using a char-based decision tree
(use-package avy
  :bind ("M-j" . avy-goto-char-timer)
  :config
  (setq avy-timeout-seconds 1))

;; Advanced undo/redo system
(use-package undo-tree
  :config
  (setq undo-tree-auto-save-history t)
  :init
  (global-undo-tree-mode))

;; In-buffer completion framework
(use-package company
  :init
  (global-company-mode))

(use-package diff-hl
  :init
  (global-diff-hl-mode))

;; An alternative to isearch
(use-package swiper
  :bind (("C-s" . swiper)
         ("C-r" . swiper-backward)))


(provide 'packages)
;;; packages.el ends here
