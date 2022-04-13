;;; init.el --- Variables required by Emacs configuration

;; Author: Ricky Nelson <rickyn@socketwiz.com>

;;; Commentary:
;; Variables used to customize Emacs

;;; Code:
(defvar config-which-key-delay 0.0
  "How long before which-key displays.")

(defvar config-keep-backups t
  "Whether or not to make a backup of a file when it is saved.")

(defvar config-font-family "Fira Code"
  "Font to use in each buffer.")
(defvar config-font-height 160
  "The font-height is 1/10pt so 160 == 160/10 == 16pt.")

(defvar config-indent-web-mode-spaces 2
  "How many spaces to indent in \"web-mode\".")

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
(defvar config-enable-web-mode t
  "Whether or not to enable web, js, css.")
(defvar config-enable-shell-mode t
  "Whether or not to enable \"shell-mode\".")
(defvar config-enable-dockerfile-mode t
  "Whether or not to enable \"dockerfile-mode\".")
(defvar config-enable-yaml-mode t
  "Whether or not to enable \"yaml-mode\".")

;; Prevent the startup window
(setq inhibit-startup-message t)

;; Keep backups
(setq make-backup-files config-keep-backups)

;; Save ALL backup files to this location
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
;; Undo history
(setq undo-tree-history-directory-alist `(("." . "~/.emacs.d/undo")))

;; Disable re-center of the cursor to the middle of page when scroll hits top or bottom of the page
(setq scroll-conservatively 101)

;; Give focus to new help windows
(setq help-window-select t)

(provide 'variables)
;;; variables.el ends here
