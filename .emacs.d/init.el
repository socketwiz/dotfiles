;;; init.el --- Main Emacs initialization -*- lexical-binding: t; -*-

;; Load bootstrap
(load (expand-file-name "bootstrap.el" user-emacs-directory))

;; Set up custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; Shell environment setup
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-copy-envs '("PATH" "NODE_PATH")))

(let ((node-bin (string-trim (shell-command-to-string "rtx which node"))))
  (when (and node-bin (not (string-empty-p node-bin)))
    (setenv "PATH" (concat (file-name-directory node-bin) ":" (getenv "PATH")))
    (add-to-list 'exec-path (file-name-directory node-bin))))

;; Load critical config variables early
(load (expand-file-name "modules/variables.el" user-emacs-directory))

;; Load all remaining modules automatically
(let ((modules-dir (expand-file-name "modules" user-emacs-directory)))
  (add-to-list 'load-path modules-dir)
  (dolist (file (directory-files modules-dir t "^[^#].*\\.el$"))
    (unless (string-match-p "variables\\.el\\'" file)
      (load file))))

(provide 'init)
;;; init.el ends here
