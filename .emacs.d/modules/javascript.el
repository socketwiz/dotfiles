;;; javascript.el --- JavaScript, TypeScript, HTML, CSS setup -*- lexical-binding: t; -*-

;; Author: Ricky Nelson <rickyn@socketwiz.com>

;;; Commentary:
;; Configuration for JavaScript, TypeScript, HTML, CSS, Vue development.

;;; Code:

;; Prettier formatting
(use-package prettier-js
  :ensure t)

(defun project-has-prettier-config-p ()
  "Check if the project has a Prettier configuration."
  (let ((root (locate-dominating-file default-directory
                                      (lambda (dir)
                                        (directory-files dir nil
                                                         "\\`\\(\\.prettierrc\\|\\.prettierrc\\..*\\|prettier\\.config\\.js\\|package\\.json\\)\\'")))))
    (and root
         (or (file-exists-p (expand-file-name ".prettierrc" root))
             (file-exists-p (expand-file-name ".prettierrc.json" root))
             (file-exists-p (expand-file-name ".prettierrc.js" root))
             (file-exists-p (expand-file-name "prettier.config.js" root))
             (with-temp-buffer
               (when (file-exists-p (expand-file-name "package.json" root))
                 (insert-file-contents (expand-file-name "package.json" root))
                 (search-forward "\"prettier\"" nil t)))))))

(defun maybe-enable-prettier ()
  "Enable Prettier if a Prettier config is detected."
  (when (project-has-prettier-config-p)
    (prettier-js-mode 1)
    (add-hook 'before-save-hook #'prettier-js nil t)
    (message "Prettier enabled for %s" (buffer-name))))

;; JavaScript base mode
(use-package js-base-mode
  :ensure nil
  :defer t
  :custom
  (js-indent-level 2)
  :hook (js-base-mode . maybe-enable-prettier))

;; TypeScript mode
(use-package typescript-ts-mode
  :defer t
  :custom
  (typescript-indent-level 2)
  :hook (typescript-ts-mode . maybe-enable-prettier))

(defun web-mode-init ()
  "Setup yasnippet and disable electric pair for web-mode."
  (yas-minor-mode 1)
  (electric-pair-mode -1))

(use-package web-mode
  :if config-enable-web-mode
  :mode ("\\.html?\\'" . web-mode)
  :init
  (setq-default indent-tabs-mode nil)
  :config
  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "js")
        (let ((web-mode-enable-part-face nil))
          ad-do-it)
      ad-do-it))

  ;; Disable argument/concat lining
  (dolist (param '(("lineup-args" . nil)
                   ("lineup-calls" . nil)
                   ("lineup-concats" . nil)
                   ("lineup-ternary" . nil)))
    (add-to-list 'web-mode-indentation-params param))

  (add-hook 'web-mode-hook #'web-mode-init)
  :custom
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-css-colorization t)
  (web-mode-engines-alist '(("django" . "\\.html\\'")))
  (web-mode-enable-auto-quoting nil)
  (web-mode-markup-indent-offset config-indent-web-mode-spaces)
  (web-mode-css-indent-offset config-indent-web-mode-spaces)
  (web-mode-code-indent-offset config-indent-web-mode-spaces)
  (web-mode-attr-indent-offset config-indent-web-mode-spaces))

;; CSS Mode
(use-package css-mode
  :hook (css-mode . maybe-enable-prettier))

(use-package js-ts-mode
  :ensure nil
  :defer t)

(provide 'javascript)
;;; javascript.el ends here
