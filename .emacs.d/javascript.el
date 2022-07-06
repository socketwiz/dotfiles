;;; settings.el --- Language JavaScript/TypeScript

;; Author: Ricky Nelson <rickyn@socketwiz.com>

;;; Commentary:
;; Settings the JavaScript/TypeScript, HTML, and CSS languages for frontend development

;;; Code:
;; To enable the lsp backend:
;; npm install -g typescript-language-server typescript
;; jsconfig.json
;; {
;;  "compilerOptions": {
;;    "target": "es2017",
;;    "allowSyntheticDefaultImports": true,
;;    "noEmit": true,
;;    "checkJs": true,
;;    "jsx": "react",
;;    "lib": [ "dom", "es2017" ]
;;  }
;; }
;;
;; tsconfig.json
;; {
;;  "indentSize": 2,
;;  "tabSize": 2
;; }
(defun configure-flymake-checker ()
  "Configure flymake for JavaScript."

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
      (setq-local flymake-eslint-executable-name eslint)
      ;; Set project root folders
      (setq-local flymake-eslint-project-root root)))

  (flymake-eslint-enable))

(defun configure-mode ()
  "When \"tide-mode\" is loaded setup linters, yas and such."
  (if (equal web-mode-content-type "javascript")
    (web-mode-set-content-type "jsx")
    (message "now set to: %s" web-mode-content-type))
  (configure-flymake-checker)
  (define-key evil-normal-state-map (kbd "M-.") 'tide-jump-to-definition)
  (tide-hl-identifier-mode)
  (tide-setup)
  (yas-minor-mode)
  (smartparens-mode)
  (show-smartparens-mode)
  (prettier-js-mode)
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

(use-package js2-mode
  :mode "\\.js\\'"
  :interpreter "node")

(use-package prettier-js)

(add-hook 'js-mode-hook 'configure-mode)
(add-hook 'typescript-mode-hook 'configure-mode)
(add-hook 'web-mode-hook 'configure-mode)

;; * Language HTML, CSS
(defun web-mode-init ()
  "Setup yas when in \"web-mode\"."
  (interactive)
  (yas-minor-mode)
  (configure-flymake-checker)
  ;; disable auto-pairing just in web-mode so in django templates
  ;; you can do {% %} without it becoming {% %}}
  (electric-pair-mode -1))

;; Major mode for editing web templates
(use-package web-mode
  :if config-enable-web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.css\\'" . web-mode)
         ("\\.js\\'" . web-mode)
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

(use-package indium
  :init
  (add-hook 'js-mode-hook 'indium-interaction-mode))

(provide 'javascript)
;;; javascript.el ends here
