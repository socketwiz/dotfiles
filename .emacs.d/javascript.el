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
(defun configure-web-mode ()
  "Need an extra check here to setup JSX."
  (message "conifgure-web-mode")
  (if (equal web-mode-content-type "javascript")
      (funcall (lambda ()
        (message "web-mode-content-type=javascript")
        (web-mode-set-content-type "jsx")
        (message "now set to: %s" web-mode-content-type))))
  (configure-mode))

(defun configure-mode ()
  "Setup linters, yas and such."
  (eglot-ensure)
  (yas-minor-mode)
  (smartparens-mode)
  (show-smartparens-mode)
  (prettier-js-mode)
  (setq js-indent-level config-indent-web-mode-spaces)
  (setq eldoc-documentation-strategy #'eldoc-documentation-compose))

;; Flymake eslint backend
(use-package flymake-eslint
  :if config-enable-web-mode)

(use-package prettier-js)

(add-hook 'css-ts-mode-hook 'configure-mode)
(add-hook 'js-ts-mode-hook 'configure-mode)
(add-hook 'typescript-ts-mode-hook 'configure-mode)
(add-hook 'web-mode-hook 'configure-web-mode)

;; treesit-install-language-grammar
;;(add-to-list 'auto-mode-alist '("\\.css\\'" . css-ts-mode))
;;(add-to-list 'auto-mode-alist '("\\.scss\\'" . css-ts-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-ts-mode))

;; * Language HTML
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
  :mode (("\\.html?\\'" . web-mode))
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

(use-package indium
  :init
  (add-hook 'js-mode-hook 'indium-interaction-mode))

(provide 'javascript)
;;; javascript.el ends here
