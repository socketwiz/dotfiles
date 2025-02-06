;;; javascript.el --- Language JavaScript/TypeScript

;; Author: Ricky Nelson <rickyn@socketwiz.com>

;;; Commentary:
;; Setting the JavaScript/TypeScript, HTML, and CSS languages for frontend development

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

;; Flymake eslint backend
(use-package flymake-eslint
  :if config-enable-web-mode)

(use-package prettier-js
  :ensure t)

(defun enable-prettier-js-mode ()
  "Enable `prettier-js-mode`."
  (prettier-js-mode t))

(use-package js-base-mode
  :defer t
  :ensure js
  :custom
  (js-indent-level 2)
  :config
  (unbind-key "M-." js-base-mode-map)
  :hook ((js-base-mode . enable-prettier-js-mode)
         (js-base-mode . flymake-eslint-enable)))

(use-package typescript-ts-mode
  :defer t
  :custom
  (typescript-indent-level 2)
  :config
  (unbind-key "M-." typescript-ts-base-mode-map)
  :hook (typescript-ts-mode . enable-prettier-js-mode))

(defun setup-vue-mode ()
  "Custom configurations for editing Vue files with web-mode."
  (setq web-mode-content-type "vue")
  ;; Set up embedded JS mode
  (add-to-list 'web-mode-engines-alist '("vue" . "\\.vue\\'"))
  (web-mode-set-content-type "vue")
  ;; Use js-mode for JavaScript sections
  (setq-local web-mode-script-padding 0)
  (setq-local web-mode-style-padding 0)
  (setq-local web-mode-block-padding 0)

  ;; Enable prettier-js-mode for web-mode
  (enable-prettier-js-mode))

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
  :mode (("\\.html?\\'" . web-mode)
         ("\\.vue\\'" . web-mode))
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
  (web-mode-enable-css-colorization t)
  :hook (web-mode . (lambda ()
                      (when (string-equal "vue" (file-name-extension buffer-file-name))
                        (setup-vue-mode)))))

(use-package css-mode
  :hook (css-mode . enable-prettier-js-mode))


(provide 'javascript)
;;; javascript.el ends here
