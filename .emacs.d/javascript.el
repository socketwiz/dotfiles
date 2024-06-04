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

(use-package prettier-js)

(use-package js-base-mode
  :defer 't
  :ensure js
  :custom
  (js-indent-level 2)
  :config
  (unbind-key "M-." js-base-mode-map))

(use-package typescript-ts-mode
  :ensure typescript-ts-mode
  :defer 't
  :custom
  (typescript-indent-level 2)
  :config
  (unbind-key "M-." typescript-ts-base-mode-map))

(use-package indium
  :init
  (add-hook 'js-mode-hook 'indium-interaction-mode))

(provide 'javascript)
;;; javascript.el ends here
