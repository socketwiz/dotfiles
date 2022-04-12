
(use-package web-mode
  :ensure t
  :bind (("C-n" . web-mode-tag-match))
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'"  . web-mode))       ;; HTML
  (add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))       ;; JS/JSX
  (add-to-list 'auto-mode-alist '("\\.css\\'"    . web-mode))       ;; CSS
  (add-to-list 'auto-mode-alist '("\\.scss\\'"   . web-mode))       ;; SCSS

  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "jsx")
	(let ((web-mode-enable-part-face nil))
	  ad-do-it)
      ad-do-it))

  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "js")
	(let ((web-mode-enable-part-face nil))
	  ad-do-it)
      ad-do-it))
  :init
  (setq web-mode-content-types-alist
	'(("jsx" . "\\.js[x]?\\'")
	  ("javascript" . "\\.es6?\\'")))

  ;; indent with 4 spaces
  (setq-default indent-tabs-mode nil)
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t))

