
(use-package web-mode
  :ensure t
  :bind (("C-n" . web-mode-tag-match))
  :mode (("\\.html?\\'" . web-mode)
         ("\\.js[x]?\\'" . web-mode)
         ("\\.css\\'" . web-mode)
         ("\\.scss\\'" . web-mode))
  :config
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
  ;; disable auto-quoting
  (setq web-mode-enable-auto-quoting nil)
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

