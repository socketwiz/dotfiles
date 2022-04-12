
(defun configure-web-mode-flycheck-checkers ()
  ;; in order to have flycheck enabled in web-mode, add an entry to this
  ;; cond that matches the web-mode engine/content-type/etc and returns the
  ;; appropriate checker.
  (-when-let (checker (cond
                       ((string= web-mode-content-type "jsx")
                        'javascript-eslint)))
    (flycheck-mode)
    ;; use the locally installed eslint
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint)))

    (flycheck-select-checker checker)))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1))

(defun my/init-javascript ()
  (use-package web-mode
    :ensure t
    :bind (:map web-mode-map ("C-n" . web-mode-tag-match))
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

    ;; disable lining up the args
    (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))
    :init
    (setq web-mode-content-types-alist
          '(("jsx" . "\\.js[x]?\\'")
            ("javascript" . "\\.es6?\\'")))

    ;; disable auto-quoting
    (setq web-mode-enable-auto-quoting nil)
    ;; indent with 4 spaces
    (setq-default indent-tabs-mode nil)
    (setq web-mode-markup-indent-offset 4)
    (setq web-mode-css-indent-offset 4)
    (setq web-mode-code-indent-offset 4)
    ;; automatically close tag
    (setq web-mode-enable-auto-pairing t)
    (setq web-mode-enable-css-colorization t)
    ;; don't lineup element attributes
    (setq web-mode-attr-indent-offset 4))

  (use-package skewer-mode
    :ensure t)

  (use-package tide
    :ensure t
    :defer 1
    :bind (("M-?" . tide-documentation-at-point)
           ("M-/" . tide-jump-to-definition))
    :config
    (add-hook 'web-mode-hook
              (lambda ()
                (when (string-match-p "js[x]?" (file-name-extension buffer-file-name))
                  (setup-tide-mode)))))

  ;; use eslint with web-mode for js[x]? files
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (add-hook 'web-mode-hook #'configure-web-mode-flycheck-checkers)

  (add-to-list 'auto-mode-alist '("\\.js[x]?'" . web-mode))
  (add-hook 'web-mode-hook #'yas-minor-mode))

(provide 'my-javascript)

