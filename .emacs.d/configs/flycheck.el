
(defun configure-web-mode-flycheck-checkers ()
  ;; in order to have flycheck enabled in web-mode, add an entry to this
  ;; cond that matches the web-mode engine/content-type/etc and returns the
  ;; appropriate checker.
  (-when-let (checker (cond
                       ((string= web-mode-content-type "jsx")
                        'javascript-eslint)))
    (flycheck-mode)
    (flycheck-select-checker checker)))

(use-package flycheck
  :ensure t

  :config
  ;; turn on flychecking globally
  ;; (global-flycheck-mode)

  ;; use eslint with web-mode for jsx files
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (add-hook 'web-mode-hook #'configure-web-mode-flycheck-checkers))
