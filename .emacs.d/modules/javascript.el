;;; javascript.el --- JavaScript, TypeScript, HTML, CSS setup -*- lexical-binding: t; -*-

;; Author: Ricky Nelson <rickyn@socketwiz.com>

;;; Commentary:
;; Configuration for JavaScript, TypeScript, HTML, CSS, Vue development.

;;; Code:

;; Flymake ESLint backend
(use-package flymake-eslint
  :if config-enable-web-mode)

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
    (prettier-js-mode 1)))

;; JavaScript base mode
(use-package js-base-mode
  :ensure nil
  :defer t
  :custom
  (js-indent-level 2)
  :config
  (unbind-key "M-." js-base-mode-map)
  :hook ((js-base-mode . maybe-enable-prettier)
         (js-base-mode . flymake-eslint-enable)))

;; TypeScript mode
(use-package typescript-ts-mode
  :defer t
  :custom
  (typescript-indent-level 2)
  :config
  (unbind-key "M-." typescript-ts-base-mode-map)
  :hook (typescript-ts-mode . maybe-enable-prettier))

;; Vue and Web Mode setup
(defun setup-vue-mode ()
  "Setup settings specific for Vue files."
  (setq web-mode-content-type "vue")
  (add-to-list 'web-mode-engines-alist '("vue" . "\\.vue\\'"))
  (web-mode-set-content-type "vue")
  (setq-local web-mode-script-padding 0)
  (setq-local web-mode-style-padding 0)
  (setq-local web-mode-block-padding 0)
  (maybe-enable-prettier))

(defun web-mode-init ()
  "Setup yasnippet and disable electric pair for web-mode."
  (yas-minor-mode 1)
  (electric-pair-mode -1))

(use-package web-mode
  :if config-enable-web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.vue\\'" . web-mode))
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
  (web-mode-attr-indent-offset config-indent-web-mode-spaces)
  :hook (web-mode . (lambda ()
                      (when (and buffer-file-name
                                 (string-equal "vue" (file-name-extension buffer-file-name)))
                        (setup-vue-mode)))))

;; CSS Mode
(use-package css-mode
  :hook (css-mode . maybe-enable-prettier))

;; Flymake ESLint checker
(defun flymake-eslint--checker (report-fn &rest _args)
  "Run ESLint on the current buffer and report results to REPORT-FN."
  (let* ((source (current-buffer))
         (eslint-bin (executable-find "eslint"))
         (buffer (generate-new-buffer "*flymake-eslint*")))
    (if (not (and eslint-bin (buffer-file-name source)))
        (progn
          (flymake-log :warning "eslint executable not found or buffer has no file")
          (funcall report-fn nil))
      (let ((default-directory (or (locate-dominating-file (buffer-file-name source) ".eslintrc.js")
                                   default-directory)))
        (let ((proc
               (make-process
                :name "flymake-eslint"
                :buffer buffer
                :command (list eslint-bin "--format=json" "--stdin" "--stdin-filename" (buffer-file-name source))
                :noquery t
                :connection-type 'pipe
                :sentinel
                (lambda (proc _event)
                  (when (eq (process-status proc) 'exit)
                    (let ((exit-code (process-exit-status proc)))
                      (if (= exit-code 0)
                          (with-current-buffer (process-buffer proc)
                            (let ((parsed-diagnostics nil)) ;; TODO: parse ESLint output
                              (condition-case nil
                                  (funcall report-fn parsed-diagnostics)
                                (error (flymake-log :warning "Flymake state gone for eslint")))))
                        (flymake-log :warning "eslint exited with code %d" exit-code)
                        (condition-case nil
                            (funcall report-fn nil)
                          (error (flymake-log :warning "Flymake state gone after eslint fail")))))))))))
        (process-send-region proc (point-min) (point-max))
        (process-send-eof proc)))))

(defun setup-eslint-flymake ()
  "Setup Flymake ESLint for JS modes."
  (add-hook 'flymake-diagnostic-functions #'flymake-eslint--checker nil t))

(add-hook 'js-mode-hook #'setup-eslint-flymake)
(add-hook 'js-ts-mode-hook #'setup-eslint-flymake)

(provide 'javascript)
;;; javascript.el ends here
