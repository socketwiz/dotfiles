;;; init.el --- Python language

;; Author: Ricky Nelson <rickyn@socketwiz.com>

;;; Commentary:
;; Settings needed to make Python development easier

;;; Code:
;; Need to activate pipenv
;; C-c C-p a
(defun setup-python ()
  "Do these things after \"python-mode\" is enabled."
  ;; To enable the eglot backend:
  ;; pip install 'python-lsp-server[all]'
  ;; pip install pyls-flake8
  (eglot-ensure)
  (jedi:setup))

(use-package elpy
  :after jedi
  :if config-enable-elpy-mode
  :hook (python-mode . setup-python)
  :init
  (setq python-shell-interpreter "python3"
      python-shell-interpreter-args "-i")
  (elpy-enable))
(use-package jedi)
(use-package pipenv
  :hook (python-mode . pipenv-mode))

(provide 'python)
