;;; bootstrap.el --- Bootstrap Emacs package manager -*- lexical-binding: t; -*-

;; Author: Ricky Nelson <rickyn@socketwiz.com>

;;; Commentary:
;; Sets up package.el, use-package, quelpa.

;;; Code:

(require 'package)

(setq package-archives
      '(("elpa" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")))

(setq package-enable-at-startup nil) ;; Prevent double-init (important!)

(unless package--initialized
  (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(unless (package-installed-p 'quelpa)
  (package-refresh-contents)
  (package-install 'quelpa))

(use-package quelpa-use-package
  :ensure t)

(provide 'bootstrap)
;;; bootstrap.el ends here
