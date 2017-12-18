
(require 'core-config)
(require 'core-keybindings)
(require 'core-makeup)

(defun my/init ()
  (my/init-use-package)
  (my/init-diminish)
  (my/init-bind-map)
  (my/init-which-key)
  (my/init-makeup))

(defun my/init-diminish ()
  (use-package diminish))

(defun my/init-use-package ()
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

(defun my/init-bind-map ()
  (my/global-map))

(defun my/init-which-key ()
  (use-package which-key
    :defer t
    :diminish which-key-mode)
  (which-key-mode)
  (setq which-key-idle-delay config-which-key-delay))

(provide 'core-my)
