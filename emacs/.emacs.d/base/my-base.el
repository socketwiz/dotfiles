
(require 'my-evil)
;; (require 'my-ido)
(require 'my-projectile)
(require 'my-emacs)
(require 'my-git)

(defun my/init-base ()
  (my/init-evil-base)
  ;; (my/init-ido)
  (my/init-projectile)
  (my/init-emacs)
  (my/init-git))

(provide 'my-base)
