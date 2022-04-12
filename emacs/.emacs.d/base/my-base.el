
(require 'my-evil)
(require 'my-ivy)
(require 'my-projectile)
(require 'my-emacs)
(require 'my-git)
(require 'my-hydra)

(defun my/init-base ()
  (my/init-evil-base)
  (my/init-hydra)
  (my/init-ivy)
  (my/init-projectile)
  (my/init-emacs)
  (my/init-git))

(provide 'my-base)
