
(require 'my-cpp)
(require 'my-elisp)
(require 'my-javascript)
(require 'my-markdown)

(defun my/init-langs ()
  (my/init-cpp)
  (my/init-elisp)
  (my/init-javascript)
  (my/init-markdown))

(provide 'my-langs)