
(require 'my-cpp)
(require 'my-elisp)
(require 'my-javascript)
(require 'my-markdown)
(require 'my-java)

(defun my/init-langs ()
  (my/init-cpp)
  (my/init-elisp)
  (my/init-javascript)
  (my/init-markdown)
  (my/init-java))

(provide 'my-langs)
