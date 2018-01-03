
(defun add-to-load-path (dir)
  (add-to-list 'load-path dir))

(defconst my-core-directory (expand-file-name (concat user-emacs-directory "core/"))
  "My core directory.")

(defconst my-base-directory (expand-file-name (concat user-emacs-directory "base/"))
  "My base directory.")

(defconst my-lang-directory (expand-file-name (concat user-emacs-directory "lang/"))
  "My lang directory.")

(mapc 'add-to-load-path `(,my-core-directory
                          ,my-base-directory
                          ,my-lang-directory
                          ,(concat my-core-directory "libs/")))
