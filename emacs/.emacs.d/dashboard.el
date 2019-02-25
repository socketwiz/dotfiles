
(defun exit-dashboard ()
  "Stop displaying the dashboard buffer."
  (interactive)
  (quit-window t))

(defvar dashboard-screen-keymap
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (set-keymap-parent map button-buffer-map)
    (define-key map "q" 'exit-dashboard)
    map)
  "Keymap for dashboard buffer.")

(defconst fancy-startup-text
  `((:face (variable-pitch font-lock-comment-face)
           "Welcome to "
           :link ("GNU Emacs",
                  (lambda (_button) (browse-url "https://www.gnu.org/software/emacs/"))
                  "Browse https://www.gnu.org/software/emacs/")
           ", one component of the "
           :link,
           (lambda ()
             (if (eq system-type 'gnu/linux)
                 `("GNU/Linux",
                   (lambda (_button) (browse-url "https://www.gnu.org/gnu/linux-and-gnu.html"))
                   "Browse https://www.gnu.org/gnu/linux-and-gnu.html")
               `("GNU", (lambda (_button)
                          (browse-url "https://www.gnu.org/gnu/thegnuproject.html"))
                 "Browse https://www.gnu.org/gnu/thegnuproject.html")))
           " operating system.\n\n")))

(defun load-dashboard ()
  "Display a custom dashboard on startup"
  (let ((dashboard-buffer (get-buffer-create "*dashboard*")))
    (with-current-buffer dashboard-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)

        (fancy-splash-head)
        (dolist (text fancy-startup-text)
          (apply #'fancy-splash-insert text)
          (insert "\n"))

        (fancy-splash-insert
         :face 'variable-pitch "Recent Files:"
         :face 'variable-pitch "\n")

        (dolist (recent recentf-list)
          (if (not (string-suffix-p "ido.last" recent))
              (progn
                (defconst file-text
                  `((:link (,recent
                            (lambda (_button) (find-file ,recent))))))

                (apply #'fancy-splash-insert (car file-text))
                (insert "\n"))))

        (setq buffer-read-only t)
        (set-buffer-modified-p nil)
        (if (and view-read-only (not view-mode))
            (view-mode-enter nil 'kill-buffer))
        (goto-char (point-min))
        (forward-line 6)

        (use-local-map dashboard-screen-keymap)
        (switch-to-buffer dashboard-buffer)))))

(defun load-state ()
  (let ((dashboard-buffer (get-buffer-create "*dashboard*")))
    (with-current-buffer dashboard-buffer
      (evil-emacs-state))))

(defun dashboard ()
  (add-hook 'recentf-mode-hook 'load-dashboard)
  (add-hook 'evil-normal-state-entry-hook 'load-state))

(provide'dashboard)
