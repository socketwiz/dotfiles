
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
         (next-win-buffer (window-buffer (next-window)))
         (this-win-edges (window-edges (selected-window)))
         (next-win-edges (window-edges (next-window)))
         (this-win-2nd (not (and (<= (car this-win-edges)
                     (car next-win-edges))
                     (<= (cadr this-win-edges)
                     (cadr next-win-edges)))))
         (splitter
          (if (= (car this-win-edges)
             (car (window-edges (next-window))))
          'split-window-horizontally
        'split-window-vertically)))
    (delete-other-windows)
    (let ((first-win (selected-window)))
      (funcall splitter)
      (if this-win-2nd (other-window 1))
      (set-window-buffer (selected-window) this-win-buffer)
      (set-window-buffer (next-window) next-win-buffer)
      (select-window first-win)
      (if this-win-2nd (other-window 1))))))

(defun my/global-map () 
  (global-set-key (kbd "<f5>") #'revert-buffer)
  (global-set-key (kbd "C-c a") #'org-agenda)
  (global-set-key (kbd "C-c c") #'org-capture)
  (global-set-key (kbd "C-c C-.") #'helpful-at-point)
  (global-set-key (kbd "C-c r") #'helm-recentf)
  (global-set-key (kbd "C-c t") #'switch-theme)
  (global-set-key (kbd "C-h b") #'describe-bindings)
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-x |") #'toggle-window-split)
  (global-set-key (kbd "C-x C-b") #'ibuffer)
  (global-set-key (kbd "C-x C-f") #'ido-find-file)
  (global-set-key (kbd "C-x g") #'magit-status)
  (global-set-key (kbd "C-x M-g") #'magit-dispatch-popup)
  (global-set-key (kbd "M-i") #'imenu)
  (global-set-key (kbd "M-x") #'helm-M-x))

(provide 'core-keybindings)
