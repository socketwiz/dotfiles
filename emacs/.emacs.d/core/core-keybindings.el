
(defun my/global-map () 
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-s") 'swiper)
  (global-set-key (kbd "C-x C-b") 'ivy-switch-buffer)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-x C-r") 'counsel-recentf)
  (global-set-key (kbd "C-h b") 'counsel-descbinds))

(provide 'core-keybindings)
