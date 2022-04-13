;;; dashboard.el --- Display a dashboard of activities on Emacs startup

;; Author: Ricky Nelson <rickyn@socketwiz.com>

;;; Commentary:
;; Provide a dashboard of quick links

;;; Code:

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

(defun action-org-agenda (button)
  "BUTTON to launch the \"org-agenda\"."
  (org-agenda))

(defun action-open-project (button)
  "BUTTON to open a project."
  (counsel-projectile-switch-project))

(defun action-open-config (button)
  "BUTTON to open Emacs configuration."
  (find-file "~/.emacs.d/settings.el"))

(defun action-open-wiki (button)
  "BUTTON to open personal notes and documentation."
  (find-file "~/org/wiki/index.org"))

(define-button-type 'org-agenda-button
  'action 'action-org-agenda
  'follow-link t
  'help-echo' "Open org-agenda")

(define-button-type 'open-project-button
  'action 'action-open-project
  'follow-link t
  'help-echo "Open project")

(define-button-type 'open-config-button
  'action 'action-open-config
  'follow-link t
  'help-echo "Open Emacs configuration")

(define-button-type 'open-wiki-button
  'action 'action-open-wiki
  'follow-link t
  'help-echo "Open wiki")

(defun create-link (text button-type start)
  "Create a clickable link containing TEXT.
BUTTON-TYPE: will trigger an action when clicked.
START: point location to start the link."
  (insert text)
  (make-button start (point) :type button-type))

(defun load-dashboard ()
  "Display a custom dashboard on startup."
  (let ((dashboard-buffer (get-buffer-create "*dashboard*")))
    (with-current-buffer dashboard-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (set-window-margins nil 10 10)

        (create-link "Org Agenda" 'org-agenda-button 1)
        (insert "\n")
        (create-link "Open Project" 'open-project-button 12)
        (insert "\n")
        (create-link "Open Emacs Configuration" 'open-config-button 25)
        (insert "\n")
        (create-link "Open Wiki" 'open-wiki-button 50)
        (insert "\n")

        (setq buffer-read-only t)
        (set-buffer-modified-p nil)
        (if (and view-read-only (not view-mode))
            (view-mode-enter nil 'kill-buffer))
        (goto-char (point-min))

        (use-local-map dashboard-screen-keymap)
        (switch-to-buffer dashboard-buffer)))))

(defun dashboard ()
  "Initialize dashboard settings."
  (add-hook 'recentf-mode-hook 'load-dashboard))

(provide 'dashboard)

;;; dashboard.el ends here
