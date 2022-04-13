;;; email.el --- MU4E email configuration

;; Author: Ricky Nelson <rickyn@socketwiz.com>

;;; Commentary:
;; Configuration required to check email through MU4E

;;; Code:
;; MU4E
;; Need to install mu4e OS package
;; e.g. `sudo apt-get install mu4e isync`
;; Initialize mu
;; mu init --maildir=~/mail/hackerzol430 --my-address=hackerzol430@gmail.com
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(load "/usr/share/emacs/site-lisp/mu4e/mu4e.el")

; Refresh mail using isync every 5 minutes
(setq mu4e-update-interval (* 5 60))
(setq mu4e-get-mail-command "mbsync -a")
(setq mu4e-maildir "~/mail")

;; Make sure that moving a message (like to Trash) causes the
;; message to get a new file name.  This helps to avoid the
;; dreaded "UID is N beyond highest assigned" error.
;; See this link for more info: https://stackoverflow.com/a/43461973
(setq mu4e-change-filenames-when-moving t)

;; Display options
(setq mu4e-view-show-images t)
(setq mu4e-view-show-addresses 't)

;; Prevent mu4e from permanently deleting trashed items
;; This snippet was taken from the following article:
;; http://cachestocaches.com/2017/3/complete-guide-email-emacs-using-mu-and-/
(defun remove-nth-element (nth list)
  (if (zerop nth) (cdr list)
    (let ((last (nthcdr (1- nth) list)))
      (setcdr last (cddr last))
      list)))
(setq mu4e-marks (remove-nth-element 5 mu4e-marks))
(add-to-list 'mu4e-marks
             '(trash
               :char ("d" . "▼")
               :prompt "dtrash"
               :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
               :action (lambda (docid msg target)
                         (mu4e~proc-move docid
                                         (mu4e~mark-check-target target) "-N"))))

;; Use mu4e for sending e-mail
(setq mail-user-agent 'mu4e-user-agent
      message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 465
      smtpmail-stream-type  'ssl)

;; Alert when a new message comes in
(use-package mu4e-alert
  :ensure t
  :after mu4e
  :init
  (setq mu4e-alert-interesting-mail-query
    (concat
     "flag:unread maildir:/hackerzol430/inbox "
     "OR "
     "flag:unread maildir:/socketwiz/inbox"
     "OR "
     "flag:unread maildir:/larksoftware/inbox"))
  (mu4e-alert-enable-mode-line-display))

(setq mu4e-contexts
        `(,(make-mu4e-context
            :name "hackerzol"
            :match-func (lambda (msg) (when msg
                                        (string-prefix-p "/hackerzol" (mu4e-message-field msg :maildir))))
            :vars '(
                    (user-full-name . "Ricky Nelson")
                    (user-mail-address . "hackerzol430@gmail.com")
                    (mu4e-sent-folder . "/hackerzol430/\[Gmail\]/Sent Mail")
                    (mu4e-trash-folder . "/hackerzol430/\[Gmail\]/Trash")
                    (mu4e-drafts-folder . "/hackerzol430/\[Gmail\]/Drafts")
                    (mu4e-refile-folder . "/hackerzol430/archive")
                    (smtpmail-smtp-user "hackerzol430@gmail.com")
                    ))
	  ,(make-mu4e-context
            :name "socketwiz"
            :match-func (lambda (msg) (when msg
                                        (string-prefix-p "/socketwiz" (mu4e-message-field msg :maildir))))
            :vars '(
                    (user-full-name . "Ricky Nelson")
                    (user-mail-address . "rickyn@socketwiz.com")
                    (mu4e-sent-folder . "/hackerzol430/\[Gmail\]/Sent Mail")
                    (mu4e-trash-folder . "/hackerzol430/\[Gmail\]/Trash")
                    (mu4e-drafts-folder . "/hackerzol430/\[Gmail\]/Drafts")
                    (mu4e-refile-folder . "/socketwiz/archive")
                    (smtpmail-smtp-user "rickyn@socketwiz.com")
                    ))
	  ,(make-mu4e-context
            :name "larksoftware"
            :match-func (lambda (msg) (when msg
                                        (string-prefix-p "/larksoftware" (mu4e-message-field msg :maildir))))
            :vars '(
                    (user-full-name . "Ricky Nelson")
                    (user-mail-address . "info@larksoftware.com")
                    (mu4e-sent-folder . "/hackerzol430/\[Gmail\]/Sent Mail")
                    (mu4e-trash-folder . "/hackerzol430/\[Gmail\]/Trash")
                    (mu4e-drafts-folder . "/hackerzol430/\[Gmail\]/Drafts")
                    (mu4e-refile-folder . "/socketwiz/archive")
                    (smtpmail-smtp-user "info@larksoftware.com")
                    ))
          ))

;; Start mu4e in the background so that it syncs mail periodically
(mu4e t)

(provide 'email)
;;; email.el ends here
