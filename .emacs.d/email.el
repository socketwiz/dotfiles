;;; email.el --- MU4E email configuration

;; Author: Ricky Nelson <rickyn@socketwiz.com>

;;; Commentary:
;; Configuration required to check email through MU4E

;;; Code:
;; MU4E
;; Need to install mu4e OS package
;; e.g. `sudo apt-get install mu4e isync`
;; Initialize mu
;; mu init --maildir=~/mail/ --my-address=<email1> --my-address=<email2> --my-address=<emailN>
;; mu index
;; Sample .mbsyncrc
;;
;; IMAPAccount <name>
;; # Address to connect to
;; Host imap.mail.us-east-1.awsapps.com
;; User <email | username>
;; PassCmd "gpg2 -q --for-your-eyes-only --no-tty -d ~/.mailpass-<name>.gpg"
;; SSLType IMAPS
;; CertificateFile /etc/ssl/certs/ca-certificates.crt
;;
;; IMAPStore <name>-remote
;; Account <name>
;;
;; MaildirStore <name>-local
;; SubFolders Verbatim
;; # The trailing "/" is important
;; Path ~/mail/<name>/
;; Inbox ~/mail/<name>/inbox
;;
;; Channel <name>
;; Master :<name>-remote:
;; Slave :<name>-local:
;; Patterns * !"[Gmail]/All Mail" !"[Gmail]/Important" !"[Gmail]/Starred" !"[Gmail]/Drafts" !"[Gmail]/Spam"
;; # Automatically create missing mailboxes, both locally and on the server
;; Create Both
;; # Sync the movement of messages between folders and deletions, add after making sure the sync works
;; Expunge Both
;; # Save the synchronization state files in the relevant directory
;; SyncState *
;;
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(require 'mu4e)

(setq mu4e-maildir "~/mail")

;; Make sure that moving a message (like to Trash) causes the
;; message to get a new file name.  This helps to avoid the
;; dreaded "UID is N beyond highest assigned" error.
;; See this link for more info: https://stackoverflow.com/a/43461973
(setq mu4e-change-filenames-when-moving t)

;; Display options
(setq mu4e-view-show-images t)
(setq mu4e-view-show-addresses 't)

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
     "flag:unread maildir:/socketwiz/inbox "
     "OR "
     "flag:unread maildir:/larksoftware/inbox "
     "OR "
     "flag:unread maildir:/zolmok/inbox "
     "OR "
     "flag:unread maildir:/picnic/inbox"))
  (setq mu4e-bookmarks
        '((:name "Unread messages" :query "flag:unread maildir:/hackerzol430/inbox OR flag:unread maildir:/socketwiz/inbox OR flag:unread maildir:/larksoftware/inbox OR flag:unread maildir:/zolmok/inbox" :key 117)
        (:name "Today's messages" :query "date:today..now" :key 116)
        (:name "Last 7 days" :query "date:7d..now" :hide-unread t :key 119)
        (:name "Messages with images" :query "mime:image/*" :key 112)))
  (mu4e-alert-enable-mode-line-display))

(setq mu4e-contexts
        `(,(make-mu4e-context
            :name "hackerzol"
            :match-func (lambda (msg) (when msg
                                        (mu4e-message-contact-field-matches
                                         msg :to "hackerzol430@gmail.com")))
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
                                        (mu4e-message-contact-field-matches
                                         msg :to "rickyn@socketwiz.com")))
            :vars '(
                    (user-full-name . "Ricky Nelson")
                    (user-mail-address . "rickyn@socketwiz.com")
                    (mu4e-sent-folder . "/socketwiz/\[Gmail\]/Sent Mail")
                    (mu4e-trash-folder . "/socketwiz/\[Gmail\]/Trash")
                    (mu4e-drafts-folder . "/socketwiz/\[Gmail\]/Drafts")
                    (mu4e-refile-folder . "/socketwiz/archive")
                    (smtpmail-smtp-user "rickyn@socketwiz.com")
                    ))
	  ,(make-mu4e-context
            :name "larksoftware"
            :match-func (lambda (msg) (when msg
                                        (mu4e-message-contact-field-matches
                                         msg :to "info@larksoftware.com")))
            :vars '(
                    (user-full-name . "Ricky Nelson")
                    (user-mail-address . "info@larksoftware.com")
                    (mu4e-sent-folder . "/larksoftware/\[Gmail\]/Sent Mail")
                    (mu4e-trash-folder . "/larksoftware/\[Gmail\]/Trash")
                    (mu4e-drafts-folder . "/larksoftware/\[Gmail\]/Drafts")
                    (mu4e-refile-folder . "/larksoftware/archive")
                    (smtpmail-smtp-user "info@larksoftware.com")
                    ))
	  ,(make-mu4e-context
            :name "zolmok"
            :match-func (lambda (msg) (when msg
                                        (mu4e-message-contact-field-matches
                                         msg :to "rickyn@zolmok.org")))
            :vars '(
                    (user-full-name . "Ricky Nelson")
                    (user-mail-address . "rickyn@zolmok.org")
                    (mu4e-sent-folder . "/zolmok/Sent Items")
                    (mu4e-trash-folder . "/zolmok/Deleted Items")
                    (mu4e-drafts-folder . "/zolmok/Drafts")
                    (mu4e-refile-folder . "/zolmok/Archive")
                    (smtpmail-smtp-user "info@larksoftware.com")
                    ))
          ,(make-mu4e-context
            :name "picnic"
            :match-func (lambda (msg) (when msg
                                        (mu4e-message-contact-field-matches
                                         msg :to "ricky.nelson@picnicscore.com")))
            :vars '(
                    (user-full-name . "Ricky Nelson")
                    (user-mail-address . "ricky.nelson@picnicscore.com")
                    (mu4e-sent-folder . "/picnic/\[Gmail\]/Sent Mail")
                    (mu4e-trash-folder . "/picnic/\[Gmail\]/Trash")
                    (mu4e-drafts-folder . "/picnic/\[Gmail\]/Drafts")
                    (mu4e-refile-folder . "/picnic/archive")
                    (smtpmail-smtp-user "ricky.nelson@picnicscore.com")
                    ))
          ))

(provide 'email)
;;; email.el ends here
