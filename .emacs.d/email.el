;;; email.el --- Mu4e email configuration

;; Author: Ricky Nelson <rickyn@socketwiz.com>

;;; Commentary:
;; Configuration required to check email through Mu4e

;;; Code:
;; Mu4e
;; Need to install mu4e OS package
;; e.g. `sudo apt-get install mu4e isync`
;; mkdir ~/mail
;; mkdir ~/mail/<each-mailbox-defined-in-.mbsyncrc>
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
;; Flags:
;; D=_draft_
;; F=_flagged_ (i.e., ‘starred’)
;; N=_new_
;; P=_passed_ (i.e., forwarded)
;; R=_replied_
;; S=_seen_
;; T=_trashed_,
;; a=_has-attachment_
;; x=_encrypted_
;; s=_signed_
;; u=_unread_
;;
(setq mu4e-maildir "~/mail")

;; Make sure that moving a message (like to Trash) causes the
;; message to get a new file name.  This helps to avoid the
;; dreaded "UID is N beyond highest assigned" error.
;; See this link for more info: https://stackoverflow.com/a/43461973
(setq mu4e-change-filenames-when-moving t)

;; Display options
(setq mu4e-view-show-images t)
;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))
(setq mu4e-view-prefer-html t)
(setq mu4e-view-show-addresses 't)

;; Use mu4e for sending e-mail
(setq mail-user-agent 'mu4e-user-agent
      message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 465
      smtpmail-stream-type  'ssl)

(setq mu4e-bookmarks
      '((:name "Unread messages" :query config-mu4e-unread-query :key 117)
        (:name "Today's messages" :query "date:today..now" :key 116)
        (:name "Last 7 days" :query "date:7d..now" :hide-unread t :key 119)
        (:name "Messages with images" :query "mime:image/*" :key 112)))
(setq mu4e-completing-read-function 'completing-read)

(setq mu4e-contexts config-mu4e-contexts)

(provide 'email)
;;; email.el ends here
