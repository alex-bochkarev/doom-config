;;; my-email.el -*- lexical-binding: t; -*-
;;; My setup, mu / mu4e / mbsync

;; email setup
(add-to-list 'load-path "/usr/share/emacs/site-lisp/elpa/mu4e-1.12.8")
(require 'mu4e)

(setq +mu4e-compose-org-msg-toggle-next nil)

(defun ab-highlight-names ()
  "Highlights @mentions in the current buffer."
  (interactive)
  (font-lock-add-keywords nil '(("@[A-Za-z]+[:]*" . font-lock-warning-face)))
  (font-lock-fontify-buffer))

(set-email-account!
 "Private"
 '( ( user-mail-address	    . "a@bochkarev.io"  )
    ( user-full-name	    . "Alexey Bochkarev" )
    ( mu4e-compose-signature .
                             (concat
                              "Alexey Bochkarev\n"
                              "https://www.bochkarev.io\n"
                              "matrix: @bochkarev:matrix.org\n"
                              "telegram: t.me/abochka\n"))
    ;; set up maildir folders
    (mu4e-sent-folder . "/personal/Sent")
    (mu4e-drafts-folder . "/personal/Drafts")
    (mu4e-trash-folder . "/personal/Trash")
    (mu4e-refile-folder . "/personal/Archive")
    ;; sending mail preferences
    (mu4e-sent-messages-behavior . sent)
    (smtpmail-queue-dir . "~/.mail/personal/queue/cur")
    (message-send-mail-function . smtpmail-send-it)
    (smtpmail-smtp-user . "a@bochkarev.io")
    (smtpmail-starttls-credentials . (("smtp.mailbox.org" 587 nil nil)))
    (smtpmail-auth-credentials . (expand-file-name "~/.authinfo.gpg"))
    (smtpmail-default-smtp-server . "smtp.mailbox.org")
    (smtpmail-smtp-server . "smtp.mailbox.org")
    (smtpmail-smtp-service . 587)
    (stmpmail-stream-type 'ssl)
    (smtpmail-debug-info . t)
    (smtpmail-debug-verbose . t)
    ))

(set-email-account!
 "CU"
 '( ( user-mail-address	     . "abochka@g.clemson.edu" )
    ( user-full-name	     . "Alexey Bochkarev" )
    ( mu4e-compose-signature  .
                              (concat
                               "Alexey Bochkarev\n"
                               "https://www.bochkarev.io\n"
                               "matrix: @bochkarev:matrix.org\n"
                               "telegram: t.me/abochka\n"))
    ;; set up maildir folders
    (mu4e-sent-folder . "/CU/Sent")
    (mu4e-drafts-folder . "/CU/Drafts")
    (mu4e-trash-folder . "/CU/Trash")
    (mu4e-refile-folder . "/CU/Archive")
    ;; sending mail preferences
    (mu4e-sent-messages-behavior . delete)
    (smtpmail-queue-dir . "~/.mail/CU/queue/cur")
    (message-send-mail-function . smtpmail-send-it)
    (smtpmail-smtp-user . "abochka@g.clemson.edu")
    (smtpmail-starttls-credentials . (("smtp.gmail.com" 587 nil nil)))
    (smtpmail-auth-credentials . (expand-file-name "~/.authinfo.gpg"))
    (smtpmail-default-smtp-server . "smtp.gmail.com")
    (smtpmail-smtp-server . "smtp.gmail.com")
    (smtpmail-smtp-service . 587)
    (smtpmail-debug-info . t)
    (smtpmail-debug-verbose . t)
    ))

(set-email-account!
 "Work"
 '( ( user-mail-address	     . "a.bochkarev@rptu.de" )
    ( user-full-name	     . "Alexey Bochkarev" )
    ( mu4e-compose-signature  .
                              (concat
                               "Alexey Bochkarev\n"
                               "Postdoc @ RPTU Kaiserslautern-Landau :: AG Optimierung,\n"
                               "tel: +49 (0)631 205 3925\n"
                               "https://www.bochkarev.io\n"
                               "matrix: @bochkarev:matrix.org\n"
                               "telegram: @abochka (https://t.me/abochka)"))
    ;; set up maildir folders
    (mu4e-sent-folder . "/RPTU/Sent")
    (mu4e-drafts-folder . "/RPTU/Drafts")
    (mu4e-trash-folder . "/RPTU/Trash")
    (mu4e-refile-folder . "/RPTU/Archive")
    ;; sending mail preferences
    (mu4e-sent-messages-behavior . sent)
    (smtpmail-queue-dir . "~/.mail/RPTU/queue/cur")
    (message-send-mail-function . smtpmail-send-it)
    (smtpmail-smtp-user . "a.bochkarev@rptu.de")
    (smtpmail-auth-credentials . (expand-file-name "~/.authinfo.gpg"))
    (smtpmail-default-smtp-server . "smtp.uni-kl.de")
    (smtpmail-smtp-server . "smtp.uni-kl.de")
    (smtpmail-smtp-service . 587)
    (smtpmail-debug-info . t)
    (smtpmail-debug-verbose . t)
    ))

(set-email-account!
 "legacy"
 '( ( user-mail-address	     . "aabochkaryov@gmail.com" )
    ( user-full-name	     . "Alexey Bochkarev" )
    ( mu4e-compose-signature  .
                              (concat
                               "Alexey Bochkarev\n"
                               "https://www.bochkarev.io\n"
                               "matrix: @bochkarev:matrix.org\n"
                               "telegram: t.me/abochka\n"))
    ;; set up maildir folders
    (mu4e-sent-folder . "/legacy/Sent")
    (mu4e-drafts-folder . "/legacy/Drafts")
    (mu4e-trash-folder . "/legacy/Trash")
    (mu4e-refile-folder . "/legacy/Archive")
    ;; sending mail preferences
    (mu4e-sent-messages-behavior . delete)
    (smtpmail-queue-dir . "~/.mail/legacy/queue/cur")
    (message-send-mail-function . smtpmail-send-it)
    (smtpmail-smtp-user . "aabochkaryov@gmail.com")
    (smtpmail-starttls-credentials . (("smtp.gmail.com" 587 nil nil)))
    (smtpmail-auth-credentials . (expand-file-name "~/.authinfo.gpg"))
    (smtpmail-default-smtp-server . "smtp.gmail.com")
    (smtpmail-smtp-server . "smtp.gmail.com")
    (smtpmail-smtp-service . 587)
    (smtpmail-debug-info . t)
    (smtpmail-debug-verbose . t)
    ))

;; config based on the Spacemacs docs
;; Set up some common mu4e variables

(setq mu4e-maildir "~/.mail"
      mu4e-get-mail-command "mbsync -a"
      mu4e-update-interval nil
      mu4e-compose-signature-auto-include t
      ;; mu4e-compose-format-flowed t
      mu4e-attachment-dir "~/Downloads/email"
      mu4e-view-show-images t
      mu4e-view-show-addresses t)

;; Mail directory shortcuts
(setq mu4e-maildir-shortcuts
      '(("/CU/INBOX" . ?c)
        ("/personal/INBOX" . ?p)
        ("/RPTU/INBOX" . ?w)
        ("/personal/lists" . ?l)
        ("/legacy/INBOX" . ?g)))

;; Bookmarks
(setq mu4e-bookmarks
      `(("flag:unread AND NOT flag:trashed AND NOT maildir:/\/.+\/Trash/" "Unread messages" ?u)
        ("date:today..now AND NOT maildir:/\/.+\/Trash/" "Today's messages" ?t)
        ("date:7d..now AND NOT flag:trashed AND NOT maildir:/\/.+\/Trash/" "Last 7 days" ?w)
        ("flag:flagged" "Flagged/starred" ?f)
        ("maildir:/.+\/lists/" "Mailing lists" ?l)
        (,(concat "(" (mapconcat 'identity
                             (mapcar
                              (lambda (maildir)
                                (concat "maildir:" (car maildir)))
                              mu4e-maildir-shortcuts) " OR ") ") AND NOT maildir:/.+\/lists/")
         "All inboxes" ?i)
        ("((maildir:/\/.+/Sent/) OR (from:Alexey Bochkarev)) AND NOT flag:trashed" "All sent" ?s)))

(setq mail-user-agent #'mu4e-user-agent
      message-mail-user-agent t)
