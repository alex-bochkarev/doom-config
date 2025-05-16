;;; my-email.el -*- lexical-binding: t; -*-
;;; My setup, mu / mu4e / mbsync

;; email setup
(add-to-list 'load-path "~/.local/share/emacs/site-lisp/mu4e")
(require 'mu4e)

(setq +mu4e-compose-org-msg-toggle-next nil)

(set-email-account!
 "personal"
 '( ( user-mail-address	    . "a@bochkarev.io"  )
    ( user-full-name	    . "Alexey Bochkarev" )
    ( mu4e-compose-signature .
                             (concat
                              "Dr. Alexey Bochkarev\n"
                              "https://www.bochkarev.io\n"
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
 "RPTU"
 '( ( user-mail-address	     . "a.bochkarev@rptu.de" )
    ( user-full-name	     . "Alexey Bochkarev" )
    ( mu4e-compose-signature  .
                              (concat
                               "Alexey Bochkarev\n"
                               "RPTU Kaiserslautern-Landau :: AG Opt,\n"
                               "https://www.bochkarev.io\n"
                               "tel: +49 (0)631 205 5327\n"
                               ))
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
      mu4e-compose-signature-auto-include nil  ; include it manually with C-c C-w
      ;; mu4e-compose-format-flowed t
      mu4e-attachment-dir "~/Downloads/email"
      mu4e-view-show-images t
      mu4e-view-show-addresses t
      message-citation-line-format "On %Y-%m-%d %a, %H:%M (%Z), %f wrote:\n"
      message-citation-line-function 'message-insert-formatted-citation-line)

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
        ("maildir:/\/.+\/Archive/" "All archives" ?a)
        ("((maildir:/\/.+/Sent/) OR (from:Alexey Bochkarev)) AND NOT flag:trashed" "All sent" ?s)))

(setq mail-user-agent #'mu4e-user-agent
      message-mail-user-agent t)

(defun insert-mu4e-sig-here ()
  "Insert the mu4e signature here, assuming it is a string."
  (interactive)
  (save-excursion
    (let ((my-sign (eval mu4e-compose-signature)))
      (when (stringp my-sign)
        (insert (concat "\n" my-sign "\n"))))))


(map! :map message-mode-map
      :desc "Insert current signature here." "C-c C-w" #'insert-mu4e-sig-here)
