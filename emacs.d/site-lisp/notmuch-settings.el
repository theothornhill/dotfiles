;; -*- lexical-binding: t; -*-

(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

(use-package notmuch
  :if (executable-find "notmuch")
  :defer t
  :bind (("C-x m" . 'notmuch-mua-new-mail)
         ("C-x n m" . 'notmuch))
  :config
  (setq message-sendmail-f-is-evil nil
        ;; message-fill-column nil
        notmuch-show-logo nil
        notmuch-search-oldest-first nil
        mail-specify-envelope-from t
        gnus-dired-mode t
        message-directory "~/.mail"
        user-full-name "Theodor Thornhill"
        user-mail-address "theo@thornhill.no"
        sendmail-program "msmtp"
        send-mail-function 'sendmail-send-it
        message-sendmail-envelope-from 'header
        mail-envelope-from 'header
        notmuch-fcc-dirs "main/Sent +sent -inbox -unread"
        notmuch-saved-searches '((:name "inbox" :query "tag:inbox" :key "i" :sort-order newest-first)
                                 (:name "unread" :query "tag:unread" :key "u" :sort-order newest-first)
                                 (:name "flagged" :query "tag:flagged" :key "f" :sort-order newest-first)
                                 (:name "sent" :query "tag:sent" :key "s" :sort-order newest-first)
                                 (:name "drafts" :query "tag:draft" :key "d" :sort-order newest-first)
                                 (:name "emacs" :query "to:emacs-devel@gnu.org OR to:*@debbugs.gnu.org" :key "e" :sort-order newest-first)
                                 (:name "bugs" :query "tag:bugs" :key "b" :sort-order newest-first)
                                 (:name "sbcl-devel" :query "tag:sbcl-devel" :key "l" :sort-order newest-first)
                                 (:name "all mail" :query "*" :key "a" :sort-order newest-first))))

(provide 'notmuch-settings)
