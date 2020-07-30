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
        notmuch-saved-searches '((:name "inbox" :query "tag:inbox" :key "i")
                                 (:name "unread" :query "tag:unread" :key "u")
                                 (:name "flagged" :query "tag:flagged" :key "f")
                                 (:name "sent" :query "tag:sent" :key "t")
                                 (:name "drafts" :query "tag:draft" :key "d")
                                 (:name "emacs-devel" :query "to:emacs-devel@gnu.org" :key "e")
                                 (:name "emacs-bug" :query "to:*@debbugs.gnu.org" :key "b")
                                 (:name "sbcl-devel" :query "to:sbcl-devel@lists.sourceforge.net" :key "s")
                                 (:name "all mail" :query "*" :key "a"))))

(provide 'notmuch-settings)
