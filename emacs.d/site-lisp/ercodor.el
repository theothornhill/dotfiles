;; -*- lexical-binding: t; -*-


(defun ercodor-start ()
  (interactive)
  (load "~/.emacs.d/erc-auth.el")
  (erc :server "irc.freenode.net"
       :port "6667"
       :nick "theothornhill"
       :password erc-pass))

(setq erc-autojoin-channels-alist '(("freenode.net" "#sr.ht" "#lisp")))

(provide 'ercodor)
