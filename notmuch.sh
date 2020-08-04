#!/bin/sh
notmuch new
notmuch tag -inbox +emacs to:emacs-devel@gnu.org
notmuch tag -inbox +emacs-bug to:*@debbugs.gnu.org
notmuch tag -inbox +emacs-bug cc:*@debbugs.gnu.org
notmuch tag -inbox +bugs to:~*@todo.sr.ht OR from:~outgoing@sr.ht
notmuch tag -inbox +sbcl-devel to:sbcl-devel@lists.sourceforge.net
notmuch tag +allegria from:ensemble.allegria@gmail.com
