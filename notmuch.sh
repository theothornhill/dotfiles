#!/bin/sh
notmuch new
notmuch tag -inbox +emacs to:emacs-devel@gnu.org
notmuch tag -inbox +emacs to:*@debbugs.gnu.org
notmuch tag -inbox +emacs cc:*@debbugs.gnu.org
notmuch tag -inbox +sbcl-devel to:sbcl-devel@lists.sourceforge.net
notmuch tag +allegria from:ensemble.allegria@gmail.com
