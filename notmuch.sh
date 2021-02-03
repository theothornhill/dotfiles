#!/bin/sh
/usr/local/bin/mbsync -a

/usr/local/bin/notmuch new
/usr/local/bin/notmuch tag -inbox +emacs to:emacs-devel@gnu.org
/usr/local/bin/notmuch tag -inbox +emacs-bug to:*@debbugs.gnu.org
/usr/local/bin/notmuch tag -inbox +emacs-bug cc:*@debbugs.gnu.org
/usr/local/bin/notmuch tag +allegria from:ensemble.allegria@gmail.com OR to:ensemble.allegria@gmail.com
/usr/local/bin/notmuch tag +barnehage from:donotreply@info.kidplan.com
/usr/local/bin/notmuch tag -inbox +bugs +github from:notifications@github.com
/usr/local/bin/notmuch tag -inbox +bugs +sourcehut to:~*@todo.sr.ht OR from:~outgoing@sr.ht
/usr/local/bin/notmuch tag -inbox +sbcl-devel to:sbcl-devel@lists.sourceforge.net
/usr/local/bin/notmuch tag +allegria from:ensemble.allegria@gmail.com
