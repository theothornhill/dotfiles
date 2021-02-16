#!/bin/sh
/opt/homebrew/bin/mbsync -a

/opt/homebrew/bin/notmuch new
/opt/homebrew/bin/notmuch tag -inbox +emacs to:emacs-devel@gnu.org
/opt/homebrew/bin/notmuch tag -inbox +emacs-bug to:*@debbugs.gnu.org
/opt/homebrew/bin/notmuch tag -inbox +emacs-bug cc:*@debbugs.gnu.org
/opt/homebrew/bin/notmuch tag +allegria from:ensemble.allegria@gmail.com OR to:ensemble.allegria@gmail.com
/opt/homebrew/bin/notmuch tag +barnehage from:donotreply@info.kidplan.com
/opt/homebrew/bin/notmuch tag -inbox +bugs +github from:notifications@github.com
/opt/homebrew/bin/notmuch tag -inbox +bugs +sourcehut to:~*@todo.sr.ht OR from:~outgoing@sr.ht
/opt/homebrew/bin/notmuch tag -inbox +sbcl-devel to:sbcl-devel@lists.sourceforge.net
/opt/homebrew/bin/notmuch tag +allegria from:ensemble.allegria@gmail.com
