;; -*- lexical-binding: t; -*-

(defmacro org-block-set-languages (&rest languages)
  (declare (indent defun))
  `(org-babel-do-load-languages
    'org-babel-load-languages
    ',(mapcar (lambda (language) (cons language t))
              languages)))

(use-package org
  :defer t
  :bind (("C-c a" . 'org-agenda)
         ("C-c c" . 'org-capture)
         ("C-c l" . 'org-store-link)
         ("C-c t" . 'find-org-index-file))
  :config
  (setq org-hide-leading-stars nil
;;        org-startup-truncated nil
        org-outline-path-complete-in-steps nil
        org-agenda-files '("~/Dropbox/Notater")
        org-hide-emphasis-markers t
        org-use-speed-commands t
        org-refile-allow-creating-parent-nodes t
        org-refile-use-outline-path t
        org-log-done 'time
        org-default-notes-file "~/Dropbox/Notater/index.org"
        org-refile-targets '(("~/Dropbox/Notater/index.org" :maxlevel . 5))
        org-latex-packages-alist '(("margin=2cm" "geometry" nil))
        org-todo-keywords '((sequence "TODO" "|" "DONE"))
        org-capture-templates '(("i"
                                 "Innboks"
                                 entry
                                 (file+headline org-default-notes-file "Innboks")
                                 "* %?\n%a"))
        org-babel-clojure-backend 'cider
        org-indirect-buffer-display 'other-window)
  (org-block-set-languages
    emacs-lisp
    shell
    java
    scheme
    clojure
    js))

(defun find-org-index-file ()
  (interactive)
  (find-file "~/Dropbox/Notater/index.org"))

(provide 'org-settings)
