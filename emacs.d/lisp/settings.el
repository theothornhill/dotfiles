;; -*- lexical-binding: t; -*-

(use-package emacs
  :ensure nil
  ;; TODO: Hack for windows version of emacs saving on each keystroke
  :hook ((prog-mode) . (lambda () (auto-save-mode -1)))
  :bind (("C-x -"   . 'split-window-below)
         ("C-x |"   . 'split-window-right)
         ("C-x ~"   . 'cd)
         ("C-x C-b" . 'display-buffer)
         ("C-c v b" . 'view-this-buffer)
         ("C-z"     . 'repeat)
         ("C-."     . 'imenu)
         ("C-c C-r" . 'query-replace-regexp)
         ("M-'"     . 'bookmark-map)
         ("M-j"     . 'join-line)
         ("M-o"     . 'other-window)
         ("C-o"     . 'open-line-above)
         ("C-M-o"   . 'open-line-below)
         ("C-c e"   . 'find-library)
         ("C-å"     . 'project-grep-dwim))
  :config
  (define-key key-translation-map (kbd "M-|") (kbd "M-&"))
  (define-key key-translation-map (kbd "M-&") (kbd "M-|"))
  (when (fboundp 'set-charset-priority)
    (set-charset-priority 'unicode))
  (prefer-coding-system 'utf-8)
  (fset 'yes-or-no-p 'y-or-n-p)
  (electric-pair-mode t)
  (show-paren-mode t)
  (column-number-mode)
  (line-number-mode)
  (global-prettify-symbols-mode t)
  (setq-default fill-column 80
                indent-tabs-mode nil
                truncate-lines t
                scroll-step 1
                prettify-symbols-alist '(("lambda" . "λ")))
  (setq ns-use-proxy-icon nil
        frame-title-format nil
        backup-inhibited t
        inhibit-startup-message t
        x-select-enable-clipboard t
        require-final-newline t
        calendar-week-start-day 1
        split-height-threshold 100
        split-width-threshold 80
        ring-bell-function 'ignore
        initial-scratch-message ""
        locale-coding-system 'utf-8  
        auto-mode-case-fold nil
        help-window-select t
        compilation-scroll-output t))

(use-package helm
  :bind (("C-." . 'helm-imenu)
         ("C-x C-f" . 'helm-find-files)
         ("M-x" . 'helm-M-x))
  :config
  (helm-mode 1)
  (setq completion-styles '(flex)))

;; Display buffer madness
(setq display-buffer-base-action '(display-buffer-same-window))
(setq display-buffer-alist
      `((,(regexp-opt '("*vc-diff*"
                        "*Help*"
                        "*Metahelp*"
                        "*eldoc*"
                        "*xref*"))
         display-buffer-use-some-window (inhibit-same-window . t))
        ("*Completions*"
         (display-buffer-in-side-window)
         (side . right)
         (width . 1))
        ("*helm-mode-completion-at-point*"
         (display-buffer-in-side-window)
         (side . bottom)
         (height . 1.5))
        (".*" display-buffer-same-window)))

;;; Windows
(when (memq window-system '(w32))
  (setenv "PATH" (concat "C:\\Program Files\\Git\\usr\\bin" ";" (getenv "PATH")))
  (setq w32-use-visible-system-caret nil)
  (setq ediff-diff-program "C:\\Program Files\\Git\\usr\\bin\\diff.exe"))

;;; Mac
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil))

(when (memq window-system '(mac ns))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)) ; nil for dark text
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

(use-package project :defer t)

(use-package eldoc
  :ensure nil
  :config
  (setq eldoc-echo-area-use-multiline-p nil))

(use-package diff-hl
  :init
  (global-diff-hl-mode))

(use-package dired
  :ensure nil
  :bind (("C-x C-d" . 'dired))
  :hook ((dired-mode . (lambda () (dired-hide-details-mode)))
         (dired-mode . (lambda () (diff-hl-dired-mode)))
         (dired-mode . (lambda () (turn-on-gnus-dired-mode)))))

(use-package paredit
  :hook ((scheme-mode
          clojure-mode
          cider-repl-mode
          clojurescript-mode
          emacs-lisp-mode
          lisp-mode
          repl-mode
          sly-mrepl-mode
          slime-repl-mode
          geiser-repl-mode) . paredit-mode))

(use-package yasnippet
  :config
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets"))
  (yas-global-mode t))

(defun ercodor-start ()
  (interactive)
  (load "~/.emacs.d/erc-auth.el")
  (erc :server "irc.freenode.net"
       :port "6667"
       :nick "theothornhill"
       :password erc-pass))

(setq erc-autojoin-channels-alist '(("freenode.net" "#sr.ht" "#lisp")))

(use-package xref
  :ensure nil
  :config
  (setq xref-file-name-display 'project-relative
        xref-search-program 'ripgrep)
  (add-to-list 'xref-search-program-alist
               '(ripgrep . "xargs -0 rg <C> -nH --no-messages -g '!*/' -e <R> -M 400 --max-columns-preview | sort -t: -k1,1 -k2n,2")))

(use-package elm-mode
  :defer t
  :ensure nil)

(use-package fsharp-mode
  :defer t
  :ensure nil
  :config
  (setq fsharp-mode-format-on-save t))

(use-package tree-sitter
  :defer t)

(use-package tree-sitter-langs
  :defer t)

(use-package tree-sitter-indent
  :ensure nil
  :defer t)

(use-package csharp-mode
  :defer t
  :mode (("\\.csproj$" . nxml-mode)
         ("\\.cake$" . csharp-tree-sitter-mode)
         ("\\.cs\\'" . csharp-tree-sitter-mode))
  :bind (:map csharp-tree-sitter-mode-map
              ("C-c t" . 'dotnet-run-test-at-point)))

(use-package sly
  :defer t
  :pin nongnu
  :config
  (cl-font-lock-built-in-mode)
  (setq inferior-lisp-program "sbcl"
        sly-symbol-completion-mode nil))

(use-package js
  :defer t
  :ensure nil
  :mode (("\\.tsx$" . js-mode))
  :config
  (setq js-indent-level 2
        js-jsx-indent-level 2
        js--prettify-symbols-alist nil)
  (unbind-key "M-." js-mode-map))

(use-package markdown-mode :defer t :pin nongnu)

(use-package json-mode :defer t :pin gnu)

(use-package flymake
  :defer t
  :ensure nil
  :bind (:map flymake-mode-map
              ("M-n" . 'flymake-goto-next-error)
              ("M-p" . 'flymake-goto-prev-error)))

(use-package notmuch
  :if (executable-find "notmuch")
  :defer t
  :bind (("C-x m" . 'notmuch-mua-new-mail)
         ("C-x n m" . 'notmuch)
         ("C-x n m" . 'notmuch))
  :bind (:map notmuch-message-mode-map
              ("C-M-i" . 'complete-symbol))
  :config
  (setq message-sendmail-f-is-evil nil
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
        notmuch-fcc-dirs '(("theo@thornhill.no" . "main/Sent +sent -inbox -unread")
                           ("theodor.thornhill@frende.no" . "\"frende/Sendte elementer\" +sent -inbox -unread"))
        notmuch-saved-searches '((:name "inbox" :query "tag:inbox" :key "i" :sort-order newest-first)
                                 (:name "unread" :query "tag:unread" :key "u" :sort-order newest-first)
                                 (:name "flagged" :query "tag:flagged" :key "f" :sort-order newest-first)
                                 (:name "sent" :query "tag:sent" :key "s" :sort-order newest-first)
                                 (:name "drafts" :query "tag:draft" :key "d" :sort-order newest-first)
                                 (:name "emacs" :query "to:emacs-devel@gnu.org OR to:*@debbugs.gnu.org" :key "e" :sort-order newest-first)
                                 (:name "bugs" :query "tag:bugs" :key "b" :sort-order newest-first)
                                 (:name "sbcl-devel" :query "tag:sbcl-devel" :key "l" :sort-order newest-first)
                                 (:name "all mail" :query "*" :key "a" :sort-order newest-first)
                                 (:name "work" :query "to:theodor.thornhill@frende.no OR from:*@frende.no" :key "w" :sort-order newest-first))))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (open-line 1))

(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline))

(defun view-this-buffer ()
  (interactive)
  (view-buffer (current-buffer)))

(defun ngw ()
  (interactive)
  (project-shell)
  (insert "npx gulp watch"))

(defun dotnet-run-test-at-point ()
  (interactive)
  (project-compile (format "dotnet test --filter %s" (symbol-at-point))))

(defun project-grep-dwim ()
  (interactive)
  (project-find-regexp (thing-at-point 'symbol)))

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
         ("C-c l" . 'org-store-link))
  :config
  (setq org-hide-leading-stars nil
        org-outline-path-complete-in-steps nil
        org-hide-emphasis-markers t
        org-use-speed-commands t
        org-refile-allow-creating-parent-nodes t
        org-refile-use-outline-path t
        org-log-done 'time
        org-latex-packages-alist '(("margin=2cm" "geometry" nil))
        org-todo-keywords '((sequence "TODO" "|" "DONE"))
        org-capture-templates '(("i"
                                 "Innboks"
                                 entry
                                 (file+headline org-default-notes-file "Innboks")
                                 "* %?\n%a"))
        org-indirect-buffer-display 'other-window)
  (org-block-set-languages
    emacs-lisp
    shell
    java
    scheme
    clojure
    js
    python
    lisp))

(provide 'settings)

;;; settings.el ends here
