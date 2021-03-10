;; -*- lexical-binding: t; -*-

(use-package emacs
  :ensure nil
  ;; TODO: Hack for windows version of emacs saving on each keystroke
  :hook ((prog-mode) . (lambda () (auto-save-mode -1)))
  :bind (("C-x -"   . 'split-window-below)
         ("C-x |"   . 'split-window-right)
         ("C-x C-b" . 'display-buffer)
         ("C-c v b" . 'view-this-buffer)
         ("C-z"     . 'repeat)
         ("C-."     . 'imenu)
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

(use-package icomplete
  :ensure nil
  :bind ("C-ø" . 'switch-to-completions)
  :bind (:map icomplete-fido-mode-map
              ("C-n" . 'icomplete-forward-completions)
              ("C-p" . 'icomplete-backward-completions))
  :config
  (fido-mode t)
  (setq completion-show-help nil
        completions-format 'one-column
        icomplete-compute-delay 0.0
        icomplete-show-matches-on-no-input t
        icomplete-in-buffer t))

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
        (".*" display-buffer-same-window)))

;; Devenv
(when (memq window-system '(w32))
  (setenv "PATH" (concat "C:\\Program Files\\Git\\usr\\bin" ";" (getenv "PATH")))
  (setq w32-use-visible-system-caret nil)
  (setq ediff-diff-program "C:\\Program Files\\Git\\usr\\bin\\diff.exe"))

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil)
  (setenv "PATH"
          (concat "/opt/homebrew/opt/llvm/bin:"
                  "/opt/homebrew/opt/openjdk/bin:"
                  "/Users/theodor/.nvm/versions/node/v15.9.0/bin:"
                  "/opt/homebrew/bin:"
                  "/opt/homebrew/sbin:"
                  "/usr/local/bin:"
                  "/usr/bin:"
                  "/bin:"
                  "/usr/sbin:"
                  "/sbin:"
                  "/usr/local/share/dotnet:"
                  "~/.dotnet/tools:"
                  "/Library/Apple/usr/bin:"
                  "/Users/theodor/.cargo/bin"))
  (setq exec-path
        (append exec-path
                '("/opt/homebrew/opt/llvm/bin"
                  "/opt/homebrew/opt/openjdk/bin"
                  "/Users/theodor/.nvm/versions/node/v15.9.0/bin"
                  "/opt/homebrew/bin"
                  "/opt/homebrew/sbin"
                  "/usr/local/bin"
                  "/usr/bin"
                  "/bin"
                  "/usr/sbin"
                  "/sbin"
                  "/usr/local/share/dotnet"
                  "~/.dotnet/tools"
                  "/Library/Apple/usr/bin"
                  "/Users/theodor/.cargo/bin"))))



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
         ("C-x n m" . 'notmuch))
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

(use-package eglot
  :ensure nil
  :defer t
  :bind (:map eglot-mode-map
              ("C-c f" . 'eglot-format)
              ("C-c r" . 'eglot-rename)
              ("C-c x" . 'eglot-code-actions)
              ("C-c i" . 'eglot-find-implementation)
              ("C-c d" . 'eglot-find-workspace-diagnostics))
  :hook ((elm-mode
          fsharp-mode
          csharp-tree-sitter-mode) . 'eglot-ensure)
  :config
  (setq eglot-confirm-server-initiated-edits nil)

;;; Servers
  (defclass eglot-elm (eglot-lsp-server) ()
    :documentation "A custom class for elm-language-server.")

  (defclass eglot-fsautocomplete (eglot-lsp-server) ()
    :documentation "F# FsAutoComplete langserver.")


;;; Inits
  (cl-defmethod eglot-initialization-options ((server eglot-elm))
    "Init options for elm-language-server. "
    (list
     :onlyUpdateDiagnosticsOnSave :json-false
     :elmPath ""
     :elmFormatPath ""
     :elmTestPath ""
     :disableElmLSDiagnostics :json-false
     :skipInstallPackageConfirmation t))

  (cl-defmethod eglot-initialization-options ((_server eglot-fsautocomplete))
    "Passes through required FsAutoComplete initialization options.
Don't use them, since we implement the
  `eglot-signal-fsharp-workspace-load'."
    '())

;;; Other handlers
  (defun eglot-signal-fsharp-workspace-load (server)
    (interactive (list (eglot--current-server-or-lose)))
    ;; Only run this hook if we are fsautocomplete
    (when (cl-typep server 'eglot-fsautocomplete)
      (let* ((peek (jsonrpc-request
                    server :fsharp/workspacePeek
                    (list
                     :directory (expand-file-name (project-root (project-current)))
                     :deep 10
                     :excludedDirs ["paket-files" ".git" "packages" "node_modules"])))
             (content (json-parse-string (plist-get peek :content)))
             ;; Drill down in the json
             (directory (aref (gethash "Found" (gethash "Data" content)) 1))
             ;; We want the fsprojs, so drill further
             (fsprojs (gethash "Fsprojs" (gethash "Data" directory)))
             ;; We have them, now map it over to an :url payload
             (payload (vconcat (seq-map (lambda (x) (list :uri x)) fsprojs))))
        (jsonrpc-request server :fsharp/workspaceLoad `(:textDocuments ,payload)))))

  (add-to-list 'eglot-server-initialized-hook 'eglot-signal-fsharp-workspace-load)

  ;; Add modified servers
  (add-to-list 'eglot-server-programs
               `(csharp-tree-sitter-mode . (,(expand-file-name "~/omnisharp-win-x64/OmniSharp.exe") "-lsp")))
  (add-to-list 'eglot-server-programs
               `(fsharp-mode
                 . (eglot-fsautocomplete
                    "dotnet" ,(expand-file-name "~/fsautocomplete.netcore(1)/fsautocomplete.dll")
                    "--background-service-enabled")))
  (add-to-list 'eglot-server-programs '(elm-mode . (eglot-elm "elm-language-server"))))


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
        org-agenda-files '("~/Dropbox/org")
        org-hide-emphasis-markers t
        org-use-speed-commands t
        org-refile-allow-creating-parent-nodes t
        org-refile-use-outline-path t
        org-log-done 'time
        org-default-notes-file "~/Dropbox/org/index.org"
        org-refile-targets '(("~/Dropbox/org/index.org" :maxlevel . 5))
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
    js
    python
    lisp)

  (defun find-org-index-file ()
    (interactive)
    (find-file "~/Dropbox/org/index.org")))

(provide 'settings)
