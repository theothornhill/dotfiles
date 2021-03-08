;; -*- lexical-binding: t; -*-

(use-package emacs
  :ensure nil
  ;; TODO: Hack for windows version of emacs saving on each keystroke
  :hook ((prog-mode) . (lambda () (auto-save-mode -1)))
  :bind (("C-x r q" . 'save-buffers-kill-terminal)
         ("C-x -" . 'split-window-below)
         ("C-x |" . 'split-window-right)
         ("C-x C-b" . 'display-buffer)
         ("C-c v b" . 'view-this-buffer)
         ("C-z" . 'repeat)
         ("C-." . 'imenu)
         ("M-'" . 'bookmark-map)
         ("M-j" . 'join-line)
         ("M-o" . 'other-window)
         ("C-o" . 'open-line-above)
         ("C-M-o" . 'open-line-below)
         ("C-c e" . 'find-dotfiles-folder)
         ("C-å" . 'project-grep-dwim))
  :config
  (global-unset-key (kbd "C-x C-z"))
  (global-unset-key (kbd "C-x C-c"))
  (define-key key-translation-map (kbd "M-|") (kbd "M-&"))
  (define-key key-translation-map (kbd "M-&") (kbd "M-|"))
  (when (fboundp 'set-charset-priority)
    (set-charset-priority 'unicode))
  (prefer-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)  
  (setq auto-mode-case-fold nil)
  (setq help-window-select t)
  (fset 'yes-or-no-p 'y-or-n-p)
  (electric-pair-mode t)
  (show-paren-mode t)
  (setq compilation-scroll-output t)
  (column-number-mode)
  (line-number-mode)
  (global-prettify-symbols-mode t)
  (setq xref-search-program 'ripgrep)
  (setq-default fill-column 80
                indent-tabs-mode nil
                truncate-lines t
                scroll-step 1
                prettify-symbols-alist '(("fn" . "λ")
                                         ("lambda" . "λ")
                                         ("#+BEGIN_SRC" . "λ")
                                         ("#+END_SRC" . "λ")))
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
        initial-scratch-message ""))

(setq display-buffer-base-action '(display-buffer-same-window))
(setq display-buffer-alist
      '(("*vc-diff*" display-buffer-use-some-window (inhibit-same-window . t))
        ("*Help*" display-buffer-use-some-window (inhibit-same-window . t))
        ("*eldoc*" display-buffer-use-some-window (inhibit-same-window . t))
        ("*xref*" display-buffer-use-some-window (inhibit-same-window . t))
        ("*Completions*" display-buffer-use-some-window (inhibit-same-window . t))
        (".*" display-buffer-same-window)))

(use-package project)

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
  :hook ((dired-mode .  (lambda () (dired-hide-details-mode)))
         (dired-mode .  (lambda () (diff-hl-dired-mode)))))

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

(use-package prescient
  :config
  (setq prescient-filter-method '(fuzzy literal regexp initialism)))

(use-package selectrum-prescient
  :config
  (selectrum-prescient-toggle-fuzzy +1))

(use-package selectrum
  :config
  (selectrum-mode +1)
  (selectrum-prescient-mode +1))

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
  (setq xref-file-name-display 'project-relative)
  (add-to-list 'xref-search-program-alist
               '(ripgrep . "xargs -0 rg <C> -nH --no-messages -g '!*/' -e <R> -M 400 --max-columns-preview | sort -t: -k1,1 -k2n,2")))

(provide 'settings)
