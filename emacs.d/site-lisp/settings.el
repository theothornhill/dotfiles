;; -*- lexical-binding: t; -*-

(use-package emacs
  :ensure nil
  :bind (("C-x r q" . 'save-buffers-kill-terminal)
         ("C-x -" . 'split-window-below)
         ("C-x |" . 'split-window-right)
         ("C-c v b" . 'view-this-buffer)
         ("C-." . 'imenu)
         ("M-'" . 'bookmark-map)
         ("M-j" . 'join-line)
         ("M-o" . 'other-window)
         ("C-o" . 'open-line-above)
         ("C-M-o" . 'open-line-below)
         ("C-c e" . 'find-dotfiles-folder))
  :config
  (global-unset-key (kbd "C-x C-z"))
  (global-unset-key (kbd "C-x C-c"))
  (global-unset-key (kbd "C-z"))
  (when (fboundp 'set-charset-priority)
    (set-charset-priority 'unicode))
  (prefer-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)  
  (setq auto-mode-case-fold nil)
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
                tab-width 2
                prettify-symbols-alist '(("fn" . "λ")
                                         ("lambda" . "λ")
                                         ("#+BEGIN_SRC" . "λ")
                                         ("#+END_SRC" . "λ")))
  (setq ns-use-proxy-icon nil
        ;; sentence-end-double-space t
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

;; (let ((alist `((?- . ,(regexp-opt '("-->" "-<" "-<<" "->" "->>" "-}" "-~" "-<>")))
;;                (?< . ,(regexp-opt '("<!--" "<$" "<$>" "<*" "<*>" "<+" "<+>" "<-" "<--" "<->" "</" "</>" "<<-" "<<=" "<=" "<=" "<=<" "<==" "<=>" "<>" "<|" "<|>" "<~" "<~~")))
;;                (?= . ,(regexp-opt '("=<<" "==>" "=>" "=>>" "?=" "=" "==" "===")))
;;                (?> . ,(regexp-opt '(">-" ">=" ">=>" ">>-" ">>=")))
;;                (?| . ,(regexp-opt '("|=" "|>" "||=")))
;;                (?/ . ,(regexp-opt '("/=" "/==")))
;;                (?~ . ,(regexp-opt '("~-" "~=" "~>" "~@" "~~" "~~>"))))))
;;   (dolist (char-regexp alist)
;;     (set-char-table-range composition-function-table (car char-regexp)
;;                           `([,(cdr char-regexp) 0 font-shape-gstring]))))

(use-package eldoc :ensure nil :delight)

(use-package company
  :defer t
  :delight
  :hook ((prog-mode . company-mode)
         (sly-mrepl-mode . company-mode))
  :bind (("C-x C-i" . 'company-complete)
         (:map company-active-map
               ("C-n" . company-select-next)
               ("C-p" . company-select-previous)
               ("C-d" . company-show-doc-buffer)))
  :config
  (setq company-tooltip-align-annotations t
        company-tooltip-maximum-width 70
        company-minimum-prefix-length 50
        company-idle-delay 0.2
        company-backends
        '((company-capf
           company-files
           company-keywords)
          (company-abbrev company-dabbrev))))

(use-package dired
  :ensure nil
  :bind (("C-x C-d" . 'dired))
  :hook ((dired-mode .  (lambda () (dired-hide-details-mode)))))

(use-package paredit
  :defer t
  :delight
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

(use-package multiple-cursors
  :defer t
  :bind (("C-\\" . 'mc/mark-next-like-this)
         ("C-+" . 'mc/mark-previous-like-this)
         ("C-c C-\\" . 'mc/mark-all-like-this-dwim)))

(use-package magit
  :defer t
  :bind (("C-x g" . magit-status)))

(use-package icomplete
  :ensure nil
  :bind (:map icomplete-minibuffer-map
              ("C-n" . 'icomplete-forward-completions)
              ("C-p" . 'icomplete-backward-completions))
  :config
  (fido-mode t)
  (setq icomplete-compute-delay 0.0
        icomplete-max-delay-chars 0
        icomplete-in-buffer t
        read-buffer-completion-ignore-case t
        completion-ignore-case t
        read-file-name-completion-ignore-case t
        completion-styles '(initials basic flex)
        icomplete-show-matches-on-no-input t))

(use-package icomplete-vertical
  :demand t
  :config
  (icomplete-vertical-mode))

(use-package modus-operandi-theme)

(use-package doom-themes
  :config
  (load-theme 'doom-tomorrow-night t))

(use-package yasnippet
  :delight yas-minor-mode
  :config
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets"))
  (yas-global-mode t))

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config (progn (pdf-loader-install)
                 (setq-default pdf-view-display-size 'fit-page)))

(use-package delight)

(provide 'settings)
