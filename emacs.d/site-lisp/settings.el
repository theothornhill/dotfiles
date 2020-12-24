;; -*- lexical-binding: t; -*-

(use-package emacs
  :straight nil
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
  (setq-default fill-column 80
                indent-tabs-mode nil
                truncate-lines t
                scroll-step 1
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

(defun buffer-mode (buffer-or-string)
  (with-current-buffer buffer-or-string
    major-mode))

(setq display-buffer-alist
      `(("\\*help"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . right)
         (window-width . 80))
        (,(lambda (buffer action)
            (eql (buffer-mode buffer) 'dired-mode))
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . left)
         (mode . 'dired-mode)
         (window-width . 0.20))))

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

(use-package eldoc :straight nil :delight)

;; (use-package company
;;   :defer t
;;   :straight t
;;   :delight
;;   :hook ((prog-mode . company-mode)
;;          (sly-mrepl-mode . company-mode))
;;   :bind (("C-x C-i" . 'company-complete)
;;          (:map company-active-map
;;                ("C-n" . company-select-next)
;;                ("C-p" . company-select-previous)
;;                ("C-d" . company-show-doc-buffer)))
;;   :config
;;   (setq company-tooltip-align-annotations t
;;         company-minimum-prefix-length 1
;;         company-tooltip-maximum-width 70
;;         company-idle-delay 0.2
;;         ;; company-backends
;;         ;; '((company-capf
;;         ;;    company-files
;;         ;;    company-keywords)
;;         ;;   (company-abbrev company-dabbrev))
;;         ))

(use-package project)

(use-package dired
  :straight nil
  :bind (("C-x C-d" . 'dired))
  :hook ((dired-mode .  (lambda () (dired-hide-details-mode)))))

(use-package doom-themes
  :config (load-theme 'doom-tomorrow-night t))

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

(use-package ssh-agency :defer t)

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

;; (use-package icomplete
;;   :straight nil
;;   :bind (:map icomplete-minibuffer-map
;;               ("C-n" . 'icomplete-forward-completions)
;;               ("C-p" . 'icomplete-backward-completions))
;;   :config
;;   (fido-mode t)
;;   (setq icomplete-compute-delay 0.0
;;         icomplete-max-delay-chars 0
;;         icomplete-in-buffer t
;;         read-buffer-completion-ignore-case t
;;         completion-ignore-case t
;;         read-file-name-completion-ignore-case t
;; ;;        completion-styles '(initials basic flex)
;;         icomplete-show-matches-on-no-input t))

(use-package icomplete-vertical
  :demand t
  :straight t
  :config
  (icomplete-vertical-mode))

(use-package yasnippet
  :delight yas-minor-mode
  :config
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets"))
  (yas-global-mode t))

(use-package pdf-tools
  :straight nil
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :defer t
  :config (progn (pdf-loader-install)
                 (setq-default pdf-view-display-size 'fit-page)))

(use-package delight)

(provide 'settings)
