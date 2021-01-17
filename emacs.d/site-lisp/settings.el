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


(setq display-buffer-base-action '(display-buffer-use-least-recent-window))

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
  :ensure nil
  :bind (("C-x C-d" . 'dired))
  :hook ((dired-mode .  (lambda () (dired-hide-details-mode)))))

(use-package doom-themes
  :config (load-theme 'doom-tomorrow-night t))

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

(use-package yasnippet
  :config
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets"))
  (yas-global-mode t))

(provide 'settings)
