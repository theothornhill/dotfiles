;; -*- lexical-binding: t; -*-

(cd "~/")
(blink-cursor-mode -1)

(set-face-attribute 'default nil :font "JetBrains Mono-15")

;; Initialize packages
(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)
  (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-enable-imenu-support t))

(eval-when-compile (require 'use-package))

(use-package benchmark-init)

;;; Keep custom file in a different spot
(setq custom-file (concat user-emacs-directory "custom.el"))

(defun safe-require (package)
  (condition-case err (require package)
    ((debug error) (message "%s" (error-message-string err)))))

(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))

(mapc #'safe-require
      '(;; vim
        defuns devenv langserver notmuch-settings org-settings
         progmodes settings))

(mapc (lambda (feature) (put feature 'disabled nil)) 
      (list 'upcase-region
            'narrow-to-region
            'downcase-region
            'scroll-left
            'set-goal-column))
