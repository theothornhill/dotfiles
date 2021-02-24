; -*- lexical-binding: t; -*-

(cd "~/")

(blink-cursor-mode -1)

(if (member window-system '(ns))
    (set-face-attribute 'default nil :font "JetBrains Mono-14")
  (set-face-attribute 'default nil :font "iosevka ss14-12"))


;; Initialize packages
(unless (bound-and-true-p package--initialized)
  (require 'package)
  (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                           ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                           ("melpa" . "https://melpa.org/packages/")))
  ;; (setq package-check-signature nil)
  (package-initialize)
  )

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-enable-imenu-support t)
  )

(eval-when-compile (require 'use-package))

;;; Keep custom file in a different spot
(setq custom-file (concat user-emacs-directory "custom.el"))

(defun safe-require (package)
  (condition-case err (require package)
    ((debug error) (message "%s" (error-message-string err)))))

(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))

(mapc #'safe-require
      '(defuns devenv progmodes ercodor notmuch-settings
	 org-settings settings langserver))

(load (concat user-emacs-directory "site-lisp/themodor.el"))
(enable-theme 'themodor)

(mapc (lambda (feature) (put feature 'disabled nil)) 
      (list 'upcase-region
            'narrow-to-region
            'downcase-region
            'scroll-left
            'set-goal-column))
