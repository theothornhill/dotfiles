; -*- lexical-binding: t; -*-

(cd "~/")

(blink-cursor-mode -1)

(if (eq system-type 'darwin)
    (add-to-list 'default-frame-alist '(font . "JetBrains Mono-14"))
  (add-to-list 'default-frame-alist '(font . "JetBrains Mono-12")))

(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-enable-imenu-support t))

(eval-when-compile (require 'use-package))

;;; Keep custom file in a different spot
(setq custom-file (concat user-emacs-directory "custom.el"))

(use-package settings
  :ensure nil
  :load-path "lisp")

(use-package themodor
  :ensure nil
  :load-path "lisp")

(use-package themodor-dark
  :ensure nil
  :load-path "lisp"
  :config (enable-theme 'themodor-dark))

(use-package eglot-x
  :ensure nil
  :load-path "lisp")

(mapc (lambda (feature) (put feature 'disabled nil)) 
      (list 'upcase-region
            'narrow-to-region
            'downcase-region
            'scroll-left
            'set-goal-column))
