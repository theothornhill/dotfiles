; -*- lexical-binding: t; -*-

(cd "~/")

(blink-cursor-mode -1)

(if (member window-system '(ns))
    (set-face-attribute 'default nil :font "JetBrains Mono-14")
  (set-face-attribute 'default nil :font "iosevka ss14-12"))

(unless (bound-and-true-p package--initialized)
  (require 'package)
  (setq package-archives
        '(("gnu" . "http://elpa.gnu.org/packages/")
          ("nongnu" . "https://elpa.nongnu.org/nongnu/")
          ("melpa" . "https://melpa.org/packages/")))
  (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-enable-imenu-support t))

(eval-when-compile (require 'use-package))

;;; Keep custom file in a different spot
(setq custom-file (concat user-emacs-directory "custom.el"))

(defmacro site-lisp (name &rest args)
  (declare (indent defun))
  `(use-package ,name
     :ensure nil
     :load-path "site-lisp"
     ,@args))

(site-lisp defuns)
(site-lisp devenv)
(site-lisp progmodes)
(site-lisp notmuch-settings)
(site-lisp org-settings)
(site-lisp settings)
(site-lisp eglot-extensions)
(site-lisp themodor :config (enable-theme 'themodor))

(mapc (lambda (feature) (put feature 'disabled nil)) 
      (list 'upcase-region
            'narrow-to-region
            'downcase-region
            'scroll-left
            'set-goal-column))
