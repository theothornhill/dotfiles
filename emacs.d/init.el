;; -*- lexical-binding: t; -*-

(cd "~/")

(blink-cursor-mode -1)

(set-face-attribute 'default nil :font "JetBrains Mono-14")

(setq straight-use-package-by-default t
      ;; straight-disable-autoloads t
      )

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; ;; Initialize packages
;; (unless (bound-and-true-p package--initialized)
;;   (setq package-enable-at-startup nil)
;;   (package-initialize))

;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))

;; (eval-and-compile
;;   (setq use-package-always-ensure t)
;;   (setq use-package-enable-imenu-support t)
;;   )

(straight-use-package 'use-package)
;; (eval-when-compile (require 'use-package))

(use-package benchmark-init)

;;; Keep custom file in a different spot
(setq custom-file (concat user-emacs-directory "custom.el"))

(defun safe-require (package)
  (condition-case err (require package)
    ((debug error) (message "%s" (error-message-string err)))))

(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))

(mapc #'safe-require
      '(defuns devenv langserver ;;notmuch-settings
	 org-settings progmodes settings))

(mapc (lambda (feature) (put feature 'disabled nil)) 
      (list 'upcase-region
            'narrow-to-region
            'downcase-region
            'scroll-left
            'set-goal-column))
