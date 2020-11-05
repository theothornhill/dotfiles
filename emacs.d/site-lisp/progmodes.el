;; -*- lexical-binding: t; -*-

;; Elm
(add-to-list 'load-path "~/Git/elmo")
(load "~/Git/elmo/elmo.el")
(require 'elmo)

;; F#
(add-to-list 'load-path "~/Git/fsharp-mode")
(load "~/Git/fsharp-mode/fsharp-mode.el")
(require 'fsharp-mode)

;; C#
(setq csharp-codedoc-tag-face 'font-lock-type-face)
(add-to-list 'load-path "~/Git/csharp-mode")
(load "~/Git/csharp-mode/csharp-mode.el")
(require 'csharp-mode)


;; Common lisp
(when (executable-find "sbcl")
  (cl-font-lock-built-in-mode)
  (let ((slime-helper "~/.quicklisp/slime-helper.el"))
    (when (file-exists-p slime-helper)
      (load (expand-file-name slime-helper))))

  (setq inferior-lisp-program "sbcl")

  (use-package slime
    :defer t
    :config
    (slime-setup '(slime-fancy
                   slime-repl
                   slime-autodoc
                   slime-references
                   slime-cl-indent
                   slime-company
  		   slime-asdf
                   slime-fuzzy
  		   slime-fancy-inspector
  		   slime-xref-browser)))
  
  (use-package slime-company :defer t)
  ;; (use-package sly :defer t)
  ;; (use-package sly-asdf :defer t)
  )

;; Rust
(use-package rust-mode
  :defer t
  :bind (:map rust-mode-map
              ("C-c C-c" . 'rust-run)))

;; Web dev

(use-package js
  :defer t
  :ensure nil
  :mode (("\\.tsx$" . js-mode))
  :config
  (setq js-indent-level 2
        js-jsx-indent-level 2
        js--prettify-symbols-alist nil)
  (unbind-key "M-." js-mode-map))

;; (use-package nvm
;;   :defer t
;;   :if (memq window-system '(mac ns x))
;;   :config (nvm-use "13.8.0"))

(use-package web-mode
  :defer t
  :config
  (setq web-mode-enable-auto-quoting nil
        web-mode-auto-close-style nil
        web-mode-prettify-symbols-alist nil
        web-mode-enable-css-colorization t
        web-mode-enable-auto-pairing t
        web-mode-code-indent-offset 2
        web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2))

(use-package markdown-mode :defer t)

(use-package yaml-mode :defer t)

(use-package json-mode :defer t)


;; (use-package csharp-mode
;;   :mode (("\\.cake$" . csharp-mode))
;;   :defer t
;;   :config
;;   (defun csharp-disable-clear-string-fences (orig-fun &rest args)
;;     "This turns off `c-clear-string-fences' for
;; `csharp-mode'. When on for `csharp-mode' font lock breaks after
;; an interpolated string or terminating simple string."
;;     (unless (equal major-mode 'csharp-mode)
;;       (apply orig-fun args))))
;; (advice-add 'c-clear-string-fences :around 'csharp-disable-clear-string-fences)


(use-package restclient
  :defer t
  :mode (("\\.http$" . restclient-mode)))

(use-package rg :defer t)

(use-package geiser
  :defer t
  :config
  (setq geiser-active-implementations '(guile)
        geiser-guile-binary "guile3.0"))

(use-package cc-mode
  :ensure nil
  :defer t
  :bind (("C-c m" . 'man)))

(provide 'progmodes)
