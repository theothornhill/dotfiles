;; -*- lexical-binding: t; -*-

;; Elm
(add-to-list 'load-path "~/Git/elmo")
(load "~/Git/elmo/elmo.el")
(require 'elm-mode)

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
    (add-to-list 'load-path "~/.emacs.d/elpa/slime-*")
    (slime-setup '(slime-fancy
                   slime-repl
                   slime-autodoc
                   slime-references
                   slime-company
  		             slime-asdf
                   slime-fuzzy
  		             slime-fancy-inspector
  		             slime-xref-browser)))
  
  (use-package slime-company :defer t))

;; F#
(use-package fsharp-mode
  :defer t
  :config
  (setq inferior-fsharp-program "dotnet fsi --readline-"))

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


(use-package csharp-mode
  :config
  (defun csharp-disable-clear-string-fences (orig-fun &rest args)
    "This turns off `c-clear-string-fences' for
`csharp-mode'. When on for `csharp-mode' font lock breaks after
an interpolated string or terminating simple string."
    (unless (equal major-mode 'csharp-mode)
      (apply orig-fun args))))
(advice-add 'c-clear-string-fences :around 'csharp-disable-clear-string-fences)


(use-package restclient
  :defer t
  :mode (("\\.http$" . restclient-mode)))

(use-package rg :defer t)

(provide 'progmodes)
