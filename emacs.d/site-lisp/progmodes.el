; -*- lexical-binding: t; -*-

(use-package elm-mode
  :load-path "~/Git/elm-mode")

(use-package fsharp-mode
  :load-path "~/Git/fsharp-mode"
  :config
  (setq fsharp-mode-format-on-save t))

(use-package tree-sitter
  :defer t)

(use-package tree-sitter-langs
  :defer t)

(use-package tree-sitter-indent
  :defer t)

;; C#
(use-package csharp-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-tree-sitter-mode))
  :mode (("\\.csproj$" . nxml-mode)
         ("\\.cake$" . csharp-tree-sitter-mode))
  :bind (:map csharp-tree-sitter-mode-map
              ("C-c t" . 'dotnet-run-test-at-point)))

;; Common lisp
(use-package sly
  :defer t
  :pin nongnu
  :config
  (cl-font-lock-built-in-mode)
  (setq inferior-lisp-program "sbcl"))

(use-package js
  :defer t
  :ensure nil
  :mode (("\\.tsx$" . js-mode))
  :config
  (setq js-indent-level 2
        js-jsx-indent-level 2
        js--prettify-symbols-alist nil)
  (unbind-key "M-." js-mode-map))

(use-package markdown-mode :defer t)

(use-package json-mode :defer t :pin gnu)

(use-package flymake
  :bind (:map flymake-mode-map
              ("M-n" . 'flymake-goto-next-error)
              ("M-p" . 'flymake-goto-prev-error)))

(provide 'progmodes)
