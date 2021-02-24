; -*- lexical-binding: t; -*-

(use-package elm-mode
  :load-path "~/Git/real-elm-mode"
  :init
  (setq elm-mode-indent-mode 'elm-indent-simple-mode))

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

(use-package sharper
  :demand t)

;; C#
(use-package csharp-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-tree-sitter-mode))
  :mode (("\\.csproj$" . nxml-mode)
         ("\\.cake$" . csharp-tree-sitter-mode))
  :bind (:map csharp-tree-sitter-mode-map
              ("C-c n" . sharper-main-transient)))

;; Common lisp
(use-package sly
  :defer t
  :config
  (cl-font-lock-built-in-mode)
  (setq inferior-lisp-program "sbcl"))

;; Rust
(use-package rust-mode :defer t)

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

(use-package cider :defer t)

(provide 'progmodes)
