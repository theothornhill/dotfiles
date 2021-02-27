; -*- lexical-binding: t; -*-

(use-package elm-mode
  :config
  (setq elm-mode-hook '(elm-indent-simple-mode)
        elm-format-on-save nil))

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

(use-package deadgrep
  :defer t
  :bind (("C-å" . 'deadgrep-without-asking)))

(provide 'progmodes)
