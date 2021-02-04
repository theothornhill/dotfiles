; -*- lexical-binding: t; -*-

(ignore-errors
  (add-to-list 'load-path "~/Git/elm-mode")
  (require 'elm-mode))

(ignore-errors
  (add-to-list 'load-path "~/Git/fsharp-mode")
  (require 'fsharp-mode))

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
  )

;; Common lisp
(when (executable-find "sbcl")
  ;; (cl-font-lock-built-in-mode)
  ;; (let ((slime-helper "~/quicklisp/slime-helper.el"))
  ;;   (when (file-exists-p slime-helper)
  ;;     (load (expand-file-name slime-helper))))

  (setq inferior-lisp-program "sbcl")

  ;; (use-package slime-company :defer t)

  ;; (use-package slime
  ;;   :defer t
  ;;   :config
  ;;   (slime-setup '(slime-fancy
  ;;                  slime-repl
  ;;                  slime-autodoc
  ;;                  slime-references
  ;;                  slime-cl-indent
  ;;                  ;; slime-company
  ;;       	   slime-asdf
  ;;                  slime-fuzzy
  ;;       	   slime-fancy-inspector
  ;;       	   slime-xref-browser)))

  (use-package sly :defer t
    :config
    (setq sly-complete-symbol-function #'sly-simple-completions))

  (use-package sly-asdf :defer t
    :config
    (add-to-list 'sly-contribs 'sly-asdf 'append))

  )

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
