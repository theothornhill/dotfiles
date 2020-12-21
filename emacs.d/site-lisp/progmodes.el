; -*- lexical-binding: t; -*-

;; Elm
(use-package elm-mode
  :straight (elm-mode :type git
                      :host nil
                      :repo "https://git.sr.ht/~theothornhill/elm-mode"
                      :branch "master"
                      :files ("elm-mode.el")))

;; F#
(use-package fsharp-mode
  :straight (fsharp-mode :type git
                         :host nil
                         :repo "https://git.sr.ht/~theothornhill/fsharp-mode"
                         :branch "master"
                         :files ("fsharp-mode.el")))

(use-package tree-sitter)

(use-package tree-sitter-langs)

;; (use-package tree-sitter-indent
;;   :straight (tree-sitter-indent :type git
;;                       :repo "https://codeberg.org/theo/tree-sitter-indent.el.git"
;;                       :branch "main"
;;                       :files ("tree-sitter-indent.el")))

;; C#
(use-package csharp-mode
  :straight
  (csharp-mode :type git
               :host github
               :repo "emacs-csharp/csharp-mode"
               :branch "tree-sitter")
  :init
  (setq csharp-mode-enable-tree-sitter t))

;; Common lisp
(when (executable-find "sbcl")
  ;; (cl-font-lock-built-in-mode)
  (let ((slime-helper "~/.quicklisp/slime-helper.el"))
    (when (file-exists-p slime-helper)
      (load (expand-file-name slime-helper))))

  (setq inferior-lisp-program "sbcl")

  (use-package slime-company :defer t)

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
  ;; (use-package sly
  ;;   :defer t
  ;;   :config
  ;;   (setq sly-complete-symbol-function #'sly-simple-completions))
  ;; (require 'sly-autoloads)
  ;; (use-package sly-asdf :defer t)
  )

;; ;; Rust
;; (use-package rustic :defer t)

;; Rust
(use-package rust-mode :defer t)

;; Web dev

(use-package js
  :defer t
  :straight nil
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


(defun find-restclient-file ()
  (interactive)
  (find-file "~/Frende/test.http"))

(use-package restclient
  :defer t
  :mode (("\\.http$" . restclient-mode))
  :bind (("C-c r" . 'find-restclient-file)))

(use-package rg :defer t)

(use-package geiser
  :defer t
  :config
  (setq geiser-active-implementations '(guile)
        geiser-guile-binary "guile3.0"))

(use-package cc-mode
  :straight nil
  :defer t
  :bind (("C-c m" . 'man)))

(provide 'progmodes)
