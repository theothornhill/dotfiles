; -*- lexical-binding: t; -*-

;; ;; Elm
;; (use-package elm-mode
;;   :defer t
;;   :straight (elm-mode :type git
;;                       :host nil
;;                       :repo "https://git.sr.ht/~theothornhill/elm-mode"
;;                       :branch "master"
;;                       :files ("elm-mode.el")))

;; ;; F#
;; (use-package fsharp-mode
;;   :defer t
;;   :straight (fsharp-mode :type git
;;                          :host nil
;;                          :repo "https://git.sr.ht/~theothornhill/fsharp-mode"
;;                          :branch "master"
;;                          :files ("fsharp-mode.el")))

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
  )

;; Common lisp
(when (executable-find "sbcl")
  ;; (cl-font-lock-built-in-mode)
  (let ((slime-helper "~/.quicklisp/slime-helper.el"))
    (when (file-exists-p slime-helper)
      (load (expand-file-name slime-helper))))

  (setq inferior-lisp-program "sbcl")

  ;; (use-package slime-company :defer t)

  (use-package slime
    :defer t
    :config
    (slime-setup '(slime-fancy
                   slime-repl
                   slime-autodoc
                   slime-references
                   slime-cl-indent
                   ;; slime-company
        	   slime-asdf
                   slime-fuzzy
        	   slime-fancy-inspector
        	   slime-xref-browser)))
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

(defun find-restclient-file ()
  (interactive)
  (find-file "~/Frende/test.http"))

(use-package restclient
  :defer t
  :mode (("\\.http$" . restclient-mode))
  :bind (("C-c r" . 'find-restclient-file)))

(provide 'progmodes)
