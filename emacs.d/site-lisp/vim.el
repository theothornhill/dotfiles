
(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-escape
  :delight
  :config
  (evil-escape-mode))

(use-package evil-cleverparens
  :hook ((scheme-mode
          clojure-mode
          cider-repl-mode
          clojurescript-mode
          emacs-lisp-mode
          lisp-mode
          repl-mode
          sly-mrepl-mode
          slime-repl-mode
          geiser-repl-mode) . evil-cleverparens-mode))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))


(provide 'vim)
