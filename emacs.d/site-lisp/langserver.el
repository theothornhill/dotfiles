;; -*- lexical-binding: t; -*-

(use-package flycheck :defer t
  :bind (:map flycheck-mode-map
              ("M-n" . 'flycheck-next-error)
              ("M-p" . 'flycheck-previous-error)))

(use-package flymake
  :defer t
  :config (setq flymake-no-changes-timeout 0.1)
  :bind (:map flymake-mode-map
              ("M-n" . 'flymake-goto-next-error)
              ("M-p" . 'flymake-goto-prev-error)))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "M-i"
        lsp-enable-symbol-highlighting nil
        lsp-semantic-tokens-enable t
        lsp-enable-semantic-highlighting t
        lsp-headerline-breadcrumb-enable nil)
  :hook ((js-mode
          web-mode
          typescript-mode
          elm-mode
          json-mode
          rust-mode
          csharp-tree-sitter-mode
          fsharp-mode) . lsp)
  :commands lsp
  :bind (:map lsp-mode-map
              ("C-." . 'lsp-ui-doc-show)
              ("M-i c" . 'lsp-ui-flycheck-list))
  :config
  (setq lsp-eslint-auto-fix-on-save t
        lsp-idle-delay 0.25
        lsp-clients-typescript-javascript-server-args '("--stdio" "--tsserver-log-file" "/dev/stderr")
        ;; lsp-elm-only-update-diagnostics-on-save t
        read-process-output-max (* 1024 1024 10)
        lsp-rust-analyzer-server-display-inlay-hints t))

(use-package lsp-ui
  :defer t
  :config
  (setq lsp-ui-sideline-enable nil
        lsp-ui-doc-max-height 15
        lsp-ui-doc-enable nil
        lsp-ui-doc-position 'at-point))

(use-package lsp-mssql
  :defer t
  :hook ((sql-mode) . (lambda () (require 'lsp-mssql)))
  :bind (:map sql-mode-map
              ("C-c C-c" . 'lsp-mssql-execute-buffer))
  :config
  (setq lsp-mssql-connections
      [(:server "Z63OS2SSQ13-T\\B104170"
                :database ""
                :authenticationType "Integrated"
                :password "")]))

(defun lsp-format-hook ()
  "Format buffer when using the provided major modes."
  (when (eq major-mode 'elm-mode)
    (lsp-format-buffer)))

(add-hook 'before-save-hook 'lsp-format-hook)

(provide 'langserver)
