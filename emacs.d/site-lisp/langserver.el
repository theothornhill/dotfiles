;; -*- lexical-binding: t; -*-

(use-package flycheck :defer t
  :config
  (setq flycheck-check-syntax-automatically '(save))
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
  (setq lsp-keymap-prefix "M-i")
  :hook ((js-mode
          web-mode
          typescript-mode
          elm-mode
          json-mode
          rust-mode
          csharp-mode
          fsharp-mode) . lsp)
  :commands lsp
  :bind (:map lsp-mode-map
              ("C-h ." . 'lsp-describe-thing-at-point)
              ("C-." . 'lsp-ui-imenu)
              ("M-i M-i" . 'lsp-format-buffer))
  :config
  (setq lsp-eslint-auto-fix-on-save t
        lsp-prefer-capf t
        read-process-output-max (* 1024 1024 10)
        lsp-elm-elm-analyse-trigger "save"
        max-specpdl-size 5000
        max-lisp-eval-depth 5000
        lsp-clients-typescript-server-args '("--stdio" "--tsserver-log-file" "/tmp/.log")
        lsp-eslint-server-command
        '("node"
          "c:/Users/theot/.vscode/extensions/dbaeumer.vscode-eslint-2.1.1/server/out/eslintServer.js"
          "--stdio")
        lsp-eslint-validate ["javascript" "javascriptreact" "typescript" "typescriptreact"]))

(use-package lsp-ui
  :defer t
  :config
  (setq lsp-ui-sideline-enable nil
        lsp-ui-doc-max-height 15
        lsp-ui-doc-enable nil))

(provide 'langserver)
