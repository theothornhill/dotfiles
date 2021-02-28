;; -*- lexical-binding: t; -*-

(use-package eglot
  :load-path "~/Git/theglot"
  :bind (:map eglot-mode-map
              ("C-c f" . 'eglot-format)
              ("C-c r" . 'eglot-rename)
              ("C-c h" . 'eldoc))
  :hook ((elm-mode
          fsharp-mode
          csharp-tree-sitter-mode) . 'eglot-ensure)
  :config
  (setq max-mini-window-height 4))

;;; Servers
(defclass eglot-elm (eglot-lsp-server) ()
  :documentation "A custom class for elm-language-server.")

(defclass eglot-fsautocomplete (eglot-lsp-server) ()
  :documentation "F# FsAutoComplete langserver.")


;;; Inits
(cl-defmethod eglot-initialization-options ((server eglot-elm))
  "Init options for elm-language-server.  

Since the parser is so flaky, only update diagnostics on save."
'(:onlyUpdateDiagnosticsOnSave t))

(cl-defmethod eglot-initialization-options ((_server eglot-fsautocomplete))
  "Passes through required FsAutoComplete initialization options.
Don't use them, since we implement the
  `eglot-signal-fsharp-workspace-load'."
  '())


;;; Other handlers
(defun eglot-signal-fsharp-workspace-load (server)
  (interactive (list (eglot--current-server-or-lose)))
  ;; Only run this hook if we are fsautocomplete
  (when (cl-typep server 'eglot-fsautocomplete)
    (let* ((peek (jsonrpc-request
                  server :fsharp/workspacePeek
                  (list
                   :directory (expand-file-name (project-root (project-current)))
                   :deep 10
                   :excludedDirs ["paket-files" ".git" "packages" "node_modules"])))
           (content (json-parse-string (plist-get peek :content)))
           ;; Drill down in the json
           (directory (aref (gethash "Found" (gethash "Data" content)) 1))
           ;; We want the fsprojs, so drill further
           (fsprojs (gethash "Fsprojs" (gethash "Data" directory)))
           ;; We have them, now map it over to an :url payload
           (payload (vconcat (seq-map (lambda (x) (list :uri x)) fsprojs))))
      (jsonrpc-request server :fsharp/workspaceLoad `(:textDocuments ,payload)))))

(add-to-list 'eglot-server-initialized-hook 'eglot-signal-fsharp-workspace-load)

;; Add modified servers
(add-to-list 'eglot-server-programs
             `(csharp-tree-sitter-mode . (,(expand-file-name "~/omnisharp-win-x64/OmniSharp.exe") "-lsp")))
(add-to-list 'eglot-server-programs
             `(fsharp-mode
               . (eglot-fsautocomplete
                  "dotnet" ,(expand-file-name "~/fsautocomplete.netcore(1)/fsautocomplete.dll")
                  "--background-service-enabled")))
(add-to-list 'eglot-server-programs '(elm-mode . (eglot-elm "elm-language-server")))


(provide 'eglot-extensions)
