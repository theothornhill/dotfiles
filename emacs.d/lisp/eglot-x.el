;; -*- lexical-binding: t; -*-

(use-package eglot
  :ensure nil
  :defer t
  :bind (:map eglot-mode-map
              ("C-c f" . 'eglot-format)
              ("C-c r" . 'eglot-rename)
              ("C-c x" . 'eglot-code-actions)
              ("C-c i" . 'eglot-find-implementation)
              ("C-c d" . 'eglot-find-workspace-diagnostics))
  :hook ((elm-mode
          fsharp-mode
          csharp-mode
          csharp-tree-sitter-mode) . 'eglot-ensure)
  :config
  (setq eglot-confirm-server-initiated-edits nil))

(require 'eglot)

;;; Servers
(defclass eglot-elm (eglot-lsp-server) ()
  :documentation "A custom class for elm-language-server.")

(defclass eglot-fsautocomplete (eglot-lsp-server) ()
  :documentation "F# FsAutoComplete langserver.")

(defclass eglot-omnisharp (eglot-lsp-server) ()
  :documentation "OmniSharp server")

;;; Inits
(cl-defmethod eglot-initialization-options ((server eglot-elm))
  "Init options for elm-language-server. "
  (list
   :onlyUpdateDiagnosticsOnSave :json-false
   :elmPath ""
   :elmFormatPath ""
   :elmTestPath ""
   :disableElmLSDiagnostics :json-false
   :skipInstallPackageConfirmation t))

(cl-defmethod eglot-initialization-options ((_server eglot-fsautocomplete))
  "Passes through required FsAutoComplete initialization options.
Don't use them, since we implement the
  `eglot-signal-fsharp-workspace-load'."
  '())

(cl-defmethod eglot-initialization-options ((_server eglot-omnisharp))
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
             `(csharp-mode
               . (eglot-omnisharp
                  ;; Hacked version of this file that runs mono from brew 
                  ,(expand-file-name "~/LSP/omnisharp-roslyn/v1.37.7/run")
                  "-lsp")))

(add-to-list 'eglot-server-programs
             `(fsharp-mode
               . (eglot-fsautocomplete
                  "dotnet"
                  ,(expand-file-name "~/LSP/FsAutoComplete/bin/release_netcore/fsautocomplete.dll")
                  "--background-service-enabled")))

(add-to-list 'eglot-server-programs '(elm-mode . (eglot-elm "elm-language-server")))
(provide 'eglot-x)
;;; eglot-x.el ends here
