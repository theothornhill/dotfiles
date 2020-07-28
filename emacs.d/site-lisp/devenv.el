;; -*- lexical-binding: t; -*-

(when (memq window-system '(w32))
  (setenv "PATH" (concat "C:\\Program Files\\Git\\usr\\bin" ";" (getenv "PATH")))
  (setq explicit-shell-file-name
        "c:/Program Files/Git/bin/bash.exe"))

(provide 'devenv)
