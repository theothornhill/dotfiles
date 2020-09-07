;; -*- lexical-binding: t; -*-

(when (memq window-system '(w32))
  (setenv "PATH" (concat "C:\\Program Files\\Git\\usr\\bin" ";" (getenv "PATH")))
  (setq ediff-diff-program "C:\\Program Files\\Git\\usr\\bin\\diff.exe"))

(provide 'devenv)
