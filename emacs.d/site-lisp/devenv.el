;; -*- lexical-binding: t; -*-

(when (memq window-system '(w32))
  (setenv "PATH" (concat "C:\\Program Files\\Git\\usr\\bin" ";" (getenv "PATH")))
  (setq w32-use-visible-system-caret nil)
  (setq ediff-diff-program "C:\\Program Files\\Git\\usr\\bin\\diff.exe")
  (setq magit-git-executable "C:\\Program Files\\Git\\bin\\git.exe"))

(provide 'devenv)
