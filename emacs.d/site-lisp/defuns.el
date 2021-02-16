;; -*- lexical-binding: t; -*-

(defun fetch-mail ()
  (interactive)
  (async-shell-command
   "mbsync -a && ~/dotfiles/notmuch.sh"))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (open-line 1))

(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline))

(defun find-dotfiles-folder ()
  (interactive)
  (dired user-emacs-directory))

(defun view-this-buffer ()
  (interactive)
  (view-buffer (current-buffer)))

(defun ngw ()
  (interactive)
  (project-shell)
  (insert "npx gulp watch"))

(defun dotnet-run-test-at-point ()
  (interactive)
  (project-compile (format "dotnet test --filter %s" (symbol-at-point))))

(provide 'defuns)
