;;; themodor.el --- Personal theme for emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Theodor Thornhill

;; Author: Theodor Thornhill <theo@thornhill.no>
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(deftheme themodor
  "The Themodor theme")

;;; Color Palette

(let* ((class '((class color) (min-colors 89)))
       (foreground "grey20")
       (background "white")
       (cursor "black")
       (border "grey90")
       (minibuffer cursor)
       (region "grey90")
       (comment-delimiter "grey85")
       (comment "grey50")
       (constant foreground)
       (string "grey40")
       (modeline-foreground foreground)
       (modeline-background "grey95")
       (modeline-foreground-inactive comment)
       (modeline-background-inactive background)
       (hl-background region)
       (hl-face-background nil)
       (failure "red")
       (org-background "grey98")
       (refine-added "#22aa22")
       (refine-changed "#aaaa22")
       (refine-removed "#aa2222")
       (match "#eeffee"))
  (setq fci-rule-color comment)
  (custom-theme-set-faces
   'themodor

   ;; basic stuff
   `(default ((,class (:background ,background :foreground ,foreground))))
   `(cursor ((,class (:background ,cursor :inverse-video t))))
   `(vertical-border ((,class (:foreground ,border))))
   `(fringe ((,class (:foreground ,foreground :background ,background))))
   `(match ((,class (:background ,match))))
   
   ;; minibuffer
   `(minibuffer-prompt ((,class (:foreground ,minibuffer :weight bold))))

   ;; region
   `(region ((,class (:background ,region))))
   `(secondary-selection ((,class (:background ,region))))

   ;; faces
   `(font-lock-builtin-face ((,class (:foreground ,foreground :weight bold))))
   `(font-lock-constant-face ((,class (:foreground ,foreground :weight bold))))
   `(font-lock-keyword-face ((,class (:foreground ,foreground :weight bold))))
   `(font-lock-type-face ((,class (:foreground ,foreground :slant italic))))
   `(font-lock-function-name-face ((,class (:foreground ,foreground :weight bold))))
   `(font-lock-variable-name-face ((,class (:foreground ,foreground))))

   `(font-lock-comment-delimiter-face ((,class (:foreground ,comment-delimiter))))
   `(font-lock-comment-face ((,class (:foreground ,comment))))
   `(font-lock-doc-face ((,class (:inherit (font-lock-comment-face)))))
   `(font-lock-string-face ((,class (:foreground ,foreground :foreground ,string))))

   ;; tree-sitter
   `(tree-sitter-hl-face:function.call ((,class (:foreground ,foreground :weight bold))))

   ;; isearch
   `(isearch ((,class (:foreground ,foreground :background ,region :weight normal))))
   `(isearch-fail ((,class (:foreground ,failure :bold t))))
   `(lazy-highlight
     ((,class (:foreground ,foreground :background ,region))))

   ;; show-paren
   `(show-paren-match
     ((,class (:background ,region))))
   `(show-paren-mismatch
     ((,class (:foreground ,failure :weight bold))))

   ;; eshell
   `(eshell-prompt ((,class (:foreground ,foreground :weight bold))))

   ;; modeline
   `(mode-line
     ((,class (:inverse-video unspecified
                              :overline ,border
                              :underline nil
                              :foreground ,modeline-foreground
                              :background ,modeline-background
                              :box (:line-width 1 :color ,background :style unspecified)
                              ))))
   `(mode-line-buffer-id ((,class (:weight bold))))
   `(mode-line-inactive
     ((,class (:inverse-video unspecified
                              :overline ,border
                              :underline nil
                              :foreground ,modeline-foreground-inactive
                              :background ,modeline-background-inactive
                              :box (:line-width 1 :color ,border :style unspecified)))))
   `(diff-refine-added      ((,class (:foreground ,refine-added))))
   `(diff-refine-changed    ((,class (:foreground ,refine-changed))))
   `(diff-refine-removed    ((,class (:foreground ,refine-removed))))

   `(helm-header
     ((,class (:foreground ,foreground
                      :background ,background
                      :underline nil
                      :box nil
                      :extend t))))
   `(helm-source-header
     ((,class (:foreground ,foreground
                      :background ,background
                      :underline nil
                      :box ,background
                      :height 1.8
                      :extend t))))
   `(helm-selection ((t (:background ,hl-background :underline nil))))
   ;; `(helm-selection-line ((t (:background ,themodor-bg+1))))
   ;; `(helm-visible-mark ((t (:foreground ,themodor-bg :background ,themodor-yellow-2))))
   ;; `(helm-candidate-number ((t (:foreground ,themodor-green+4 :background ,themodor-bg-1))))
   ;; `(helm-separator ((t (:foreground ,themodor-red :background ,themodor-bg))))
   ;; `(helm-time-zone-current ((t (:foreground ,themodor-green+2 :background ,themodor-bg))))
   ;; `(helm-time-zone-home ((t (:foreground ,themodor-red :background ,themodor-bg))))
   ;; `(helm-bookmark-addressbook ((t (:foreground ,themodor-orange :background ,themodor-bg))))
   ;; `(helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))
   ;; `(helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))
   ;; `(helm-bookmark-gnus ((t (:foreground ,themodor-magenta :background ,themodor-bg))))
   ;; `(helm-bookmark-info ((t (:foreground ,themodor-green+2 :background ,themodor-bg))))
   ;; `(helm-bookmark-man ((t (:foreground ,themodor-yellow :background ,themodor-bg))))
   ;; `(helm-bookmark-w3m ((t (:foreground ,themodor-magenta :background ,themodor-bg))))
   ;; `(helm-buffer-not-saved ((t (:foreground ,themodor-red :background ,themodor-bg))))
   ;; `(helm-buffer-process ((t (:foreground ,themodor-cyan :background ,themodor-bg))))
   ;; `(helm-buffer-saved-out ((t (:foreground ,themodor-fg :background ,themodor-bg))))
   ;; `(helm-buffer-size ((t (:foreground ,themodor-fg-1 :background ,themodor-bg))))
   `(helm-ff-directory ((t (:foreground ,foreground :background ,background :weight bold))))
   `(helm-ff-file ((,class (:foreground ,foreground :background ,background :weight normal))))
   ;; `(helm-ff-executable ((t (:foreground ,themodor-green+2 :background ,themodor-bg :weight normal))))
   ;; `(helm-ff-invalid-symlink ((t (:foreground ,themodor-red :background ,themodor-bg :weight bold))))
   ;; `(helm-ff-symlink ((t (:foreground ,themodor-yellow :background ,themodor-bg :weight bold))))
   ;; `(helm-ff-prefix ((t (:foreground ,themodor-bg :background ,themodor-yellow :weight normal))))
   `(helm-ff-file-extension ((,class :foreground ,foreground)))
   `(helm-ff-dotted-directory ((,class :inherit bold :background ,background :foreground ,foreground)))
   ;; `(helm-ff-dotted-symlink-directory ((,class :inherit (button helm-ff-dotted-directory))))
   ;; `(helm-grep-cmd-line ((t (:foreground ,themodor-cyan :background ,themodor-bg))))
   ;; `(helm-grep-file ((t (:foreground ,themodor-fg :background ,themodor-bg))))
   ;; `(helm-grep-finish ((t (:foreground ,themodor-green+2 :background ,themodor-bg))))
   ;; `(helm-grep-lineno ((t (:foreground ,themodor-fg-1 :background ,themodor-bg))))
   ;; `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
   ;; `(helm-grep-running ((t (:foreground ,themodor-red :background ,themodor-bg))))
   ;; `(helm-match ((t (:foreground ,themodor-orange :background ,themodor-bg-1 :weight bold))))
   ;; `(helm-moccur-buffer ((t (:foreground ,themodor-cyan :background ,themodor-bg))))
   ;; `(helm-mu-contacts-address-face ((t (:foreground ,themodor-fg-1 :background ,themodor-bg))))
   ;; `(helm-mu-contacts-name-face ((t (:foreground ,themodor-fg :background ,themodor-bg))))

   ;; erc
   `(erc-notice-face ((,class (:inherit ,font-lock-comment-face))))
   `(erc-timestamp-face ((,class (:inherit ,font-lock-comment-face))))

   ;; Markdown
   `(markdown-code-face ((,class nil)))

   ;; org-mode
   `(org-level-1 ((,class (:foreground ,foreground :height 1.6))))
   `(org-level-2 ((,class (:foreground ,foreground :height 1.5))))
   `(org-level-3 ((,class (:foreground ,foreground :height 1.4))))
   `(org-level-4 ((,class (:foreground ,foreground :height 1.3))))
   `(org-level-5 ((,class (:foreground ,foreground :height 1.2))))
   `(org-level-6 ((,class (:foreground ,foreground :height 1.1))))
   `(org-level-7 ((,class (:foreground ,foreground))))
   `(org-level-8 ((,class (:foreground ,foreground))))

   ;; outline
   `(outline-1 ((,class (:inherit org-level-1))))
   `(outline-2 ((,class (:inherit org-level-2))))
   `(outline-3 ((,class (:inherit org-level-3))))
   `(outline-4 ((,class (:inherit org-level-4))))
   `(outline-5 ((,class (:inherit org-level-5))))
   `(outline-6 ((,class (:inherit org-level-6))))
   `(outline-7 ((,class (:inherit org-level-7))))
   `(outline-8 ((,class (:inherit org-level-8))))

   `(org-document-title ((,class (:foreground ,foreground))))

   `(org-link ((,class (:background ,org-background :foreground ,foreground :underline t))))
   `(org-tag ((,class (:background ,org-background :foreground ,foreground))))
   `(org-warning ((,class (:background ,region :foreground ,foreground :weight bold))))
   `(org-todo ((,class (:background ,region :foreground ,foreground :weight bold))))
   `(org-done ((,class (:background ,region :foreground ,foreground :weight bold))))

   `(org-table ((,class (:background ,org-background))))
   `(org-code ((,class (:background ,org-background))))
   `(org-date ((,class (:background ,org-background :underline t))))
   `(org-block ((,class (:background ,org-background))))
   `(org-block-background ((,class (:background ,org-background :foreground ,foreground))))
   `(org-block-begin-line
     ((,class (:background ,org-background :foreground ,comment-delimiter :weight bold))))
   `(org-block-end-line
     ((,class (:background ,org-background :foreground ,comment-delimiter :weight bold))))))

;;; Footer
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide 'themodor)

;;; themodor.el ends here
