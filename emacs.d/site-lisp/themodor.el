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

;;; Commentary:

;; Personal theme for emacs

;;; Credits:

;; This theme is based on Zenburn from Bozhidar Batsov

;;; Code:

(deftheme themodor
  "The Themodor theme")

(defgroup themodor-theme nil
  "Themodor theme."
  :group 'faces
  :prefix "themodor-"
  :tag "Themodor theme")

;;;###autoload
(defcustom themodor-override-colors-alist '()
  "Place to override default theme colors.

You can override a subset of the theme's default colors by
defining them in this alist."
  :group 'themodor-theme
  :type '(alist
          :key-type (string :tag "Name")
          :value-type (string :tag " Hex")))

(defvar themodor-use-variable-pitch nil
  "When non-nil, use variable pitch face for some headings and titles.")

(defvar themodor-scale-org-headlines nil
  "Whether `org-mode' headlines should be scaled.")

(defvar themodor-scale-outline-headlines nil
  "Whether `outline-mode' headlines should be scaled.")

(defcustom themodor-height-minus-1 0.8
  "Font size -1."
  :type 'number
  :group 'themodor-theme
  :package-version '(themodor . "0.1"))

(defcustom themodor-height-plus-1 1.1
  "Font size +1."
  :type 'number
  :group 'themodor-theme
  :package-version '(themodor . "0.1"))

(defcustom themodor-height-plus-2 1.15
  "Font size +2."
  :type 'number
  :group 'themodor-theme
  :package-version '(themodor . "0.1"))

(defcustom themodor-height-plus-3 1.2
  "Font size +3."
  :type 'number
  :group 'themodor-theme
  :package-version '(themodor . "0.1"))

(defcustom themodor-height-plus-4 1.3
  "Font size +4."
  :type 'number
  :group 'themodor-theme
  :package-version '(themodor . "0.1"))

;;; Color Palette

(defvar themodor-default-colors-alist
  '(("themodor-fg-1"     . "#656555")
    ("themodor-fg"       . "#C5C8C6")
    ("themodor-fg+1"     . "#FFFFEF")

    ("themodor-bg-1"     . "#07090E")
    ("themodor-bg-08"    . "#303030")
    ("themodor-bg-05"    . "#383838")
    ("themodor-bg"       . "#0D1117")
    ("themodor-bg+05"    . "#0D1116")
    ("themodor-bg+1"     . "#161B22")
    ("themodor-bg+2"     . "#1D232B")
    ("themodor-bg+3"     . "#6F6F6F")

    ("themodor-grey"     . "#5A5B5A")
    ("themodor-brown"    . "#DBCEA4")

    ("themodor-red-4"    . "#B73F45")
    ("themodor-red-3"    . "#B74C51")
    ("themodor-red-2"    . "#C26065")
    ("themodor-red-1"    . "#C17B7E")
    ("themodor-red"      . "#D6867A")
    ("themodor-red+1"    . "#DCA3A3")

    ("themodor-purple"   . "#B294BB")

    ("themodor-orange"   . "#DE935F")

    ("themodor-yellow-2" . "#D0BF8F")
    ("themodor-yellow-1" . "#E0CF9F")
    ("themodor-yellow"   . "#C1AE7C")

    ("themodor-green-2"  . "#238636")
    ("themodor-green-1"  . "#228769")
    ("themodor-green"    . "#3FB68B")
    ("themodor-green+1"  . "#8FB28F")
    ("themodor-green+2"  . "#9FC59F")
    ("themodor-green+3"  . "#AFD8AF")
    ("themodor-green+4"  . "#BFEBBF")

    ("themodor-cyan"     . "#8ABEB7")

    ("themodor-blue+1"   . "#88DDFF")
    ("themodor-blue"     . "#389EDB")
    ("themodor-blue-1"   . "#718CBC")
    ("themodor-blue-2"   . "#6CA0A3")
    ("themodor-blue-3"   . "#5C888B")
    ("themodor-blue-4"   . "#4C7073")
    ("themodor-blue-5"   . "#366060")

    ("themodor-magenta"  . "#C9B4CF")

    ("themodor-added"             . "#172E20")
    ("themodor-changed"           . "#16252d")
    ("themodor-removed"           . "#371A1E")
    ("themodor-refine-added"      . "#3CC156")
    ("themodor-refine-changed"    . "#888811")
    ("themodor-refine-removed"    . "#BA3434")
    ("themodor-indicator-added"   . "#172E20")
    ("themodor-indicator-removed" . "#371A1E")
    )
  "List of Themodor colors.
Each element has the form (NAME . HEX).

`+N' suffixes indicate a color is lighter.
`-N' suffixes indicate a color is darker.")

(defmacro themodor-with-color-variables (&rest body)
  "`let' bind all colors defined in `themodor-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   (append themodor-default-colors-alist
                           themodor-override-colors-alist))
         (z-variable-pitch (if themodor-use-variable-pitch
                               'variable-pitch 'default)))
     ,@body))

;;; Theme Faces
(themodor-with-color-variables
  (custom-theme-set-faces
   'themodor
;;;; Built-in
;;;;; basic coloring
   '(button ((t (:underline t))))
   `(link ((t (:foreground ,themodor-yellow :underline t :weight bold))))
   `(link-visited ((t (:foreground ,themodor-yellow-2 :underline t :weight normal))))
   `(default ((t (:foreground ,themodor-fg :background ,themodor-bg))))
   `(cursor ((t (:foreground ,themodor-fg :background ,themodor-fg+1))))
   `(widget-field ((t (:foreground ,themodor-fg :background ,themodor-bg+3))))
   `(escape-glyph ((t (:foreground ,themodor-yellow :weight bold))))
   `(fringe ((t (:foreground ,themodor-fg :background ,themodor-bg))))
   `(header-line ((t (:foreground ,themodor-yellow
                                  :background ,themodor-bg-1
                                  :box (:line-width -1 :style released-button)
                                  :extend t))))
   `(highlight ((t (:background ,themodor-bg-1))))
   `(success ((t (:foreground ,themodor-green))))
   `(warning ((t (:foreground ,themodor-orange))))
   `(error ((t (:foreground ,themodor-red))))
   `(tooltip ((t (:foreground ,themodor-fg :background ,themodor-bg+1))))
;;;;; modeline
   `(mode-line ((t (:foreground ,themodor-fg :background ,themodor-bg :box ,themodor-bg+2))))
   `(mode-line-buffer-id ((t :inherit bold)))
   `(mode-line-emphasis ((t :inherit bold :foreground ,themodor-blue)))
   `(mode-line-highlight ((t :foreground ,themodor-blue :box (:line-width -1 :style pressed-button))))
   `(mode-line-inactive ((t (:foreground ,themodor-fg :background ,themodor-bg+2 :box ,themodor-bg+2))))
;;;;; compilation
   `(compilation-column-face ((t (:foreground ,themodor-yellow))))
   `(compilation-column-number ((t (:foreground ,themodor-yellow))))
   `(compilation-enter-directory-face ((t (:foreground ,themodor-green))))
   `(compilation-error-face ((t (:foreground ,themodor-red-1 :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,themodor-fg))))
   `(compilation-info-face ((t (:foreground ,themodor-blue))))
   `(compilation-info ((t (:foreground ,themodor-green+4 :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,themodor-green))))
   `(compilation-line-face ((t (:foreground ,themodor-yellow))))
   `(compilation-line-number ((t (:foreground ,themodor-yellow))))
   `(compilation-message-face ((t (:foreground ,themodor-blue))))
   `(compilation-warning-face ((t (:foreground ,themodor-orange :weight bold :underline t))))
   `(compilation-mode-line-exit ((t (:foreground ,themodor-green+2 :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,themodor-red :weight bold))))
   `(compilation-mode-line-run ((t (:foreground ,themodor-yellow :weight bold))))
;;;;; completions
   `(completions-annotations ((t (:foreground ,themodor-fg-1))))
;;;;; customize
   `(custom-variable-tag ((t (:foreground ,themodor-blue :weight bold))))
   `(custom-group-tag ((t (:foreground ,themodor-blue :weight bold :height 1.2))))
   `(custom-state ((t (:foreground ,themodor-green+4))))
;;;;; display-fill-column-indicator
   `(fill-column-indicator ((,class :foreground ,themodor-bg-05 :weight semilight)))
;;;;; grep
   `(grep-context-face ((t (:foreground ,themodor-fg))))
   `(grep-error-face ((t (:foreground ,themodor-red-1 :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,themodor-blue))))
   `(grep-match-face ((t (:foreground ,themodor-orange :weight bold))))
   `(match ((t (:background ,themodor-bg-1 :foreground ,themodor-orange :weight bold))))
;;;;; hi-lock
   `(hi-blue    ((t (:background ,themodor-cyan    :foreground ,themodor-bg-1))))
   `(hi-green   ((t (:background ,themodor-green+4 :foreground ,themodor-bg-1))))
   `(hi-pink    ((t (:background ,themodor-magenta :foreground ,themodor-bg-1))))
   `(hi-yellow  ((t (:background ,themodor-yellow  :foreground ,themodor-bg-1))))
   `(hi-blue-b  ((t (:foreground ,themodor-blue    :weight     bold))))
   `(hi-green-b ((t (:foreground ,themodor-green+2 :weight     bold))))
   `(hi-red-b   ((t (:foreground ,themodor-red     :weight     bold))))
;;;;; info
   `(Info-quoted ((t (:inherit font-lock-constant-face))))
;;;;; isearch
   `(isearch ((t (:foreground ,themodor-yellow-2 :weight bold :background ,themodor-bg))))
   `(isearch-fail ((t (:foreground ,themodor-fg :background ,themodor-red-4))))
   `(lazy-highlight ((t (:foreground ,themodor-yellow-2 :weight bold :background ,themodor-bg))))

   `(menu ((t (:foreground ,themodor-fg :background ,themodor-bg))))
   `(minibuffer-prompt ((t (:foreground ,themodor-yellow))))
;;;;; modeline
   `(mode-line ((t (:foreground ,themodor-fg :background ,themodor-bg :box ,themodor-bg+2))))
   `(mode-line-buffer-id ((t :inherit bold)))
   `(mode-line-emphasis ((t :inherit bold :foreground ,themodor-blue)))
   `(mode-line-highlight ((t :foreground ,themodor-blue :box (:line-width -1 :style pressed-button))))
   `(mode-line-inactive ((t (:foreground ,themodor-fg :background ,themodor-bg+1 :box ,themodor-bg+2))))

   `(region ((,class (:background ,themodor-bg+1 :extend t))
             (t :inverse-video t)))
   `(secondary-selection ((t (:background ,themodor-bg+2))))
   `(trailing-whitespace ((t (:background ,themodor-red))))
   `(vertical-border ((t (:foreground ,themodor-fg))))
;;;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,themodor-red))))
   `(font-lock-comment-face ((t (:foreground ,themodor-grey :slant italic))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,themodor-grey :slant italic))))
   `(font-lock-constant-face ((t (:foreground ,themodor-orange))))
   `(font-lock-doc-face ((t (:foreground ,themodor-grey))))
   `(font-lock-function-name-face ((t (:foreground ,themodor-green))))
   `(font-lock-keyword-face ((t (:foreground ,themodor-blue))))
   `(font-lock-negation-char-face ((t (:foreground ,themodor-yellow :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,themodor-blue+1))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,themodor-yellow :weight bold))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,themodor-green :weight bold))))
   `(font-lock-string-face ((t (:foreground ,themodor-brown))))
   `(font-lock-type-face ((t (:foreground ,themodor-green-1))))
   `(font-lock-variable-name-face ((t (:foreground ,themodor-blue+1))))
   `(font-lock-warning-face ((t (:foreground ,themodor-yellow-2 :weight bold))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))
;;;;; line numbers (Emacs 26.1 and above)
   `(line-number ((t (:foreground ,themodor-bg+3 :background ,themodor-bg-05))))
   `(line-number-current-line ((t (:inherit line-number :foreground ,themodor-yellow-2))))
;;;;; man
   '(Man-overstrike ((t (:inherit font-lock-keyword-face))))
   '(Man-underline  ((t (:inherit (font-lock-string-face underline)))))
;;;;; woman
   '(woman-bold   ((t (:inherit font-lock-keyword-face))))
   '(woman-italic ((t (:inherit (font-lock-string-face italic)))))
;;;; Third-party
;;;;; ace-jump
   `(ace-jump-face-background
     ((t (:foreground ,themodor-fg-1 :background ,themodor-bg :inverse-video nil))))
   `(ace-jump-face-foreground
     ((t (:foreground ,themodor-green+2 :background ,themodor-bg :inverse-video nil))))
;;;;; ace-window
   `(aw-background-face
     ((t (:foreground ,themodor-fg-1 :background ,themodor-bg :inverse-video nil))))
   `(aw-leading-char-face ((t (:inherit aw-mode-line-face))))
;;;;; company-mode
   `(company-tooltip ((t (:foreground ,themodor-fg :background ,themodor-bg+05))))
   `(company-tooltip-annotation ((t (:foreground ,themodor-orange :background ,themodor-bg+1))))
   `(company-tooltip-annotation-selection ((t (:foreground ,themodor-orange :background ,themodor-bg-1))))
   `(company-tooltip-selection ((t (:foreground ,themodor-fg :background ,themodor-bg+1))))
   `(company-tooltip-mouse ((t (:background ,themodor-bg-1))))
   `(company-tooltip-common ((t (:foreground ,themodor-green+2))))
   `(company-tooltip-common-selection ((t (:foreground ,themodor-green+2))))
   `(company-scrollbar-fg ((t (:background ,themodor-bg-1))))
   `(company-scrollbar-bg ((t (:background ,themodor-bg+2))))
   `(company-preview ((t (:background ,themodor-green+2))))
   `(company-preview-common ((t (:foreground ,themodor-green+2 :background ,themodor-bg-1))))
;;;;; cider
   `(cider-result-overlay-face ((t (:background unspecified))))
   `(cider-enlightened-face ((t (:box (:color ,themodor-orange :line-width -1)))))
   `(cider-enlightened-local-face ((t (:weight bold :foreground ,themodor-green+1))))
   `(cider-deprecated-face ((t (:background ,themodor-yellow-2))))
   `(cider-instrumented-face ((t (:box (:color ,themodor-red :line-width -1)))))
   `(cider-traced-face ((t (:box (:color ,themodor-cyan :line-width -1)))))
   `(cider-test-failure-face ((t (:background ,themodor-red-4))))
   `(cider-test-error-face ((t (:background ,themodor-magenta))))
   `(cider-test-success-face ((t (:background ,themodor-green-2))))
   `(cider-fringe-good-face ((t (:foreground ,themodor-green+4))))
;;;;; SLIME
   `(sly-mrepl-output-face ((t (:foreground ,themodor-blue))))
   `(slime-repl-inputed-output-face ((t (:foreground ,themodor-green))))
   `(slime-error-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,themodor-red)))
      (t
       (:underline ,themodor-red))))
   `(slime-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,themodor-orange)))
      (t
       (:underline ,themodor-orange))))
   `(slime-style-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,themodor-yellow)))
      (t
       (:underline ,themodor-yellow))))
   `(slime-note-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,themodor-green)))
      (t
       (:underline ,themodor-green))))
   `(slime-highlight-face ((t (:inherit highlight))))
;;;;; diff
   `(diff-added             ((t (:background ,themodor-added))))
   `(diff-changed           ((t (:background ,themodor-changed))))
   `(diff-removed           ((t (:background ,themodor-removed))))
   `(diff-refine-added      ((t (:foreground ,themodor-refine-added))))
   `(diff-refine-changed    ((t (:foreground ,themodor-refine-changed))))
   `(diff-refine-removed    ((t (:foreground ,themodor-refine-removed))))
   `(diff-indicator-added   ((t (:background ,themodor-indicator-added))))
   `(diff-indicator-removed ((t (:background ,themodor-indicator-removed))))
   `(diff-header ((,class (:background ,themodor-bg+1))
                  (t (:background ,themodor-fg :foreground ,themodor-bg))))
   `(diff-file-header
     ((,class (:background ,themodor-bg+1 :foreground ,themodor-fg :weight bold))
      (t (:background ,themodor-fg :foreground ,themodor-bg :weight bold))))
;;;;; diff-hl
   `(diff-hl-change ((,class (:foreground ,themodor-blue :background ,themodor-changed))))
   `(diff-hl-delete ((,class (:foreground ,themodor-red+1 :background ,themodor-removed))))
   `(diff-hl-insert ((,class (:foreground ,themodor-green+1 :background ,themodor-added))))
;;;;; dim-autoload
   `(dim-autoload-cookie-line ((t :foreground ,themodor-bg+1)))
;;;;; dired+
   `(diredp-display-msg ((t (:foreground ,themodor-blue))))
   `(diredp-compressed-file-suffix ((t (:foreground ,themodor-orange))))
   `(diredp-date-time ((t (:foreground ,themodor-magenta))))
   `(diredp-deletion ((t (:foreground ,themodor-yellow))))
   `(diredp-deletion-file-name ((t (:foreground ,themodor-red))))
   `(diredp-dir-heading ((t (:foreground ,themodor-blue :background ,themodor-bg-1))))
   `(diredp-dir-priv ((t (:foreground ,themodor-cyan))))
   `(diredp-exec-priv ((t (:foreground ,themodor-red))))
   `(diredp-executable-tag ((t (:foreground ,themodor-green+1))))
   `(diredp-file-name ((t (:foreground ,themodor-blue))))
   `(diredp-file-suffix ((t (:foreground ,themodor-green))))
   `(diredp-flag-mark ((t (:foreground ,themodor-yellow))))
   `(diredp-flag-mark-line ((t (:foreground ,themodor-orange))))
   `(diredp-ignored-file-name ((t (:foreground ,themodor-red))))
   `(diredp-link-priv ((t (:foreground ,themodor-yellow))))
   `(diredp-mode-line-flagged ((t (:foreground ,themodor-yellow))))
   `(diredp-mode-line-marked ((t (:foreground ,themodor-orange))))
   `(diredp-no-priv ((t (:foreground ,themodor-fg))))
   `(diredp-number ((t (:foreground ,themodor-green+1))))
   `(diredp-other-priv ((t (:foreground ,themodor-yellow-1))))
   `(diredp-rare-priv ((t (:foreground ,themodor-red-1))))
   `(diredp-read-priv ((t (:foreground ,themodor-green-2))))
   `(diredp-symlink ((t (:foreground ,themodor-yellow))))
   `(diredp-write-priv ((t (:foreground ,themodor-magenta))))
;;;;; dired-async
   `(dired-async-failures ((t (:foreground ,themodor-red :weight bold))))
   `(dired-async-message ((t (:foreground ,themodor-yellow :weight bold))))
   `(dired-async-mode-message ((t (:foreground ,themodor-yellow))))
;;;;; diredfl
   `(diredfl-compressed-file-suffix ((t (:foreground ,themodor-orange))))
   `(diredfl-date-time ((t (:foreground ,themodor-magenta))))
   `(diredfl-deletion ((t (:foreground ,themodor-yellow))))
   `(diredfl-deletion-file-name ((t (:foreground ,themodor-red))))
   `(diredfl-dir-heading ((t (:foreground ,themodor-blue :background ,themodor-bg-1))))
   `(diredfl-dir-priv ((t (:foreground ,themodor-cyan))))
   `(diredfl-exec-priv ((t (:foreground ,themodor-red))))
   `(diredfl-executable-tag ((t (:foreground ,themodor-green+1))))
   `(diredfl-file-name ((t (:foreground ,themodor-blue))))
   `(diredfl-file-suffix ((t (:foreground ,themodor-green))))
   `(diredfl-flag-mark ((t (:foreground ,themodor-yellow))))
   `(diredfl-flag-mark-line ((t (:foreground ,themodor-orange))))
   `(diredfl-ignored-file-name ((t (:foreground ,themodor-red))))
   `(diredfl-link-priv ((t (:foreground ,themodor-yellow))))
   `(diredfl-no-priv ((t (:foreground ,themodor-fg))))
   `(diredfl-number ((t (:foreground ,themodor-green+1))))
   `(diredfl-other-priv ((t (:foreground ,themodor-yellow-1))))
   `(diredfl-rare-priv ((t (:foreground ,themodor-red-1))))
   `(diredfl-read-priv ((t (:foreground ,themodor-green-1))))
   `(diredfl-symlink ((t (:foreground ,themodor-yellow))))
   `(diredfl-write-priv ((t (:foreground ,themodor-magenta))))
;;;;; ediff
   `(ediff-current-diff-A ((t (:foreground ,themodor-fg :background ,themodor-red-4))))
   `(ediff-current-diff-Ancestor ((t (:foreground ,themodor-fg :background ,themodor-red-4))))
   `(ediff-current-diff-B ((t (:foreground ,themodor-fg :background ,themodor-green-2))))
   `(ediff-current-diff-C ((t (:foreground ,themodor-fg :background ,themodor-blue-5))))
   `(ediff-even-diff-A ((t (:background ,themodor-bg+1))))
   `(ediff-even-diff-Ancestor ((t (:background ,themodor-bg+1))))
   `(ediff-even-diff-B ((t (:background ,themodor-bg+1))))
   `(ediff-even-diff-C ((t (:background ,themodor-bg+1))))
   `(ediff-fine-diff-A ((t (:foreground ,themodor-fg :background ,themodor-red-2 :weight bold))))
   `(ediff-fine-diff-Ancestor ((t (:foreground ,themodor-fg :background ,themodor-red-2 weight bold))))
   `(ediff-fine-diff-B ((t (:foreground ,themodor-fg :background ,themodor-green :weight bold))))
   `(ediff-fine-diff-C ((t (:foreground ,themodor-fg :background ,themodor-blue-3 :weight bold ))))
   `(ediff-odd-diff-A ((t (:background ,themodor-bg+2))))
   `(ediff-odd-diff-Ancestor ((t (:background ,themodor-bg+2))))
   `(ediff-odd-diff-B ((t (:background ,themodor-bg+2))))
   `(ediff-odd-diff-C ((t (:background ,themodor-bg+2))))
;;;;; erc
   `(erc-action-face ((t (:inherit erc-default-face))))
   `(erc-bold-face ((t (:weight bold))))
   `(erc-current-nick-face ((t (:foreground ,themodor-blue :weight bold))))
   `(erc-dangerous-host-face ((t (:inherit font-lock-warning-face))))
   `(erc-default-face ((t (:foreground ,themodor-fg))))
   `(erc-direct-msg-face ((t (:inherit erc-default-face))))
   `(erc-error-face ((t (:inherit font-lock-warning-face))))
   `(erc-fool-face ((t (:inherit erc-default-face))))
   `(erc-highlight-face ((t (:inherit hover-highlight))))
   `(erc-input-face ((t (:foreground ,themodor-yellow))))
   `(erc-keyword-face ((t (:foreground ,themodor-blue :weight bold))))
   `(erc-nick-default-face ((t (:foreground ,themodor-cyan :weight bold))))
   `(erc-my-nick-face ((t (:foreground ,themodor-red :weight bold))))
   `(erc-nick-msg-face ((t (:inherit erc-default-face))))
   `(erc-notice-face ((t (:foreground ,themodor-grey))))
   `(erc-pal-face ((t (:foreground ,themodor-orange :weight bold))))
   `(erc-prompt-face ((t (:foreground ,themodor-orange :background ,themodor-bg :weight bold))))
   `(erc-timestamp-face ((t (:foreground ,themodor-green+4))))
   `(erc-underline-face ((t (:underline t))))
;;;;; ert
   `(ert-test-result-expected ((t (:foreground ,themodor-green+4 :background ,themodor-bg))))
   `(ert-test-result-unexpected ((t (:foreground ,themodor-red :background ,themodor-bg))))
;;;;; eshell
   `(eshell-prompt ((t (:foreground ,themodor-yellow :weight bold))))
   `(eshell-ls-archive ((t (:foreground ,themodor-red-1 :weight bold))))
   `(eshell-ls-backup ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-clutter ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-directory ((t (:foreground ,themodor-blue+1 :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,themodor-red+1 :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,themodor-fg))))
   `(eshell-ls-missing ((t (:inherit font-lock-warning-face))))
   `(eshell-ls-product ((t (:inherit font-lock-doc-face))))
   `(eshell-ls-special ((t (:foreground ,themodor-yellow :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,themodor-cyan :weight bold))))
;;;;; flycheck
   `(flycheck-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,themodor-red-1) :inherit unspecified))
      (t (:foreground ,themodor-red-1 :weight bold :underline t))))
   `(flycheck-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,themodor-yellow) :inherit unspecified))
      (t (:foreground ,themodor-yellow :weight bold :underline t))))
   `(flycheck-info
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,themodor-cyan) :inherit unspecified))
      (t (:foreground ,themodor-cyan :weight bold :underline t))))
   `(flycheck-fringe-error ((t (:foreground ,themodor-red-1 :weight bold))))
   `(flycheck-fringe-warning ((t (:foreground ,themodor-yellow :weight bold))))
   `(flycheck-fringe-info ((t (:foreground ,themodor-cyan :weight bold))))
;;;;; flymake
   `(flymake-errline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,themodor-red)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,themodor-red-1 :weight bold :underline t))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,themodor-orange)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,themodor-orange :weight bold :underline t))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,themodor-green)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,themodor-green-2 :weight bold :underline t))))
;;;;; flyspell
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,themodor-orange) :inherit unspecified))
      (t (:foreground ,themodor-orange :weight bold :underline t))))
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,themodor-red) :inherit unspecified))
      (t (:foreground ,themodor-red-1 :weight bold :underline t))))
;;;;; git-commit
   `(git-commit-comment-action  ((,class (:foreground ,themodor-green+1 :weight bold))))
   `(git-commit-comment-branch  ((,class (:foreground ,themodor-blue+1  :weight bold)))) ; obsolete
   `(git-commit-comment-branch-local  ((,class (:foreground ,themodor-blue+1  :weight bold))))
   `(git-commit-comment-branch-remote ((,class (:foreground ,themodor-green  :weight bold))))
   `(git-commit-comment-heading ((,class (:foreground ,themodor-yellow  :weight bold))))
;;;;; git-gutter
   `(git-gutter:added ((t (:foreground ,themodor-green :weight bold :inverse-video t))))
   `(git-gutter:deleted ((t (:foreground ,themodor-red :weight bold :inverse-video t))))
   `(git-gutter:modified ((t (:foreground ,themodor-magenta :weight bold :inverse-video t))))
   `(git-gutter:unchanged ((t (:foreground ,themodor-fg :weight bold :inverse-video t))))
;;;;; git-gutter-fr
   `(git-gutter-fr:added ((t (:foreground ,themodor-green  :weight bold))))
   `(git-gutter-fr:deleted ((t (:foreground ,themodor-red :weight bold))))
   `(git-gutter-fr:modified ((t (:foreground ,themodor-magenta :weight bold))))
;;;;; git-rebase
   `(git-rebase-hash ((t (:foreground, themodor-orange))))
;;;;; highlight-numbers
   `(highlight-numbers-number ((t (:foreground ,themodor-blue))))
;;;;; highlight-symbol
   `(highlight-symbol-face ((t (:background ,themodor-bg+2))))
;;;;; highlight-thing
   `(highlight-thing ((t (:background ,themodor-bg+2))))
;;;;; hl-line-mode
   `(hl-line-face ((,class (:background ,themodor-bg-05))
                   (t :weight bold)))
   `(hl-line ((,class (:background ,themodor-bg-05 :extend t)) ; old emacsen
              (t :weight bold)))
;;;;; hl-sexp
   `(hl-sexp-face ((,class (:background ,themodor-bg+1))
                   (t :weight bold)))
;;;;; info+
   `(info-command-ref-item ((t (:background ,themodor-bg-1 :foreground ,themodor-orange))))
   `(info-constant-ref-item ((t (:background ,themodor-bg-1 :foreground ,themodor-magenta))))
   `(info-double-quoted-name ((t (:inherit font-lock-comment-face))))
   `(info-file ((t (:background ,themodor-bg-1 :foreground ,themodor-yellow))))
   `(info-function-ref-item ((t (:background ,themodor-bg-1 :inherit font-lock-function-name-face))))
   `(info-macro-ref-item ((t (:background ,themodor-bg-1 :foreground ,themodor-yellow))))
   `(info-menu ((t (:foreground ,themodor-yellow))))
   `(info-quoted-name ((t (:inherit font-lock-constant-face))))
   `(info-reference-item ((t (:background ,themodor-bg-1))))
   `(info-single-quote ((t (:inherit font-lock-keyword-face))))
   `(info-special-form-ref-item ((t (:background ,themodor-bg-1 :foreground ,themodor-yellow))))
   `(info-string ((t (:inherit font-lock-string-face))))
   `(info-syntax-class-item ((t (:background ,themodor-bg-1 :foreground ,themodor-blue+1))))
   `(info-user-option-ref-item ((t (:background ,themodor-bg-1 :foreground ,themodor-red))))
   `(info-variable-ref-item ((t (:background ,themodor-bg-1 :foreground ,themodor-orange))))
;;;;; iedit-mode
   `(iedit-occurrence ((t (:background ,themodor-bg+2 :weight bold))))
;;;;; macrostep
   `(macrostep-gensym-1
     ((t (:foreground ,themodor-green+2 :background ,themodor-bg-1))))
   `(macrostep-gensym-2
     ((t (:foreground ,themodor-red+1 :background ,themodor-bg-1))))
   `(macrostep-gensym-3
     ((t (:foreground ,themodor-blue+1 :background ,themodor-bg-1))))
   `(macrostep-gensym-4
     ((t (:foreground ,themodor-magenta :background ,themodor-bg-1))))
   `(macrostep-gensym-5
     ((t (:foreground ,themodor-yellow :background ,themodor-bg-1))))
   `(macrostep-expansion-highlight-face
     ((t (:inherit highlight))))
   `(macrostep-macro-face
     ((t (:underline t))))
;;;;; markup-faces
   `(markup-anchor-face ((t (:foreground ,themodor-blue+1))))
   `(markup-code-face ((t (:inherit font-lock-constant-face))))
   `(markup-command-face ((t (:foreground ,themodor-yellow))))
   `(markup-emphasis-face ((t (:inherit bold))))
   `(markup-internal-reference-face ((t (:foreground ,themodor-yellow-2 :underline t))))
   `(markup-list-face ((t (:foreground ,themodor-fg+1))))
   `(markup-meta-face ((t (:foreground ,themodor-yellow))))
   `(markup-meta-hide-face ((t (:foreground ,themodor-yellow))))
   `(markup-secondary-text-face ((t (:foreground ,themodor-yellow-1))))
   `(markup-title-0-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-1-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-2-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-3-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-4-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-typewriter-face ((t (:inherit font-lock-constant-face))))
   `(markup-verbatim-face ((t (:inherit font-lock-constant-face))))
   `(markup-value-face ((t (:foreground ,themodor-yellow))))
;;;;; message-mode
   `(message-cited-text ((t (:inherit font-lock-comment-face))))
   `(message-header-name ((t (:foreground ,themodor-green+1))))
   `(message-header-other ((t (:foreground ,themodor-green))))
   `(message-header-to ((t (:foreground ,themodor-yellow :weight bold))))
   `(message-header-cc ((t (:foreground ,themodor-yellow :weight bold))))
   `(message-header-newsgroups ((t (:foreground ,themodor-yellow :weight bold))))
   `(message-header-subject ((t (:foreground ,themodor-orange :weight bold))))
   `(message-header-xheader ((t (:foreground ,themodor-green))))
   `(message-mml ((t (:foreground ,themodor-yellow :weight bold))))
   `(message-separator ((t (:inherit font-lock-comment-face))))
;;;;; notmuch
   `(notmuch-crypto-decryption ((t (:foreground ,themodor-bg :background ,themodor-magenta))))
   `(notmuch-crypto-part-header ((t (:foreground ,themodor-blue+1))))
   `(notmuch-crypto-signature-bad ((t (:foreground ,themodor-bg :background ,themodor-red))))
   `(notmuch-crypto-signature-good ((t (:foreground ,themodor-bg :background ,themodor-green+1))))
   `(notmuch-crypto-signature-good-key ((t (:foreground ,themodor-bg :background ,themodor-orange))))
   `(notmuch-crypto-signature-unknown ((t (:foreground ,themodor-bg :background ,themodor-red))))
   `(notmuch-hello-logo-background ((t (:background ,themodor-bg+2))))
   `(notmuch-message-summary-face ((t (:background ,themodor-bg-08))))
   `(notmuch-search-flagged-face ((t (:foreground ,themodor-blue+1))))
   `(notmuch-search-non-matching-authors ((t (:foreground ,themodor-fg-1))))
   `(notmuch-tag-added ((t (:underline ,themodor-green+1))))
   `(notmuch-tag-deleted ((t (:strike-through ,themodor-red))))
   `(notmuch-tag-face ((t (:foreground ,themodor-green+1))))
   `(notmuch-tag-flagged ((t (:foreground ,themodor-blue+1))))
   `(notmuch-tag-unread ((t (:foreground ,themodor-red))))
   `(notmuch-tree-match-author-face ((t (:foreground ,themodor-green+1))))
   `(notmuch-tree-match-tag-face ((t (:foreground ,themodor-green+1))))
;;;;; markdown
   `(markdown-code-face
     ((t (:foreground ,themodor-green))))
;;;;; org-mode
   `(org-agenda-date-today
     ((t (:foreground ,themodor-fg+1 :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((t (:inherit font-lock-comment-face))))
   `(org-archived ((t (:foreground ,themodor-fg :weight bold))))
   `(org-block ((t (:background ,themodor-bg+05 :extend t))))
   `(org-checkbox ((t (:background ,themodor-bg+2 :foreground ,themodor-fg+1
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((t (:foreground ,themodor-blue :underline t))))
   `(org-deadline-announce ((t (:foreground ,themodor-red-1))))
   `(org-done ((t (:weight bold :weight bold :foreground ,themodor-green+3))))
   `(org-formula ((t (:foreground ,themodor-yellow-2))))
   `(org-headline-done ((t (:foreground ,themodor-green+3))))
   `(org-hide ((t (:foreground ,themodor-bg))))
   `(org-level-1 ((t (:inherit ,z-variable-pitch :foreground ,themodor-orange
                               ,@(when themodor-scale-org-headlines
                                   (list :height themodor-height-plus-4))))))
   `(org-level-2 ((t (:inherit ,z-variable-pitch :foreground ,themodor-green+4
                               ,@(when themodor-scale-org-headlines
                                   (list :height themodor-height-plus-3))))))
   `(org-level-3 ((t (:inherit ,z-variable-pitch :foreground ,themodor-blue-1
                               ,@(when themodor-scale-org-headlines
                                   (list :height themodor-height-plus-2))))))
   `(org-level-4 ((t (:inherit ,z-variable-pitch :foreground ,themodor-yellow-2
                               ,@(when themodor-scale-org-headlines
                                   (list :height themodor-height-plus-1))))))
   `(org-level-5 ((t (:inherit ,z-variable-pitch :foreground ,themodor-cyan))))
   `(org-level-6 ((t (:inherit ,z-variable-pitch :foreground ,themodor-green+2))))
   `(org-level-7 ((t (:inherit ,z-variable-pitch :foreground ,themodor-red-4))))
   `(org-level-8 ((t (:inherit ,z-variable-pitch :foreground ,themodor-blue-4))))
   `(org-link ((t (:foreground ,themodor-yellow-2 :underline t))))
   `(org-quote ((t (:background ,themodor-bg+05 :extend t))))
   `(org-scheduled ((t (:foreground ,themodor-green+4))))
   `(org-scheduled-previously ((t (:foreground ,themodor-red))))
   `(org-scheduled-today ((t (:foreground ,themodor-blue+1))))
   `(org-sexp-date ((t (:foreground ,themodor-blue+1 :underline t))))
   `(org-special-keyword ((t (:inherit font-lock-comment-face))))
   `(org-table ((t (:foreground ,themodor-green+2))))
   `(org-tag ((t (:weight bold :weight bold))))
   `(org-time-grid ((t (:foreground ,themodor-orange))))
   `(org-todo ((t (:weight bold :foreground ,themodor-red :weight bold))))
   `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   `(org-warning ((t (:weight bold :foreground ,themodor-red :weight bold :underline nil))))
   `(org-column ((t (:background ,themodor-bg-1))))
   `(org-column-title ((t (:background ,themodor-bg-1 :underline t :weight bold))))
   `(org-mode-line-clock ((t (:foreground ,themodor-fg :background ,themodor-bg-1))))
   `(org-mode-line-clock-overrun ((t (:foreground ,themodor-bg :background ,themodor-red-1))))
   `(org-ellipsis ((t (:foreground ,themodor-yellow-1 :underline t))))
   `(org-footnote ((t (:foreground ,themodor-cyan :underline t))))
   `(org-document-title ((t (:inherit ,z-variable-pitch :foreground ,themodor-blue
                                      :weight bold
                                      ,@(when themodor-scale-org-headlines
                                          (list :height themodor-height-plus-4))))))
   `(org-document-info ((t (:foreground ,themodor-blue))))
   `(org-habit-ready-face ((t :background ,themodor-green)))
   `(org-habit-alert-face ((t :background ,themodor-yellow-1 :foreground ,themodor-bg)))
   `(org-habit-clear-face ((t :background ,themodor-blue-3)))
   `(org-habit-overdue-face ((t :background ,themodor-red-3)))
   `(org-habit-clear-future-face ((t :background ,themodor-blue-4)))
   `(org-habit-ready-future-face ((t :background ,themodor-green-2)))
   `(org-habit-alert-future-face ((t :background ,themodor-yellow-2 :foreground ,themodor-bg)))
   `(org-habit-overdue-future-face ((t :background ,themodor-red-4)))
;;;;; org-ref
   `(org-ref-ref-face ((t :underline t)))
   `(org-ref-label-face ((t :underline t)))
   `(org-ref-cite-face ((t :underline t)))
   `(org-ref-glossary-face ((t :underline t)))
   `(org-ref-acronym-face ((t :underline t)))
;;;;; outline
   `(outline-1 ((t (:inherit ,z-variable-pitch :foreground ,themodor-orange
                             ,@(when themodor-scale-outline-headlines
                                 (list :height themodor-height-plus-4))))))
   `(outline-2 ((t (:inherit ,z-variable-pitch :foreground ,themodor-green+4
                             ,@(when themodor-scale-outline-headlines
                                 (list :height themodor-height-plus-3))))))
   `(outline-3 ((t (:inherit ,z-variable-pitch :foreground ,themodor-blue-1
                             ,@(when themodor-scale-outline-headlines
                                 (list :height themodor-height-plus-2))))))
   `(outline-4 ((t (:inherit ,z-variable-pitch :foreground ,themodor-yellow-2
                             ,@(when themodor-scale-outline-headlines
                                 (list :height themodor-height-plus-1))))))
   `(outline-5 ((t (:inherit ,z-variable-pitch :foreground ,themodor-cyan))))
   `(outline-6 ((t (:inherit ,z-variable-pitch :foreground ,themodor-green+2))))
   `(outline-7 ((t (:inherit ,z-variable-pitch :foreground ,themodor-red-4))))
   `(outline-8 ((t (:inherit ,z-variable-pitch :foreground ,themodor-blue-4))))
;;;;;; paren-face
   `(parenthesis ((t (:foreground ,themodor-fg-1))))
;;;;; perspective
   `(persp-selected-face ((t (:foreground ,themodor-yellow-2))))
;;;;; re-builder
   `(reb-match-0 ((t (:foreground ,themodor-bg :background ,themodor-magenta))))
   `(reb-match-1 ((t (:foreground ,themodor-bg :background ,themodor-blue))))
   `(reb-match-2 ((t (:foreground ,themodor-bg :background ,themodor-orange))))
   `(reb-match-3 ((t (:foreground ,themodor-bg :background ,themodor-red))))
;;;;; selectrum
   `(selectrum-current-candidate ((t (:foreground ,themodor-yellow :weight bold :background ,themodor-bg+2 :underline t :inherit highlight))))
   `(selectrum-primary-highlight ((t (:foreground ,themodor-green-1))))
   `(selectrum-secondary-highlight ((t (:background ,themodor-green-2))))
;;;;; sh-mode
   `(sh-heredoc     ((t (:foreground ,themodor-yellow :weight bold))))
   `(sh-quoted-exec ((t (:foreground ,themodor-red))))
;;;;; show-paren
   `(show-paren-mismatch ((t (:foreground ,themodor-red+1 :background ,themodor-bg+3 :weight bold))))
   `(show-paren-match ((t (:foreground ,themodor-fg :background ,themodor-bg+3 :weight bold))))
;;;;; term
   `(term-color-black ((t (:foreground ,themodor-bg
                                       :background ,themodor-bg-1))))
   `(term-color-red ((t (:foreground ,themodor-red-2
                                     :background ,themodor-red-4))))
   `(term-color-green ((t (:foreground ,themodor-green
                                       :background ,themodor-green+2))))
   `(term-color-yellow ((t (:foreground ,themodor-orange
                                        :background ,themodor-yellow))))
   `(term-color-blue ((t (:foreground ,themodor-blue-1
                                      :background ,themodor-blue-4))))
   `(term-color-magenta ((t (:foreground ,themodor-magenta
                                         :background ,themodor-red))))
   `(term-color-cyan ((t (:foreground ,themodor-cyan
                                      :background ,themodor-blue))))
   `(term-color-white ((t (:foreground ,themodor-fg
                                       :background ,themodor-fg-1))))
   '(term-default-fg-color ((t (:inherit term-color-white))))
   '(term-default-bg-color ((t (:inherit term-color-black))))
;;;;; undo-tree
   `(undo-tree-visualizer-active-branch-face ((t (:foreground ,themodor-fg+1 :weight bold))))
   `(undo-tree-visualizer-current-face ((t (:foreground ,themodor-red-1 :weight bold))))
   `(undo-tree-visualizer-default-face ((t (:foreground ,themodor-fg))))
   `(undo-tree-visualizer-register-face ((t (:foreground ,themodor-yellow))))
   `(undo-tree-visualizer-unmodified-face ((t (:foreground ,themodor-cyan))))
;;;;; whitespace-mode
   `(whitespace-space ((t (:background ,themodor-bg+1 :foreground ,themodor-bg+1))))
   `(whitespace-hspace ((t (:background ,themodor-bg+1 :foreground ,themodor-bg+1))))
   `(whitespace-tab ((t (:background ,themodor-red-1))))
   `(whitespace-newline ((t (:foreground ,themodor-bg+1))))
   `(whitespace-trailing ((t (:background ,themodor-red))))
   `(whitespace-line ((t (:background ,themodor-bg :foreground ,themodor-magenta))))
   `(whitespace-space-before-tab ((t (:background ,themodor-orange :foreground ,themodor-orange))))
   `(whitespace-indentation ((t (:background ,themodor-yellow :foreground ,themodor-red))))
   `(whitespace-empty ((t (:background ,themodor-yellow))))
   `(whitespace-space-after-tab ((t (:background ,themodor-yellow :foreground ,themodor-red))))))

;;; Theme Variables
(themodor-with-color-variables
  (custom-theme-set-variables
   'themodor
;;;;; ansi-color
   `(ansi-color-names-vector [,themodor-bg ,themodor-red ,themodor-green ,themodor-yellow
                                          ,themodor-blue ,themodor-magenta ,themodor-cyan ,themodor-fg])
;;;;; company-quickhelp
   `(company-quickhelp-color-background ,themodor-bg+1)
   `(company-quickhelp-color-foreground ,themodor-fg)
;;;;; fill-column-indicator
   `(fci-rule-color ,themodor-bg-05)
;;;;; nrepl-client
   `(nrepl-message-colors
     '(,themodor-red ,themodor-orange ,themodor-yellow ,themodor-green ,themodor-green+4
       ,themodor-cyan ,themodor-blue+1 ,themodor-magenta))
;;;;; pdf-tools
   `(pdf-view-midnight-colors '(,themodor-fg . ,themodor-bg-05))
;;;;; vc-annotate
   `(vc-annotate-color-map
     '(( 20. . ,themodor-red-1)
       ( 40. . ,themodor-red)
       ( 60. . ,themodor-orange)
       ( 80. . ,themodor-yellow-2)
       (100. . ,themodor-yellow-1)
       (120. . ,themodor-yellow)
       (140. . ,themodor-green-2)
       (160. . ,themodor-green)
       (180. . ,themodor-green+1)
       (200. . ,themodor-green+2)
       (220. . ,themodor-green+3)
       (240. . ,themodor-green+4)
       (260. . ,themodor-cyan)
       (280. . ,themodor-blue-2)
       (300. . ,themodor-blue-1)
       (320. . ,themodor-blue)
       (340. . ,themodor-blue+1)
       (360. . ,themodor-magenta)))
   `(vc-annotate-very-old-color ,themodor-magenta)
   `(vc-annotate-background ,themodor-bg-1)
   ))

;;; Footer

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide 'themodor)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (require 'rainbow-mode nil t) (rainbow-mode 1))
;; End:
;;; themodor.el ends here
