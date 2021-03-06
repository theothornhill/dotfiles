;;; themodor-dark.el --- Personal theme for emacs  -*- lexical-binding: t; -*-

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

(deftheme themodor-dark
  "The Themodor-Dark theme - dark variant")

(defgroup themodor-dark-theme nil
  "Themodor-Dark theme."
  :group 'faces
  :prefix "themodor-dark-"
  :tag "Themodor-Dark theme")

;;;###autoload
(defcustom themodor-dark-override-colors-alist '()
  "Place to override default theme colors.
You can override a subset of the theme's default colors by
defining them in this alist."
  :group 'themodor-dark-theme
  :type '(alist
          :key-type (string :tag "Name")
          :value-type (string :tag " Hex")))

(defvar themodor-dark-use-variable-pitch nil
  "When non-nil, use variable pitch face for some headings and titles.")

(defvar themodor-dark-scale-org-headlines nil
  "Whether `org-mode' headlines should be scaled.")

(defvar themodor-dark-scale-outline-headlines nil
  "Whether `outline-mode' headlines should be scaled.")

(defcustom themodor-dark-height-minus-1 0.8
  "Font size -1."
  :type 'number
  :group 'themodor-dark-theme
  :package-version '(themodor-dark . "0.1"))

(defcustom themodor-dark-height-plus-1 1.1
  "Font size +1."
  :type 'number
  :group 'themodor-dark-theme
  :package-version '(themodor-dark . "0.1"))

(defcustom themodor-dark-height-plus-2 1.15
  "Font size +2."
  :type 'number
  :group 'themodor-dark-theme
  :package-version '(themodor-dark . "0.1"))

(defcustom themodor-dark-height-plus-3 1.2
  "Font size +3."
  :type 'number
  :group 'themodor-dark-theme
  :package-version '(themodor-dark . "0.1"))

(defcustom themodor-dark-height-plus-4 1.3
  "Font size +4."
  :type 'number
  :group 'themodor-dark-theme
  :package-version '(themodor-dark . "0.1"))

;;; Color Palette

(defvar themodor-dark-default-colors-alist
  '(("themodor-dark-fg-1"     . "#656555")
    ("themodor-dark-fg"       . "#C5C8C6")
    ("themodor-dark-fg+1"     . "#FFFFEF")

    ("themodor-dark-bg-1"     . "#07090E")
    ("themodor-dark-bg-08"    . "#303030")
    ("themodor-dark-bg-05"    . "#383838")
    ("themodor-dark-bg"       . "#0D1117")
    ("themodor-dark-bg+05"    . "#0D1116")
    ("themodor-dark-bg+1"     . "#161B22")
    ("themodor-dark-bg+2"     . "#1D232B")
    ("themodor-dark-bg+3"     . "#6F6F6F")

    ("themodor-dark-grey"     . "#5A5B5A")
    ("themodor-dark-brown"    . "#DBCEA4")

    ("themodor-dark-red-4"    . "#B73F45")
    ("themodor-dark-red-3"    . "#B74C51")
    ("themodor-dark-red-2"    . "#C26065")
    ("themodor-dark-red-1"    . "#C17B7E")
    ("themodor-dark-red"      . "#D6867A")
    ("themodor-dark-red+1"    . "#DCA3A3")

    ("themodor-dark-purple"   . "#B294BB")

    ("themodor-dark-orange"   . "#DE935F")

    ("themodor-dark-yellow-2" . "#D0BF8F")
    ("themodor-dark-yellow-1" . "#E0CF9F")
    ("themodor-dark-yellow"   . "#C1AE7C")

    ("themodor-dark-green-2"  . "#238636")
    ("themodor-dark-green-1"  . "#228769")
    ("themodor-dark-green"    . "#3FB68B")
    ("themodor-dark-green+1"  . "#8FB28F")
    ("themodor-dark-green+2"  . "#9FC59F")
    ("themodor-dark-green+3"  . "#AFD8AF")
    ("themodor-dark-green+4"  . "#BFEBBF")

    ("themodor-dark-cyan"     . "#8ABEB7")

    ("themodor-dark-blue+1"   . "#88DDFF")
    ("themodor-dark-blue"     . "#389EDB")
    ("themodor-dark-blue-1"   . "#718CBC")
    ("themodor-dark-blue-2"   . "#6CA0A3")
    ("themodor-dark-blue-3"   . "#5C888B")
    ("themodor-dark-blue-4"   . "#4C7073")
    ("themodor-dark-blue-5"   . "#366060")

    ("themodor-dark-magenta"  . "#C9B4CF")

    ("themodor-dark-added"             . "#172E20")
    ("themodor-dark-changed"           . "#16252d")
    ("themodor-dark-removed"           . "#371A1E")
    ("themodor-dark-refine-added"      . "#3CC156")
    ("themodor-dark-refine-changed"    . "#888811")
    ("themodor-dark-refine-removed"    . "#BA3434")
    ("themodor-dark-indicator-added"   . "#172E20")
    ("themodor-dark-indicator-removed" . "#371A1E")
    )
  "List of Themodor-Dark colors.
Each element has the form (NAME . HEX).
`+N' suffixes indicate a color is lighter.
`-N' suffixes indicate a color is darker.")

(defmacro themodor-dark-with-color-variables (&rest body)
  "`let' bind all colors defined in `themodor-dark-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   (append themodor-dark-default-colors-alist
                           themodor-dark-override-colors-alist))
         (z-variable-pitch (if themodor-dark-use-variable-pitch
                               'variable-pitch 'default)))
     ,@body))

;;; Theme Faces
(themodor-dark-with-color-variables
  (custom-theme-set-faces
   'themodor-dark
;;;; Built-in
;;;;; basic coloring
   '(button ((t (:underline t))))
   `(link ((t (:foreground ,themodor-dark-yellow :underline t :weight bold))))
   `(link-visited ((t (:foreground ,themodor-dark-yellow-2 :underline t :weight normal))))
   `(default ((t (:foreground ,themodor-dark-fg :background ,themodor-dark-bg))))
   `(cursor ((t (:foreground ,themodor-dark-fg :background ,themodor-dark-fg+1))))
   `(widget-field ((t (:foreground ,themodor-dark-fg :background ,themodor-dark-bg+3))))
   `(escape-glyph ((t (:foreground ,themodor-dark-yellow :weight bold))))
   `(fringe ((t (:foreground ,themodor-dark-fg :background ,themodor-dark-bg))))
   `(header-line ((t (:foreground ,themodor-dark-yellow
                                  :background ,themodor-dark-bg-1
                                  :box (:line-width -1 :style released-button)
                                  :extend t))))
   `(highlight ((t (:background ,themodor-dark-bg-1))))
   `(success ((t (:foreground ,themodor-dark-green))))
   `(warning ((t (:foreground ,themodor-dark-orange))))
   `(error ((t (:foreground ,themodor-dark-red))))
   `(tooltip ((t (:foreground ,themodor-dark-fg :background ,themodor-dark-bg+1))))
;;;;; modeline
   `(mode-line ((t (:foreground ,themodor-dark-fg :background ,themodor-dark-bg :box ,themodor-dark-bg+2))))
   `(mode-line-buffer-id ((t :inherit bold)))
   `(mode-line-emphasis ((t :inherit bold :foreground ,themodor-dark-blue)))
   `(mode-line-highlight ((t :foreground ,themodor-dark-blue :box (:line-width -1 :style pressed-button))))
   `(mode-line-inactive ((t (:foreground ,themodor-dark-fg :background ,themodor-dark-bg+2 :box ,themodor-dark-bg+2))))
;;;;; compilation
   `(compilation-column-face ((t (:foreground ,themodor-dark-yellow))))
   `(compilation-column-number ((t (:foreground ,themodor-dark-yellow))))
   `(compilation-enter-directory-face ((t (:foreground ,themodor-dark-green))))
   `(compilation-error-face ((t (:foreground ,themodor-dark-red-1 :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,themodor-dark-fg))))
   `(compilation-info-face ((t (:foreground ,themodor-dark-blue))))
   `(compilation-info ((t (:foreground ,themodor-dark-green+4 :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,themodor-dark-green))))
   `(compilation-line-face ((t (:foreground ,themodor-dark-yellow))))
   `(compilation-line-number ((t (:foreground ,themodor-dark-yellow))))
   `(compilation-message-face ((t (:foreground ,themodor-dark-blue))))
   `(compilation-warning-face ((t (:foreground ,themodor-dark-orange :weight bold :underline t))))
   `(compilation-mode-line-exit ((t (:foreground ,themodor-dark-green+2 :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,themodor-dark-red :weight bold))))
   `(compilation-mode-line-run ((t (:foreground ,themodor-dark-yellow :weight bold))))
;;;;; completions
   `(completions-annotations ((t (:foreground ,themodor-dark-fg-1))))
;;;;; customize
   `(custom-variable-tag ((t (:foreground ,themodor-dark-blue :weight bold))))
   `(custom-group-tag ((t (:foreground ,themodor-dark-blue :weight bold :height 1.2))))
   `(custom-state ((t (:foreground ,themodor-dark-green+4))))
;;;;; display-fill-column-indicator
   `(fill-column-indicator ((,class :foreground ,themodor-dark-bg-05 :weight semilight)))
;;;;; grep
   `(grep-context-face ((t (:foreground ,themodor-dark-fg))))
   `(grep-error-face ((t (:foreground ,themodor-dark-red-1 :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,themodor-dark-blue))))
   `(grep-match-face ((t (:foreground ,themodor-dark-orange :weight bold))))
   `(match ((t (:background ,themodor-dark-bg-1 :foreground ,themodor-dark-orange :weight bold))))
;;;;; hi-lock
   `(hi-blue    ((t (:background ,themodor-dark-cyan    :foreground ,themodor-dark-bg-1))))
   `(hi-green   ((t (:background ,themodor-dark-green+4 :foreground ,themodor-dark-bg-1))))
   `(hi-pink    ((t (:background ,themodor-dark-magenta :foreground ,themodor-dark-bg-1))))
   `(hi-yellow  ((t (:background ,themodor-dark-yellow  :foreground ,themodor-dark-bg-1))))
   `(hi-blue-b  ((t (:foreground ,themodor-dark-blue    :weight     bold))))
   `(hi-green-b ((t (:foreground ,themodor-dark-green+2 :weight     bold))))
   `(hi-red-b   ((t (:foreground ,themodor-dark-red     :weight     bold))))
;;;;; info
   `(Info-quoted ((t (:inherit font-lock-constant-face))))
;;;;; isearch
   `(isearch ((t (:foreground ,themodor-dark-yellow-2 :weight bold :background ,themodor-dark-bg))))
   `(isearch-fail ((t (:foreground ,themodor-dark-fg :background ,themodor-dark-red-4))))
   `(lazy-highlight ((t (:foreground ,themodor-dark-yellow-2 :weight bold :background ,themodor-dark-bg))))

   `(menu ((t (:foreground ,themodor-dark-fg :background ,themodor-dark-bg))))
   `(minibuffer-prompt ((t (:foreground ,themodor-dark-yellow))))
;;;;; modeline
   `(mode-line ((t (:foreground ,themodor-dark-fg :background ,themodor-dark-bg :box ,themodor-dark-bg+2))))
   `(mode-line-buffer-id ((t :inherit bold)))
   `(mode-line-emphasis ((t :inherit bold :foreground ,themodor-dark-blue)))
   `(mode-line-highlight ((t :foreground ,themodor-dark-blue :box (:line-width -1 :style pressed-button))))
   `(mode-line-inactive ((t (:foreground ,themodor-dark-fg :background ,themodor-dark-bg+1 :box ,themodor-dark-bg+2))))

   `(region ((,class (:background ,themodor-dark-bg+1 :extend t))
             (t :inverse-video t)))
   `(secondary-selection ((t (:background ,themodor-dark-bg+2))))
   `(trailing-whitespace ((t (:background ,themodor-dark-red))))
   `(vertical-border ((t (:foreground ,themodor-dark-fg))))
;;;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,themodor-dark-red))))
   `(font-lock-comment-face ((t (:foreground ,themodor-dark-grey :slant italic))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,themodor-dark-grey :slant italic))))
   `(font-lock-constant-face ((t (:foreground ,themodor-dark-orange))))
   `(font-lock-doc-face ((t (:foreground ,themodor-dark-grey))))
   `(font-lock-function-name-face ((t (:foreground ,themodor-dark-green))))
   `(font-lock-keyword-face ((t (:foreground ,themodor-dark-blue))))
   `(font-lock-negation-char-face ((t (:foreground ,themodor-dark-yellow :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,themodor-dark-blue+1))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,themodor-dark-yellow :weight bold))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,themodor-dark-green :weight bold))))
   `(font-lock-string-face ((t (:foreground ,themodor-dark-brown))))
   `(font-lock-type-face ((t (:foreground ,themodor-dark-green-1))))
   `(font-lock-variable-name-face ((t (:foreground ,themodor-dark-blue+1))))
   `(font-lock-warning-face ((t (:foreground ,themodor-dark-yellow-2 :weight bold))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))
;;;;; line numbers (Emacs 26.1 and above)
   `(line-number ((t (:foreground ,themodor-dark-bg+3 :background ,themodor-dark-bg-05))))
   `(line-number-current-line ((t (:inherit line-number :foreground ,themodor-dark-yellow-2))))
;;;;; man
   '(Man-overstrike ((t (:inherit font-lock-keyword-face))))
   '(Man-underline  ((t (:inherit (font-lock-string-face underline)))))
;;;;; woman
   '(woman-bold   ((t (:inherit font-lock-keyword-face))))
   '(woman-italic ((t (:inherit (font-lock-string-face italic)))))
;;;; Third-party
;;;;; helm
`(helm-header
     ((,class (:foreground ,themodor-dark-fg
                      :background ,themodor-dark-bg
                      :underline nil
                      :box nil
                      :extend t))))
   `(helm-source-header
     ((,class (:foreground ,themodor-dark-green
                      :background ,themodor-dark-bg
                      :underline nil
                      :box ,themodor-dark-bg
                      :height 1.8
                      :extend t))))
   `(helm-selection ((t (:background ,themodor-dark-bg+2 :underline nil))))
   `(helm-selection-line ((t (:background ,themodor-dark-bg+1))))
   `(helm-visible-mark ((t (:foreground ,themodor-dark-bg :background ,themodor-dark-yellow-2))))
   `(helm-candidate-number ((t (:foreground ,themodor-dark-green+4 :background ,themodor-dark-bg-1))))
   `(helm-separator ((t (:foreground ,themodor-dark-red :background ,themodor-dark-bg))))
   `(helm-time-zone-current ((t (:foreground ,themodor-dark-green+2 :background ,themodor-dark-bg))))
   `(helm-time-zone-home ((t (:foreground ,themodor-dark-red :background ,themodor-dark-bg))))
   `(helm-bookmark-addressbook ((t (:foreground ,themodor-dark-orange :background ,themodor-dark-bg))))
   `(helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))
   `(helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))
   `(helm-bookmark-gnus ((t (:foreground ,themodor-dark-magenta :background ,themodor-dark-bg))))
   `(helm-bookmark-info ((t (:foreground ,themodor-dark-green+2 :background ,themodor-dark-bg))))
   `(helm-bookmark-man ((t (:foreground ,themodor-dark-yellow :background ,themodor-dark-bg))))
   `(helm-bookmark-w3m ((t (:foreground ,themodor-dark-magenta :background ,themodor-dark-bg))))
   `(helm-buffer-not-saved ((t (:foreground ,themodor-dark-red :background ,themodor-dark-bg))))
   `(helm-buffer-process ((t (:foreground ,themodor-dark-cyan :background ,themodor-dark-bg))))
   `(helm-buffer-saved-out ((t (:foreground ,themodor-dark-fg :background ,themodor-dark-bg))))
   `(helm-buffer-size ((t (:foreground ,themodor-dark-fg-1 :background ,themodor-dark-bg))))
   `(helm-ff-directory ((t (:foreground ,themodor-dark-cyan :background ,themodor-dark-bg :weight bold))))
   `(helm-ff-file ((t (:foreground ,themodor-dark-fg :background ,themodor-dark-bg :weight normal))))
   `(helm-ff-executable ((t (:foreground ,themodor-dark-green+2 :background ,themodor-dark-bg :weight normal))))
   `(helm-ff-invalid-symlink ((t (:foreground ,themodor-dark-red :background ,themodor-dark-bg :weight bold))))
   `(helm-ff-symlink ((t (:foreground ,themodor-dark-yellow :background ,themodor-dark-bg :weight bold))))
   `(helm-ff-prefix ((t (:foreground ,themodor-dark-bg :background ,themodor-dark-yellow :weight normal))))
   `(helm-ff-file-extension ((,class :foreground ,themodor-dark-fg)))
   `(helm-ff-dotted-directory ((,class :inherit bold :background ,themodor-dark-bg :foreground ,themodor-dark-fg)))
   `(helm-ff-dotted-symlink-directory ((,class :inherit (button helm-ff-dotted-directory))))
   `(helm-grep-cmd-line ((t (:foreground ,themodor-dark-cyan :background ,themodor-dark-bg))))
   `(helm-grep-file ((t (:foreground ,themodor-dark-fg :background ,themodor-dark-bg))))
   `(helm-grep-finish ((t (:foreground ,themodor-dark-green+2 :background ,themodor-dark-bg))))
   `(helm-grep-lineno ((t (:foreground ,themodor-dark-fg-1 :background ,themodor-dark-bg))))
   `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
   `(helm-grep-running ((t (:foreground ,themodor-dark-red :background ,themodor-dark-bg))))
   `(helm-match ((t (:foreground ,themodor-dark-orange :background ,themodor-dark-bg-1 :weight bold))))
   `(helm-moccur-buffer ((t (:foreground ,themodor-dark-cyan :background ,themodor-dark-bg))))
   `(helm-mu-contacts-address-face ((t (:foreground ,themodor-dark-fg-1 :background ,themodor-dark-bg))))
   `(helm-mu-contacts-name-face ((t (:foreground ,themodor-dark-fg :background ,themodor-dark-bg))))
;;;;; ace-jump
   `(ace-jump-face-background
     ((t (:foreground ,themodor-dark-fg-1 :background ,themodor-dark-bg :inverse-video nil))))
   `(ace-jump-face-foreground
     ((t (:foreground ,themodor-dark-green+2 :background ,themodor-dark-bg :inverse-video nil))))
;;;;; ace-window
   `(aw-background-face
     ((t (:foreground ,themodor-dark-fg-1 :background ,themodor-dark-bg :inverse-video nil))))
   `(aw-leading-char-face ((t (:inherit aw-mode-line-face))))
;;;;; company-mode
   `(company-tooltip ((t (:foreground ,themodor-dark-fg :background ,themodor-dark-bg+05))))
   `(company-tooltip-annotation ((t (:foreground ,themodor-dark-orange :background ,themodor-dark-bg+1))))
   `(company-tooltip-annotation-selection ((t (:foreground ,themodor-dark-orange :background ,themodor-dark-bg-1))))
   `(company-tooltip-selection ((t (:foreground ,themodor-dark-fg :background ,themodor-dark-bg+1))))
   `(company-tooltip-mouse ((t (:background ,themodor-dark-bg-1))))
   `(company-tooltip-common ((t (:foreground ,themodor-dark-green+2))))
   `(company-tooltip-common-selection ((t (:foreground ,themodor-dark-green+2))))
   `(company-scrollbar-fg ((t (:background ,themodor-dark-bg-1))))
   `(company-scrollbar-bg ((t (:background ,themodor-dark-bg+2))))
   `(company-preview ((t (:background ,themodor-dark-green+2))))
   `(company-preview-common ((t (:foreground ,themodor-dark-green+2 :background ,themodor-dark-bg-1))))
;;;;; cider
   `(cider-result-overlay-face ((t (:background unspecified))))
   `(cider-enlightened-face ((t (:box (:color ,themodor-dark-orange :line-width -1)))))
   `(cider-enlightened-local-face ((t (:weight bold :foreground ,themodor-dark-green+1))))
   `(cider-deprecated-face ((t (:background ,themodor-dark-yellow-2))))
   `(cider-instrumented-face ((t (:box (:color ,themodor-dark-red :line-width -1)))))
   `(cider-traced-face ((t (:box (:color ,themodor-dark-cyan :line-width -1)))))
   `(cider-test-failure-face ((t (:background ,themodor-dark-red-4))))
   `(cider-test-error-face ((t (:background ,themodor-dark-magenta))))
   `(cider-test-success-face ((t (:background ,themodor-dark-green-2))))
   `(cider-fringe-good-face ((t (:foreground ,themodor-dark-green+4))))
;;;;; SLIME
   `(sly-mrepl-output-face ((t (:foreground ,themodor-dark-blue))))
   `(slime-repl-inputed-output-face ((t (:foreground ,themodor-dark-green))))
   `(slime-error-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,themodor-dark-red)))
      (t
       (:underline ,themodor-dark-red))))
   `(slime-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,themodor-dark-orange)))
      (t
       (:underline ,themodor-dark-orange))))
   `(slime-style-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,themodor-dark-yellow)))
      (t
       (:underline ,themodor-dark-yellow))))
   `(slime-note-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,themodor-dark-green)))
      (t
       (:underline ,themodor-dark-green))))
   `(slime-highlight-face ((t (:inherit highlight))))
;;;;; diff
   `(diff-added             ((t (:background ,themodor-dark-added))))
   `(diff-changed           ((t (:background ,themodor-dark-changed))))
   `(diff-removed           ((t (:background ,themodor-dark-removed))))
   `(diff-refine-added      ((t (:foreground ,themodor-dark-refine-added))))
   `(diff-refine-changed    ((t (:foreground ,themodor-dark-refine-changed))))
   `(diff-refine-removed    ((t (:foreground ,themodor-dark-refine-removed))))
   `(diff-indicator-added   ((t (:background ,themodor-dark-indicator-added))))
   `(diff-indicator-removed ((t (:background ,themodor-dark-indicator-removed))))
   `(diff-header ((,class (:background ,themodor-dark-bg+1))
                  (t (:background ,themodor-dark-fg :foreground ,themodor-dark-bg))))
   `(diff-file-header
     ((,class (:background ,themodor-dark-bg+1 :foreground ,themodor-dark-fg :weight bold))
      (t (:background ,themodor-dark-fg :foreground ,themodor-dark-bg :weight bold))))
;;;;; diff-hl
   `(diff-hl-change ((,class (:foreground ,themodor-dark-blue :background ,themodor-dark-changed))))
   `(diff-hl-delete ((,class (:foreground ,themodor-dark-red+1 :background ,themodor-dark-removed))))
   `(diff-hl-insert ((,class (:foreground ,themodor-dark-green+1 :background ,themodor-dark-added))))
   `(diff-hl-reverted-hunk-highlight ((,class (:background ,themodor-dark-yellow))))
;;;;; dim-autoload
   `(dim-autoload-cookie-line ((t :foreground ,themodor-dark-bg+1)))
;;;;; dired+
   `(diredp-display-msg ((t (:foreground ,themodor-dark-blue))))
   `(diredp-compressed-file-suffix ((t (:foreground ,themodor-dark-orange))))
   `(diredp-date-time ((t (:foreground ,themodor-dark-magenta))))
   `(diredp-deletion ((t (:foreground ,themodor-dark-yellow))))
   `(diredp-deletion-file-name ((t (:foreground ,themodor-dark-red))))
   `(diredp-dir-heading ((t (:foreground ,themodor-dark-blue :background ,themodor-dark-bg-1))))
   `(diredp-dir-priv ((t (:foreground ,themodor-dark-cyan))))
   `(diredp-exec-priv ((t (:foreground ,themodor-dark-red))))
   `(diredp-executable-tag ((t (:foreground ,themodor-dark-green+1))))
   `(diredp-file-name ((t (:foreground ,themodor-dark-blue))))
   `(diredp-file-suffix ((t (:foreground ,themodor-dark-green))))
   `(diredp-flag-mark ((t (:foreground ,themodor-dark-yellow))))
   `(diredp-flag-mark-line ((t (:foreground ,themodor-dark-orange))))
   `(diredp-ignored-file-name ((t (:foreground ,themodor-dark-red))))
   `(diredp-link-priv ((t (:foreground ,themodor-dark-yellow))))
   `(diredp-mode-line-flagged ((t (:foreground ,themodor-dark-yellow))))
   `(diredp-mode-line-marked ((t (:foreground ,themodor-dark-orange))))
   `(diredp-no-priv ((t (:foreground ,themodor-dark-fg))))
   `(diredp-number ((t (:foreground ,themodor-dark-green+1))))
   `(diredp-other-priv ((t (:foreground ,themodor-dark-yellow-1))))
   `(diredp-rare-priv ((t (:foreground ,themodor-dark-red-1))))
   `(diredp-read-priv ((t (:foreground ,themodor-dark-green-2))))
   `(diredp-symlink ((t (:foreground ,themodor-dark-yellow))))
   `(diredp-write-priv ((t (:foreground ,themodor-dark-magenta))))
;;;;; dired-async
   `(dired-async-failures ((t (:foreground ,themodor-dark-red :weight bold))))
   `(dired-async-message ((t (:foreground ,themodor-dark-yellow :weight bold))))
   `(dired-async-mode-message ((t (:foreground ,themodor-dark-yellow))))
;;;;; diredfl
   `(diredfl-compressed-file-suffix ((t (:foreground ,themodor-dark-orange))))
   `(diredfl-date-time ((t (:foreground ,themodor-dark-magenta))))
   `(diredfl-deletion ((t (:foreground ,themodor-dark-yellow))))
   `(diredfl-deletion-file-name ((t (:foreground ,themodor-dark-red))))
   `(diredfl-dir-heading ((t (:foreground ,themodor-dark-blue :background ,themodor-dark-bg-1))))
   `(diredfl-dir-priv ((t (:foreground ,themodor-dark-cyan))))
   `(diredfl-exec-priv ((t (:foreground ,themodor-dark-red))))
   `(diredfl-executable-tag ((t (:foreground ,themodor-dark-green+1))))
   `(diredfl-file-name ((t (:foreground ,themodor-dark-blue))))
   `(diredfl-file-suffix ((t (:foreground ,themodor-dark-green))))
   `(diredfl-flag-mark ((t (:foreground ,themodor-dark-yellow))))
   `(diredfl-flag-mark-line ((t (:foreground ,themodor-dark-orange))))
   `(diredfl-ignored-file-name ((t (:foreground ,themodor-dark-red))))
   `(diredfl-link-priv ((t (:foreground ,themodor-dark-yellow))))
   `(diredfl-no-priv ((t (:foreground ,themodor-dark-fg))))
   `(diredfl-number ((t (:foreground ,themodor-dark-green+1))))
   `(diredfl-other-priv ((t (:foreground ,themodor-dark-yellow-1))))
   `(diredfl-rare-priv ((t (:foreground ,themodor-dark-red-1))))
   `(diredfl-read-priv ((t (:foreground ,themodor-dark-green-1))))
   `(diredfl-symlink ((t (:foreground ,themodor-dark-yellow))))
   `(diredfl-write-priv ((t (:foreground ,themodor-dark-magenta))))
;;;;; ediff
   `(ediff-current-diff-A ((t (:foreground ,themodor-dark-fg :background ,themodor-dark-red-4))))
   `(ediff-current-diff-Ancestor ((t (:foreground ,themodor-dark-fg :background ,themodor-dark-red-4))))
   `(ediff-current-diff-B ((t (:foreground ,themodor-dark-fg :background ,themodor-dark-green-2))))
   `(ediff-current-diff-C ((t (:foreground ,themodor-dark-fg :background ,themodor-dark-blue-5))))
   `(ediff-even-diff-A ((t (:background ,themodor-dark-bg+1))))
   `(ediff-even-diff-Ancestor ((t (:background ,themodor-dark-bg+1))))
   `(ediff-even-diff-B ((t (:background ,themodor-dark-bg+1))))
   `(ediff-even-diff-C ((t (:background ,themodor-dark-bg+1))))
   `(ediff-fine-diff-A ((t (:foreground ,themodor-dark-fg :background ,themodor-dark-red-2 :weight bold))))
   `(ediff-fine-diff-Ancestor ((t (:foreground ,themodor-dark-fg :background ,themodor-dark-red-2 weight bold))))
   `(ediff-fine-diff-B ((t (:foreground ,themodor-dark-fg :background ,themodor-dark-green :weight bold))))
   `(ediff-fine-diff-C ((t (:foreground ,themodor-dark-fg :background ,themodor-dark-blue-3 :weight bold ))))
   `(ediff-odd-diff-A ((t (:background ,themodor-dark-bg+2))))
   `(ediff-odd-diff-Ancestor ((t (:background ,themodor-dark-bg+2))))
   `(ediff-odd-diff-B ((t (:background ,themodor-dark-bg+2))))
   `(ediff-odd-diff-C ((t (:background ,themodor-dark-bg+2))))
;;;;; erc
   `(erc-action-face ((t (:inherit erc-default-face))))
   `(erc-bold-face ((t (:weight bold))))
   `(erc-current-nick-face ((t (:foreground ,themodor-dark-blue :weight bold))))
   `(erc-dangerous-host-face ((t (:inherit font-lock-warning-face))))
   `(erc-default-face ((t (:foreground ,themodor-dark-fg))))
   `(erc-direct-msg-face ((t (:inherit erc-default-face))))
   `(erc-error-face ((t (:inherit font-lock-warning-face))))
   `(erc-fool-face ((t (:inherit erc-default-face))))
   `(erc-highlight-face ((t (:inherit hover-highlight))))
   `(erc-input-face ((t (:foreground ,themodor-dark-yellow))))
   `(erc-keyword-face ((t (:foreground ,themodor-dark-blue :weight bold))))
   `(erc-nick-default-face ((t (:foreground ,themodor-dark-cyan :weight bold))))
   `(erc-my-nick-face ((t (:foreground ,themodor-dark-red :weight bold))))
   `(erc-nick-msg-face ((t (:inherit erc-default-face))))
   `(erc-notice-face ((t (:foreground ,themodor-dark-grey))))
   `(erc-pal-face ((t (:foreground ,themodor-dark-orange :weight bold))))
   `(erc-prompt-face ((t (:foreground ,themodor-dark-orange :background ,themodor-dark-bg :weight bold))))
   `(erc-timestamp-face ((t (:foreground ,themodor-dark-green+4))))
   `(erc-underline-face ((t (:underline t))))
;;;;; ert
   `(ert-test-result-expected ((t (:foreground ,themodor-dark-green+4 :background ,themodor-dark-bg))))
   `(ert-test-result-unexpected ((t (:foreground ,themodor-dark-red :background ,themodor-dark-bg))))
;;;;; eshell
   `(eshell-prompt ((t (:foreground ,themodor-dark-green :weight bold))))
   `(eshell-ls-archive ((t (:foreground ,themodor-dark-red-1 :weight bold))))
   `(eshell-ls-backup ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-clutter ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-directory ((t (:foreground ,themodor-dark-blue+1 :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,themodor-dark-red+1 :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,themodor-dark-fg))))
   `(eshell-ls-missing ((t (:inherit font-lock-warning-face))))
   `(eshell-ls-product ((t (:inherit font-lock-doc-face))))
   `(eshell-ls-special ((t (:foreground ,themodor-dark-yellow :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,themodor-dark-cyan :weight bold))))
;;;;; flycheck
   `(flycheck-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,themodor-dark-red-1) :inherit unspecified))
      (t (:foreground ,themodor-dark-red-1 :weight bold :underline t))))
   `(flycheck-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,themodor-dark-yellow) :inherit unspecified))
      (t (:foreground ,themodor-dark-yellow :weight bold :underline t))))
   `(flycheck-info
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,themodor-dark-cyan) :inherit unspecified))
      (t (:foreground ,themodor-dark-cyan :weight bold :underline t))))
   `(flycheck-fringe-error ((t (:foreground ,themodor-dark-red-1 :weight bold))))
   `(flycheck-fringe-warning ((t (:foreground ,themodor-dark-yellow :weight bold))))
   `(flycheck-fringe-info ((t (:foreground ,themodor-dark-cyan :weight bold))))
;;;;; flymake
   `(flymake-errline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,themodor-dark-red)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,themodor-dark-red-1 :weight bold :underline t))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,themodor-dark-orange)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,themodor-dark-orange :weight bold :underline t))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,themodor-dark-green)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,themodor-dark-green-2 :weight bold :underline t))))
;;;;; flyspell
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,themodor-dark-orange) :inherit unspecified))
      (t (:foreground ,themodor-dark-orange :weight bold :underline t))))
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,themodor-dark-red) :inherit unspecified))
      (t (:foreground ,themodor-dark-red-1 :weight bold :underline t))))
;;;;; git-commit
   `(git-commit-comment-action  ((,class (:foreground ,themodor-dark-green+1 :weight bold))))
   `(git-commit-comment-branch  ((,class (:foreground ,themodor-dark-blue+1  :weight bold)))) ; obsolete
   `(git-commit-comment-branch-local  ((,class (:foreground ,themodor-dark-blue+1  :weight bold))))
   `(git-commit-comment-branch-remote ((,class (:foreground ,themodor-dark-green  :weight bold))))
   `(git-commit-comment-heading ((,class (:foreground ,themodor-dark-yellow  :weight bold))))
;;;;; git-gutter
   `(git-gutter:added ((t (:foreground ,themodor-dark-green :weight bold :inverse-video t))))
   `(git-gutter:deleted ((t (:foreground ,themodor-dark-red :weight bold :inverse-video t))))
   `(git-gutter:modified ((t (:foreground ,themodor-dark-magenta :weight bold :inverse-video t))))
   `(git-gutter:unchanged ((t (:foreground ,themodor-dark-fg :weight bold :inverse-video t))))
;;;;; git-gutter-fr
   `(git-gutter-fr:added ((t (:foreground ,themodor-dark-green  :weight bold))))
   `(git-gutter-fr:deleted ((t (:foreground ,themodor-dark-red :weight bold))))
   `(git-gutter-fr:modified ((t (:foreground ,themodor-dark-magenta :weight bold))))
;;;;; git-rebase
   `(git-rebase-hash ((t (:foreground, themodor-dark-orange))))
;;;;; highlight-numbers
   `(highlight-numbers-number ((t (:foreground ,themodor-dark-blue))))
;;;;; highlight-symbol
   `(highlight-symbol-face ((t (:background ,themodor-dark-bg+2))))
;;;;; highlight-thing
   `(highlight-thing ((t (:background ,themodor-dark-bg+2))))
;;;;; hl-line-mode
   `(hl-line-face ((,class (:background ,themodor-dark-bg-05))
                   (t :weight bold)))
   `(hl-line ((,class (:background ,themodor-dark-bg-05 :extend t)) ; old emacsen
              (t :weight bold)))
;;;;; hl-sexp
   `(hl-sexp-face ((,class (:background ,themodor-dark-bg+1))
                   (t :weight bold)))
;;;;; info+
   `(info-command-ref-item ((t (:background ,themodor-dark-bg-1 :foreground ,themodor-dark-orange))))
   `(info-constant-ref-item ((t (:background ,themodor-dark-bg-1 :foreground ,themodor-dark-magenta))))
   `(info-double-quoted-name ((t (:inherit font-lock-comment-face))))
   `(info-file ((t (:background ,themodor-dark-bg-1 :foreground ,themodor-dark-yellow))))
   `(info-function-ref-item ((t (:background ,themodor-dark-bg-1 :inherit font-lock-function-name-face))))
   `(info-macro-ref-item ((t (:background ,themodor-dark-bg-1 :foreground ,themodor-dark-yellow))))
   `(info-menu ((t (:foreground ,themodor-dark-yellow))))
   `(info-quoted-name ((t (:inherit font-lock-constant-face))))
   `(info-reference-item ((t (:background ,themodor-dark-bg-1))))
   `(info-single-quote ((t (:inherit font-lock-keyword-face))))
   `(info-special-form-ref-item ((t (:background ,themodor-dark-bg-1 :foreground ,themodor-dark-yellow))))
   `(info-string ((t (:inherit font-lock-string-face))))
   `(info-syntax-class-item ((t (:background ,themodor-dark-bg-1 :foreground ,themodor-dark-blue+1))))
   `(info-user-option-ref-item ((t (:background ,themodor-dark-bg-1 :foreground ,themodor-dark-red))))
   `(info-variable-ref-item ((t (:background ,themodor-dark-bg-1 :foreground ,themodor-dark-orange))))
;;;;; iedit-mode
   `(iedit-occurrence ((t (:background ,themodor-dark-bg+2 :weight bold))))
;;;;; macrostep
   `(macrostep-gensym-1
     ((t (:foreground ,themodor-dark-green+2 :background ,themodor-dark-bg-1))))
   `(macrostep-gensym-2
     ((t (:foreground ,themodor-dark-red+1 :background ,themodor-dark-bg-1))))
   `(macrostep-gensym-3
     ((t (:foreground ,themodor-dark-blue+1 :background ,themodor-dark-bg-1))))
   `(macrostep-gensym-4
     ((t (:foreground ,themodor-dark-magenta :background ,themodor-dark-bg-1))))
   `(macrostep-gensym-5
     ((t (:foreground ,themodor-dark-yellow :background ,themodor-dark-bg-1))))
   `(macrostep-expansion-highlight-face
     ((t (:inherit highlight))))
   `(macrostep-macro-face
     ((t (:underline t))))
;;;;; markup-faces
   `(markup-anchor-face ((t (:foreground ,themodor-dark-blue+1))))
   `(markup-code-face ((t (:inherit font-lock-constant-face))))
   `(markup-command-face ((t (:foreground ,themodor-dark-yellow))))
   `(markup-emphasis-face ((t (:inherit bold))))
   `(markup-internal-reference-face ((t (:foreground ,themodor-dark-yellow-2 :underline t))))
   `(markup-list-face ((t (:foreground ,themodor-dark-fg+1))))
   `(markup-meta-face ((t (:foreground ,themodor-dark-yellow))))
   `(markup-meta-hide-face ((t (:foreground ,themodor-dark-yellow))))
   `(markup-secondary-text-face ((t (:foreground ,themodor-dark-yellow-1))))
   `(markup-title-0-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-1-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-2-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-3-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-4-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-typewriter-face ((t (:inherit font-lock-constant-face))))
   `(markup-verbatim-face ((t (:inherit font-lock-constant-face))))
   `(markup-value-face ((t (:foreground ,themodor-dark-yellow))))
;;;;; message-mode
   `(message-cited-text ((t (:inherit font-lock-comment-face))))
   `(message-header-name ((t (:foreground ,themodor-dark-green+1))))
   `(message-header-other ((t (:foreground ,themodor-dark-green))))
   `(message-header-to ((t (:foreground ,themodor-dark-yellow :weight bold))))
   `(message-header-cc ((t (:foreground ,themodor-dark-yellow :weight bold))))
   `(message-header-newsgroups ((t (:foreground ,themodor-dark-yellow :weight bold))))
   `(message-header-subject ((t (:foreground ,themodor-dark-orange :weight bold))))
   `(message-header-xheader ((t (:foreground ,themodor-dark-green))))
   `(message-mml ((t (:foreground ,themodor-dark-yellow :weight bold))))
   `(message-separator ((t (:inherit font-lock-comment-face))))
;;;;; notmuch
   `(notmuch-crypto-decryption ((t (:foreground ,themodor-dark-bg :background ,themodor-dark-magenta))))
   `(notmuch-crypto-part-header ((t (:foreground ,themodor-dark-blue+1))))
   `(notmuch-crypto-signature-bad ((t (:foreground ,themodor-dark-bg :background ,themodor-dark-red))))
   `(notmuch-crypto-signature-good ((t (:foreground ,themodor-dark-bg :background ,themodor-dark-green+1))))
   `(notmuch-crypto-signature-good-key ((t (:foreground ,themodor-dark-bg :background ,themodor-dark-orange))))
   `(notmuch-crypto-signature-unknown ((t (:foreground ,themodor-dark-bg :background ,themodor-dark-red))))
   `(notmuch-hello-logo-background ((t (:background ,themodor-dark-bg+2))))
   `(notmuch-message-summary-face ((t (:background ,themodor-dark-bg-08))))
   `(notmuch-search-flagged-face ((t (:foreground ,themodor-dark-blue+1))))
   `(notmuch-search-non-matching-authors ((t (:foreground ,themodor-dark-fg-1))))
   `(notmuch-tag-added ((t (:underline ,themodor-dark-green+1))))
   `(notmuch-tag-deleted ((t (:strike-through ,themodor-dark-red))))
   `(notmuch-tag-face ((t (:foreground ,themodor-dark-green+1))))
   `(notmuch-tag-flagged ((t (:foreground ,themodor-dark-blue+1))))
   `(notmuch-tag-unread ((t (:foreground ,themodor-dark-red))))
   `(notmuch-tree-match-author-face ((t (:foreground ,themodor-dark-green+1))))
   `(notmuch-tree-match-tag-face ((t (:foreground ,themodor-dark-green+1))))
;;;;; markdown
   `(markdown-code-face
     ((t (:foreground ,themodor-dark-green))))
;;;;; org-mode
   `(org-agenda-date-today
     ((t (:foreground ,themodor-dark-fg+1 :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((t (:inherit font-lock-comment-face))))
   `(org-archived ((t (:foreground ,themodor-dark-fg :weight bold))))
   `(org-block ((t (:background ,themodor-dark-bg+05 :extend t))))
   `(org-checkbox ((t (:background ,themodor-dark-bg+2 :foreground ,themodor-dark-fg+1
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((t (:foreground ,themodor-dark-blue :underline t))))
   `(org-deadline-announce ((t (:foreground ,themodor-dark-red-1))))
   `(org-done ((t (:weight bold :weight bold :foreground ,themodor-dark-green+3))))
   `(org-formula ((t (:foreground ,themodor-dark-yellow-2))))
   `(org-headline-done ((t (:foreground ,themodor-dark-green+3))))
   `(org-hide ((t (:foreground ,themodor-dark-bg))))
   `(org-level-1 ((t (:inherit ,z-variable-pitch :foreground ,themodor-dark-orange
                               ,@(when themodor-dark-scale-org-headlines
                                   (list :height themodor-dark-height-plus-4))))))
   `(org-level-2 ((t (:inherit ,z-variable-pitch :foreground ,themodor-dark-green+4
                               ,@(when themodor-dark-scale-org-headlines
                                   (list :height themodor-dark-height-plus-3))))))
   `(org-level-3 ((t (:inherit ,z-variable-pitch :foreground ,themodor-dark-blue-1
                               ,@(when themodor-dark-scale-org-headlines
                                   (list :height themodor-dark-height-plus-2))))))
   `(org-level-4 ((t (:inherit ,z-variable-pitch :foreground ,themodor-dark-yellow-2
                               ,@(when themodor-dark-scale-org-headlines
                                   (list :height themodor-dark-height-plus-1))))))
   `(org-level-5 ((t (:inherit ,z-variable-pitch :foreground ,themodor-dark-cyan))))
   `(org-level-6 ((t (:inherit ,z-variable-pitch :foreground ,themodor-dark-green+2))))
   `(org-level-7 ((t (:inherit ,z-variable-pitch :foreground ,themodor-dark-red-4))))
   `(org-level-8 ((t (:inherit ,z-variable-pitch :foreground ,themodor-dark-blue-4))))
   `(org-link ((t (:foreground ,themodor-dark-yellow-2 :underline t))))
   `(org-quote ((t (:background ,themodor-dark-bg+05 :extend t))))
   `(org-scheduled ((t (:foreground ,themodor-dark-green+4))))
   `(org-scheduled-previously ((t (:foreground ,themodor-dark-red))))
   `(org-scheduled-today ((t (:foreground ,themodor-dark-blue+1))))
   `(org-sexp-date ((t (:foreground ,themodor-dark-blue+1 :underline t))))
   `(org-special-keyword ((t (:inherit font-lock-comment-face))))
   `(org-table ((t (:foreground ,themodor-dark-green+2))))
   `(org-tag ((t (:weight bold :weight bold))))
   `(org-time-grid ((t (:foreground ,themodor-dark-orange))))
   `(org-todo ((t (:weight bold :foreground ,themodor-dark-red :weight bold))))
   `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   `(org-warning ((t (:weight bold :foreground ,themodor-dark-red :weight bold :underline nil))))
   `(org-column ((t (:background ,themodor-dark-bg-1))))
   `(org-column-title ((t (:background ,themodor-dark-bg-1 :underline t :weight bold))))
   `(org-mode-line-clock ((t (:foreground ,themodor-dark-fg :background ,themodor-dark-bg-1))))
   `(org-mode-line-clock-overrun ((t (:foreground ,themodor-dark-bg :background ,themodor-dark-red-1))))
   `(org-ellipsis ((t (:foreground ,themodor-dark-yellow-1 :underline t))))
   `(org-footnote ((t (:foreground ,themodor-dark-cyan :underline t))))
   `(org-document-title ((t (:inherit ,z-variable-pitch :foreground ,themodor-dark-blue
                                      :weight bold
                                      ,@(when themodor-dark-scale-org-headlines
                                          (list :height themodor-dark-height-plus-4))))))
   `(org-document-info ((t (:foreground ,themodor-dark-blue))))
   `(org-habit-ready-face ((t :background ,themodor-dark-green)))
   `(org-habit-alert-face ((t :background ,themodor-dark-yellow-1 :foreground ,themodor-dark-bg)))
   `(org-habit-clear-face ((t :background ,themodor-dark-blue-3)))
   `(org-habit-overdue-face ((t :background ,themodor-dark-red-3)))
   `(org-habit-clear-future-face ((t :background ,themodor-dark-blue-4)))
   `(org-habit-ready-future-face ((t :background ,themodor-dark-green-2)))
   `(org-habit-alert-future-face ((t :background ,themodor-dark-yellow-2 :foreground ,themodor-dark-bg)))
   `(org-habit-overdue-future-face ((t :background ,themodor-dark-red-4)))
;;;;; org-ref
   `(org-ref-ref-face ((t :underline t)))
   `(org-ref-label-face ((t :underline t)))
   `(org-ref-cite-face ((t :underline t)))
   `(org-ref-glossary-face ((t :underline t)))
   `(org-ref-acronym-face ((t :underline t)))
;;;;; outline
   `(outline-1 ((t (:inherit ,z-variable-pitch :foreground ,themodor-dark-orange
                             ,@(when themodor-dark-scale-outline-headlines
                                 (list :height themodor-dark-height-plus-4))))))
   `(outline-2 ((t (:inherit ,z-variable-pitch :foreground ,themodor-dark-green+4
                             ,@(when themodor-dark-scale-outline-headlines
                                 (list :height themodor-dark-height-plus-3))))))
   `(outline-3 ((t (:inherit ,z-variable-pitch :foreground ,themodor-dark-blue-1
                             ,@(when themodor-dark-scale-outline-headlines
                                 (list :height themodor-dark-height-plus-2))))))
   `(outline-4 ((t (:inherit ,z-variable-pitch :foreground ,themodor-dark-yellow-2
                             ,@(when themodor-dark-scale-outline-headlines
                                 (list :height themodor-dark-height-plus-1))))))
   `(outline-5 ((t (:inherit ,z-variable-pitch :foreground ,themodor-dark-cyan))))
   `(outline-6 ((t (:inherit ,z-variable-pitch :foreground ,themodor-dark-green+2))))
   `(outline-7 ((t (:inherit ,z-variable-pitch :foreground ,themodor-dark-red-4))))
   `(outline-8 ((t (:inherit ,z-variable-pitch :foreground ,themodor-dark-blue-4))))
;;;;;; paren-face
   `(parenthesis ((t (:foreground ,themodor-dark-fg-1))))
;;;;; perspective
   `(persp-selected-face ((t (:foreground ,themodor-dark-yellow-2))))
;;;;; re-builder
   `(reb-match-0 ((t (:foreground ,themodor-dark-bg :background ,themodor-dark-magenta))))
   `(reb-match-1 ((t (:foreground ,themodor-dark-bg :background ,themodor-dark-blue))))
   `(reb-match-2 ((t (:foreground ,themodor-dark-bg :background ,themodor-dark-orange))))
   `(reb-match-3 ((t (:foreground ,themodor-dark-bg :background ,themodor-dark-red))))
;;;;; selectrum
   `(selectrum-current-candidate ((t (:foreground ,themodor-dark-yellow :weight bold :background ,themodor-dark-bg+2 :underline t :inherit highlight))))
   `(selectrum-primary-highlight ((t (:foreground ,themodor-dark-green-1))))
   `(selectrum-secondary-highlight ((t (:background ,themodor-dark-green-2))))
;;;;; sh-mode
   `(sh-heredoc     ((t (:foreground ,themodor-dark-yellow :weight bold))))
   `(sh-quoted-exec ((t (:foreground ,themodor-dark-red))))
;;;;; show-paren
   `(show-paren-mismatch ((t (:foreground ,themodor-dark-red+1 :background ,themodor-dark-bg+3 :weight bold))))
   `(show-paren-match ((t (:foreground ,themodor-dark-fg :background ,themodor-dark-bg+3 :weight bold))))
;;;;; term
   `(term-color-black ((t (:foreground ,themodor-dark-bg
                                       :background ,themodor-dark-bg-1))))
   `(term-color-red ((t (:foreground ,themodor-dark-red-2
                                     :background ,themodor-dark-red-4))))
   `(term-color-green ((t (:foreground ,themodor-dark-green
                                       :background ,themodor-dark-green+2))))
   `(term-color-yellow ((t (:foreground ,themodor-dark-orange
                                        :background ,themodor-dark-yellow))))
   `(term-color-blue ((t (:foreground ,themodor-dark-blue-1
                                      :background ,themodor-dark-blue-4))))
   `(term-color-magenta ((t (:foreground ,themodor-dark-magenta
                                         :background ,themodor-dark-red))))
   `(term-color-cyan ((t (:foreground ,themodor-dark-cyan
                                      :background ,themodor-dark-blue))))
   `(term-color-white ((t (:foreground ,themodor-dark-fg
                                       :background ,themodor-dark-fg-1))))
   '(term-default-fg-color ((t (:inherit term-color-white))))
   '(term-default-bg-color ((t (:inherit term-color-black))))
;;;;; undo-tree
   `(undo-tree-visualizer-active-branch-face ((t (:foreground ,themodor-dark-fg+1 :weight bold))))
   `(undo-tree-visualizer-current-face ((t (:foreground ,themodor-dark-red-1 :weight bold))))
   `(undo-tree-visualizer-default-face ((t (:foreground ,themodor-dark-fg))))
   `(undo-tree-visualizer-register-face ((t (:foreground ,themodor-dark-yellow))))
   `(undo-tree-visualizer-unmodified-face ((t (:foreground ,themodor-dark-cyan))))
;;;;; whitespace-mode
   `(whitespace-space ((t (:background ,themodor-dark-bg+1 :foreground ,themodor-dark-bg+1))))
   `(whitespace-hspace ((t (:background ,themodor-dark-bg+1 :foreground ,themodor-dark-bg+1))))
   `(whitespace-tab ((t (:background ,themodor-dark-red-1))))
   `(whitespace-newline ((t (:foreground ,themodor-dark-bg+1))))
   `(whitespace-trailing ((t (:background ,themodor-dark-red))))
   `(whitespace-line ((t (:background ,themodor-dark-bg :foreground ,themodor-dark-magenta))))
   `(whitespace-space-before-tab ((t (:background ,themodor-dark-orange :foreground ,themodor-dark-orange))))
   `(whitespace-indentation ((t (:background ,themodor-dark-yellow :foreground ,themodor-dark-red))))
   `(whitespace-empty ((t (:background ,themodor-dark-yellow))))
   `(whitespace-space-after-tab ((t (:background ,themodor-dark-yellow :foreground ,themodor-dark-red))))))

;;; Theme Variables
(themodor-dark-with-color-variables
  (custom-theme-set-variables
   'themodor-dark
;;;;; ansi-color
   `(ansi-color-names-vector [,themodor-dark-bg ,themodor-dark-red ,themodor-dark-green ,themodor-dark-yellow
                                          ,themodor-dark-blue ,themodor-dark-magenta ,themodor-dark-cyan ,themodor-dark-fg])
;;;;; company-quickhelp
   `(company-quickhelp-color-background ,themodor-dark-bg+1)
   `(company-quickhelp-color-foreground ,themodor-dark-fg)
;;;;; fill-column-indicator
   `(fci-rule-color ,themodor-dark-bg-05)
;;;;; nrepl-client
   `(nrepl-message-colors
     '(,themodor-dark-red ,themodor-dark-orange ,themodor-dark-yellow ,themodor-dark-green ,themodor-dark-green+4
       ,themodor-dark-cyan ,themodor-dark-blue+1 ,themodor-dark-magenta))
;;;;; pdf-tools
   `(pdf-view-midnight-colors '(,themodor-dark-fg . ,themodor-dark-bg-05))
;;;;; vc-annotate
   `(vc-annotate-color-map
     '(( 20. . ,themodor-dark-red-1)
       ( 40. . ,themodor-dark-red)
       ( 60. . ,themodor-dark-orange)
       ( 80. . ,themodor-dark-yellow-2)
       (100. . ,themodor-dark-yellow-1)
       (120. . ,themodor-dark-yellow)
       (140. . ,themodor-dark-green-2)
       (160. . ,themodor-dark-green)
       (180. . ,themodor-dark-green+1)
       (200. . ,themodor-dark-green+2)
       (220. . ,themodor-dark-green+3)
       (240. . ,themodor-dark-green+4)
       (260. . ,themodor-dark-cyan)
       (280. . ,themodor-dark-blue-2)
       (300. . ,themodor-dark-blue-1)
       (320. . ,themodor-dark-blue)
       (340. . ,themodor-dark-blue+1)
       (360. . ,themodor-dark-magenta)))
   `(vc-annotate-very-old-color ,themodor-dark-magenta)
   `(vc-annotate-background ,themodor-dark-bg-1)
   ))

;;; Footer

;;;###autoload
(when load-file-name
  (let ((dir (file-name-directory load-file-name)))
    (unless (equal dir (expand-file-name "themes/" data-directory))
      (add-to-list 'custom-theme-load-path dir))))

(provide 'themodor-dark)

;;; themodor-dark.el ends here
