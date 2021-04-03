;;; colorful-theme.el --- A colorful color scheme
;; -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Bastian Hussi

;; Author: Bastian Hussi
;; Keywords: faces

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:
(deftheme colorful "Colorful theme")

(defgroup colorful nil
  "Colorful options"
  :group 'faces)

(defcustom colorful-height-title-1 1.3
  "Height of level 1 heading"
  :type 'number
  :group 'colorful)

(defcustom colorful-height-title-2 1.2
  "Height of level 2 heading"
  :type 'number
  :group 'colorful)

(defcustom colorful-height-title-3 1.1
  "Height of level 3 heading"
  :type 'number
  :group 'colorful)

;; TODO: use these variables!
(defcustom colorful-italic-strings t
  "Italicize strings"
  :type 'boolean
  :group 'colorful)

(defcustom colorful-italic-comments t
  "Italicize comments"
  :type 'boolean
  :group 'colorful)

;;;; Color Constants
(let ((class '((class color) (min-colors #xFFFFFF)))
  ;; dark
  (black        "#362036")
  (black-faded  "#533153")
  (red          "#DD3E79")
  (red-faded    "#E56997")
  (orange       "#CB433E")
  (orange-faded "#D66A66")
  (green        "#3EDDA2")
  (green-faded  "#69E5B7")
  (yellow       "#FAB80E")
  (yellow-faded "#FBC740")
  (blue         "#0E50FA")
  (blue-faded   "#4074FB")
  (purple       "#A775BA")
  (purple-faded "#BD97CB")
  (cyan         "#3EC6CB")
  (cyan-faded   "#66D2D6")
  (white        "#FCFBFD")
  (white-faded  "#EAE6F0")
  (grey         "#CFC6DD")
  (fg           "#362036")
  (bg           "#FCFBFD"))
  (custom-theme-set-faces
    'colorful
    ; base
    `(default ((,class (:foreground ,fg :background ,bg))))
    `(fringe ((,class (:foreground ,fg :background ,bg))))
    `(cursor ((,class (:foreground ,bg :background ,fg))))
    `(bold ((,class (:weight bold))))
    `(italic ((,class (:slant italic))))
    `(bold-italic ((,class (:weigh bold :slant italic))))
    `(link ((,class (:foreground ,blue :underline t))))
    `(link-visited ((,class (:foreground ,purple :underline t))))
    `(region ((,class (:background ,purple-faded)))) ;; selection
    `(error ((,class (:foreground ,red))))
    `(warning ((,class (:foreground ,orange))))
    `(success ((,class (:foreground ,green))))
    `(highlight ((,class (:background ,cyan :foreground ,white))))
    `(lazy-highlight ((,class (:inherit highlight))))
    ; hl-line
    `(hl-line ((,class (:background ,white-faded))))
    ; minibuffer
    `(minibuffer-prompt ((,class (:foreground ,white-faded))))
    ; ivy
    `(ivy-current-match ((,class (:inherit hl-line))))
    `(ivy-minibuffer-match-face-1 ((,class (:background ,purple-faded))))
    `(ivy-minibuffer-match-face-2 ((,class (:background ,cyan-faded))))
    `(ivy-minibuffer-match-face-3 ((,class (:background ,yellow-faded))))
    `(ivy-minibuffer-match-face-4 ((,class (:background ,purple-faded))))
    `(ivy-minibuffer-match-highlight ((,class (:inherit lazy-highlight))))
    `(ivy-highlight-face ((,class (:background ,blue-faded))))
    `(ivy-confirm-face ((,class (:foreground ,blue))))
    `(ivy-match-required-face ((,class (:inherit warning))))
    `(ivy-virtual ((,class (:background ,white-faded))))
    `(ivy-modified-buffer ((,class (:foreground ,white-faded))))
    `(swiper-match-face-1 ((,class (:inherit ivy-minibuffer-match-face-1))))
    `(swiper-match-face-2 ((,class (:inherit ivy-minibuffer-match-face-2))))
    `(swiper-match-face-3 ((,class (:inherit ivy-minibuffer-match-face-3))))
    `(swiper-match-face-4 ((,class (:inherit ivy-minibuffer-match-face-4))))
    ; font-lock
    `(font-lock-builtin-face ((,class (:foreground ,purple))))
    `(font-lock-comment-face ((,class (:foreground ,grey))))
    `(font-lock-comment-delimiter-face ((,class (:inherit font-lock-comment-face))))
    `(font-lock-doc-face ((,class (:inherit font-lock-comment-face))))
    `(font-lock-keyword-face ((,class (:foreground ,orange))))
    `(font-lock-variable-face ((,class (:foreground ,red))))
    `(font-lock-type-face ((,class (:foreground ,yellow))))
    `(font-lock-constant-face ((,class (:foreground ,cyan))))
    `(font-lock-function-face ((,class (:foreground ,blue))))
    `(font-lock-function-name-face ((,class (:foreground ,purple))))
    `(font-lock-string-face ((,class (:foreground ,green))))
    `(font-lock-warning-face ((,class (:foreground ,red))))
    `(font-lock-preprocessor-face ((,class (:foreground ,blue))))
    `(font-lock-negation-char-face ((,class (:foreground ,purple))))
    ; line-number
    `(line-number ((,class (:foreground ,fg :background ,bg))))
    `(line-number-current-line ((,class (:foreground ,yellow :background ,white-faded))))
    ; tab-bar
    `(tab-bar ((,class (:inherit default))))
    `(tab-bar-tab ((,class (:background ,black-faded :foreground ,white-faded))))
    `(tab-bar-tab-inactive ((,class (:inherit tab-bar))))
    `(tab-bar-tab-group-current ((,class (:inherit tab-bar-tab))))
    `(tab-bar-tab-group-inactive ((,class (:inherit tab-bar-tab-inactive))))
    `(tab-bar-tab-ungrouped ((,class (:inherit tab-bar))))
    ; show-parens
    `(show-paren-match ((,class (:background ,purple))))
    `(show-paren-mismatch ((,class (:background ,red))))
    ; whitespace-mode
    `(whitespace-empty ((,class (:background nil))))
    `(whitespace-space ((,class (:background nil :foreground ,white-faded))))
    `(whitespace-newline ((,class (:background nil :foreground ,white-faded))))
    `(whitespace-tab ((,class (:background nil :foreground ,white-faded))))
    `(whitespace-indentation ((,class (:background nil))))
    `(whitespace-trailing ((,class (:background ,red))))
    `(whitespace-line ((,class (:foreground ,red :weight bold))))
    ; mu4e
    ;; TODO: missing faces
    `(mu4e-header-key-face ((,class (:foreground ,green :weight bold))))
    `(mu4e-highlight-face ((,class (:background ,purple))))
    `(mu4e-header-highlight-face ((,class (:background ,green))))
    `(mu4e-unread-face ((,class (:foreground ,blue :weight bold))))
    `(mu4e-trashed-face ((,class (:foreground ,white-faded :strike-through t))))
    ; mode-line
    `(mode-line ((,class (:background ,black-faded :foreground ,white-faded))))
    `(mode-line-inactive ((,class (:background ,bg :foreground ,black-faded))))
    `(mode-line-emphasis ((,class (:inherit mode-line))))
    `(mode-line-highlight ((,class (:inherit mode-line))))
    ; company
    `(company-tooltip ((,class (:background ,bg :foreground ,white-faded))))
    `(company-tooltip-common ((,class (:background ,bg :foreground ,fg))))
    `(company-tooltip-search ((,class (:background ,bg :foreground ,white-faded))))
    `(company-tooltip-search-selection ((,class (:background ,bg :foreground ,white-faded))))
    ; company
    `(company-tooltip-selection ((,class (:background ,black-faded))))
    `(company-tooltip-annotation ((,class (:foreground ,red))))
    `(company-scrollbar-bg ((,class (:background ,bg))))
    `(company-scrollbar-fg ((,class (:background ,fg))))
    `(company-tooltip-common ((,class (:background ,bg :foreground ,fg))))
    `(company-tooltip-common-selection ((,class (:background ,bg :foreground ,fg))))
    ; outline
    `(outline-1 ((,class (:inherit bold :height ,colorful-height-title-1))))
    `(outline-2 ((,class (:inherit bold :height ,colorful-height-title-2))))
    `(outline-3 ((,class (:inherit bold :height ,colorful-height-title-3))))
    `(outline-4 ((,class (:inherit bold))))
    `(outline-2 ((,class (:inherit bold))))
    `(outline-3 ((,class (:inherit bold))))
    `(outline-4 ((,class (:inherit bold))))
    `(outline-5 ((,class (:inherit bold))))
    `(outline-6 ((,class (:inherit bold))))
    `(outline-7 ((,class (:inherit bold))))
    `(outline-8 ((,class (:inherit bold))))
    ; org-mode
    `(org-table ((,class (:foreground ,blue))))
    `(org-block ((,class (:inherit default))))
    `(org-block-begin-line ((,class (:background ,white-faded))))
    `(org-block-end-line ((,class (:inherit org-block-begin-line))))
    `(org-level-1 ((,class (:inherit outline-1 :foreground ,purple))))
    `(org-level-2 ((,class (:inherit outline-2 :foreground ,green))))
    `(org-level-3 ((,class (:inherit outline-3 :foreground ,blue))))
    `(org-level-4 ((,class (:inherit outline-4))))
    `(org-level-5 ((,class (:inherit outline-5))))
    `(org-level-6 ((,class (:inherit outline-6))))
    `(org-level-7 ((,class (:inherit outline-7))))
    `(org-level-8 ((,class (:inherit outline-8))))
    `(org-todo ((,class (:foreground ,red))))
    `(org-done ((,class (:foreground ,green :strike-through t))))
    ;; elpher
    `(elpher-gemini-heading1 ((,class (:inherit outline-1))))
    `(elpher-gemini-heading2 ((,class (:inherit outline-2))))
    `(elpher-gemini-heading3 ((,class (:inherit outline-3))))
    ;; TODO: more elpher
    ; diff
    `(diff-added ((,class (:background ,green))))
    `(diff-changed ((,class (:background ,yellow))))
    `(diff-removed ((,class (:background ,red))))
    `(diff-header ((,class (:inherit bold))))
    `(diff-file-header ((,class (:inherit bold))))
    `(diff-hunk-header ((,class (:inherit bold))))
    `(diff-refine-added ((,class (:inherit diff-added))))
    `(diff-refine-changed ((,class (:inherit diff-changed))))
    `(diff-refine-removed ((,class (:inherit diff-removed))))
    ; hydra
    `(hydra-face-red ((,class (:foreground ,red :weight bold))))
    `(hydra-face-blue ((,class (:foreground ,blue :weight bold))))
    `(hydra-face-amaranth ((,class (:foreground ,yellow :weight bold))))
    `(hydra-face-pink ((,class (:foreground ,purple :weight bold))))
    `(hydra-face-teal ((,class (:foreground ,cyan :weight bold))))
    ; woman
    `(woman-bold ((,class (:inherit bold))))
    `(woman-italic ((,class (:inherit italic))))
    ; evil
    `(evil-ex-info ((,class (:foreground ,blue))))
    `(evil-ex-search ((,class (:inherit lazy-highlight))))
    `(evil-ex-substitute-matches ((,class (:foreground ,red :strike-through t))))
    `(evil-ex-substitute-replacement ((,class (:foreground ,green))))
    ; evil-snipe
    `(evil-snipe-first-match-face ((,class (:background ,green))))
    `(evil-snipe-matches-face ((,class (:inherit lazy-highlight))))
    ; flymake
    `(flymake-error ((,class (:inherit error))))
    `(flymake-note ((,class (:foreground ,yellow))))
    `(flymake-warning ((,class (:inherit warning))))
    ; flyspell
    `(flyspell-incorrect ((,class (:underline ,red))))
    `(flyspell-duplicate ((,class (:underline ,yellow))))
    ; term
    `(term ((,class (:inherit default))))
    `(term-bold ((,class (:inherit bold))))
    `(term-color-black ((,class (:foreground ,black))))
    `(term-color-red ((,class (:foreground ,red))))
    `(term-color-green ((,class (:foreground ,green))))
    `(term-color-yellow ((,class (:foreground ,yellow))))
    `(term-color-blue ((,class (:foreground ,blue))))
    `(term-color-magenta ((,class (:foreground ,purple))))
    `(term-color-cyan ((,class (:foreground ,cyan))))
    `(term-color-white ((,class (:foreground ,white))))
    ; which-key
    `(which-key-key-face ((,class (:foreground ,green))))
    `(which-key-group-description-face   ((,class (:foreground ,purple))))
    `(which-key-command-description-face ((,class (:foreground ,red))))
    `(which-key-local-map-description-face ((,class (:foreground ,blue))))
    ; dired
    `(dired-directory ((,class (:foreground ,green))))
    `(dired-ignored ((,class (:foreground ,white-faded))))
    `(dired-flagged ((,class (:foreground ,purple))))
    `(dired-header ((,class (:foreground ,red))))
    `(dired-mark ((,class (:foreground ,yellow))))
    `(dired-marked ((,class (:foreground ,yellow))))
    `(dired-perm-write ((,class (:foreground ,blue))))
    `(dired-symlink ((,class (:foreground ,cyan))))
    `(dired-warning ((,class (:inherit warning))))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
    (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'colorful)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:

;;; colorful-theme.el ends here
