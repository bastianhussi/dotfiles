;; SEE: https://github.com/sonph/onehalf

(deftheme onehalf "Onehalf theme")

(defgroup onehalf nil
  "Onehalf options"
  :group 'faces)

(defcustom onehalf-height-title-1 1.3
  "Lorem"
  :type 'number
  :group 'onehalf)

(defcustom onehalf-height-title-2 1.2
  "Lorem"
  :type 'number
  :group 'onehalf)

(defcustom onehalf-height-title-3 1.1
  "Lorem"
  :type 'number
  :group 'onehalf)

(defface onehalf-black
  '((t :foreground "black"
       :background "aquamarine"
       :weight bold
       :underline t
       ))
  "Lorem"
  :group 'onehalf)

;;;; Color Constants
(let ((class '((class color) (min-colors #xFFFFFF)))
  ;; dark
  (black      "#282C34")
  (red        "#E06C75")
  (green      "#98C379")
  (yellow     "#E5C07B")
  (blue       "#61AFEF")
  (purple     "#C678DD")
  (cyan       "#56B6C2")
  (white      "#DCDFE4")
  (comment    "#5C6370")
  (grey       "#919BAA")
  (selection  "#474E5D")
  (fg         "#DCDFE4")
  (bg         "#282C34"))
  (custom-theme-set-faces
    'onehalf
    ; base
    `(default ((,class (:foreground ,fg :background ,bg))))
    `(fringe ((,class (:foreground ,fg :background ,bg))))
    `(cursor ((,class (:foreground ,bg :background ,fg))))
    `(bold ((,class (:weight bold))))
    `(italic ((,class (:slant italic))))
    `(bold-italic ((,class (:weigh bold :slant italic))))
    `(link ((,class (:foreground ,blue :underline t))))
    `(link-visited ((,class (:foreground ,purple :underline t))))
    `(region ((,class (:background ,grey)))) ;; selection
    `(error ((,class (:foreground ,red))))
    `(warning ((,class (:foreground ,yellow))))
    `(success ((,class (:foreground ,green))))
    `(highlight ((,class (:background ,cyan))))
    `(lazy-highlight ((,class (:background ,cyan))))
    ; hl-line
    `(hl-line ((,class (:background ,selection))))
    ; minibuffer
    `(minibuffer-prompt ((,class (:foreground ,comment))))
    ; ivy
    `(ivy-current-match ((,class (:inherit hl-line))))
    `(ivy-minibuffer-match-face-1 ((,class (:background ,purple))))
    `(ivy-minibuffer-match-face-2 ((,class (:background ,green))))
    `(ivy-minibuffer-match-face-3 ((,class (:background ,yellow))))
    `(ivy-minibuffer-match-face-4 ((,class (:background ,cyan))))
    `(ivy-minibuffer-match-highlight ((,class (:inherit lazy-highlight))))
    `(ivy-highlight-face ((,class (:background ,selection))))
    `(ivy-confirm-face ((,class (:foreground ,blue))))
    `(ivy-match-required-face ((,class (:inherit warning))))
    `(ivy-virtual ((,class (:background ,selection))))
    `(ivy-modified-buffer ((,class (:foreground ,comment))))
    `(swiper-match-face-1 ((,class (:inherit ivy-minibuffer-match-face-1))))
    `(swiper-match-face-2 ((,class (:inherit ivy-minibuffer-match-face-2))))
    `(swiper-match-face-3 ((,class (:inherit ivy-minibuffer-match-face-3))))
    `(swiper-match-face-4 ((,class (:inherit ivy-minibuffer-match-face-4))))
    ; font-lock
    `(font-lock-builtin-face ((,class (:foreground ,purple))))
    `(font-lock-comment-face ((,class (:foreground ,comment :slant italic))))
    `(font-lock-comment-delimiter-face ((,class (:inherit font-lock-comment-face))))
    `(font-lock-doc-face ((,class (:inherit font-lock-comment-face))))
    `(font-lock-keyword-face ((,class (:foreground ,blue))))
    `(font-lock-variable-face ((,class (:foreground ,red))))
    `(font-lock-type-face ((,class (:foreground ,yellow))))
    `(font-lock-constant-face ((,class (:foreground ,cyan :weight bold))))
    `(font-lock-function-face ((,class (:foreground ,blue))))
    `(font-lock-function-name-face ((,class (:foreground ,purple))))
    `(font-lock-string-face ((,class (:foreground ,green :slant italic))))
    `(font-lock-warning-face ((,class (:foreground ,red))))
    `(font-lock-preprocessor-face ((,class (:foreground ,blue))))
    `(font-lock-negation-char-face ((,class (:foreground ,purple))))
    ; line-number
    `(line-number ((,class (:foreground ,fg :background ,bg))))
    `(line-number-current-line ((,class (:foreground ,yellow :background ,selection))))
    ; tab-bar
    `(tab-bar ((,class (:foreground ,fg :background ,bg))))
    `(tab-bar-tab ((,class (:background ,selection))))
    `(tab-bar-tab-group-current ((,class (:background ,selection))))
    `(tab-bar-tab-group-inactive ((,class (:background ,bg))))
    `(tab-bar-tab-inactive ((,class (:background ,bg))))
    `(tab-bar-tab-ungrouped ((,class (:background ,bg))))
    ; tab-bar
    `(tab-bar ((,class (:foreground ,fg :background ,bg))))
    `(tab-bar-tab ((,class (:background ,selection))))
    `(tab-bar-tab-group-current ((,class (:background ,selection))))
    `(tab-bar-tab-group-inactive ((,class (:background ,bg))))
    `(tab-bar-tab-inactive ((,class (:background ,bg))))
    `(tab-bar-tab-ungrouped ((,class (:background ,bg))))
    ; tab-bar
    `(tab-bar ((,class (:foreground ,fg :background ,bg))))
    `(tab-bar-tab ((,class (:background ,selection))))
    `(tab-bar-tab-group-current ((,class (:background ,selection))))
    `(tab-bar-tab-group-inactive ((,class (:background ,bg))))
    `(tab-bar-tab-inactive ((,class (:background ,bg))))
    `(tab-bar-tab-ungrouped ((,class (:background ,bg))))
    ; show-parens
    `(show-paren-match ((,class (:background ,purple))))
    `(show-paren-mismatch ((,class (:background ,red))))
    ; whitespace-mode
    `(whitespace-empty ((,class (:inherit default))))
    `(whitespace-space ((,class (:inherit default :foreground ,grey))))
    `(whitespace-newline ((,class (:inherit default :foreground ,grey))))
    `(whitespace-tab ((,class (:background ,selection :foreground ,grey))))
    `(whitespace-indentation ((,class (:inherit default))))
    `(whitespace-trailing ((,class (:background ,red))))
    `(whitespace-line ((,class (:foreground ,red :weight bold))))
    ; mu4e
    ;; TODO: missing faces
    `(mu4e-header-key-face ((,class (:foreground ,green :weight bold))))
    `(mu4e-highlight-face ((,class (:background ,purple))))
    `(mu4e-header-highlight-face ((,class (:background ,green))))
    `(mu4e-unread-face ((,class (:foreground ,blue :weight bold))))
    `(mu4e-trashed-face ((,class (:foreground ,comment :strike-through t))))
    ; mode-line
    `(mode-line ((,class (:background ,selection :foreground ,fg))))
    `(mode-line-inactive ((,class (:background ,bg :foreground ,comment))))
    `(mode-line-emphasis ((,class (:inherit mode-line))))
    `(mode-line-highlight ((,class (:inherit mode-line))))
    ; company
    `(company-tooltip ((,class (:background ,bg :foreground ,comment))))
    `(company-tooltip-common ((,class (:background ,bg :foreground ,fg))))
    `(company-tooltip-search ((,class (:background ,bg :foreground ,grey))))
    `(company-tooltip-search-selection ((,class (:background ,bg :foreground ,comment))))
    ; company
    `(company-tooltip-selection ((,class (:background ,selection))))
    `(company-tooltip-annotation ((,class (:foreground ,red))))
    `(company-scrollbar-bg ((,class (:background ,bg))))
    `(company-scrollbar-fg ((,class (:background ,fg))))
    `(company-tooltip-common ((,class (:background ,bg :foreground ,fg))))
    `(company-tooltip-common-selection ((,class (:background ,bg :foreground ,fg))))
    ; outline
    `(outline-1 ((,class (:inherit bold :height ,onehalf-height-title-1))))
    `(outline-2 ((,class (:inherit bold :height ,onehalf-height-title-2))))
    `(outline-3 ((,class (:inherit bold :height ,onehalf-height-title-3))))
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
    `(org-block-begin-line ((,class (:background ,grey))))
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
    `(dired-ignored ((,class (:foreground ,comment))))
    `(dired-flagged ((,class (:foreground ,purple))))
    `(dired-header ((,class (:foreground ,red))))
    `(dired-mark ((,class (:foreground ,yellow))))
    `(dired-marked ((,class (:foreground ,yellow))))
    `(dired-perm-write ((,class (:foreground ,blue))))
    `(dired-symlink ((,class (:foreground ,cyan))))
    `(dired-warning ((,class (:inherit warning))))))


(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
    (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'onehalf)
