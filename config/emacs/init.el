;; NOTE: Nice config over here: https://github.com/angrybacon/dotemacs
;; TODO: Create a unified setting for character limit
;; TODO: use prescient for ivy and company
;; TODO: check on ispell
;; TODO: Configure eshell (and get rid of vterm if it works well enough)
;; TODO: Make better lsp-ui
;; TODO: dap-mode
;; TODO: improve org-mode keybindings
;; TODO: use as an latex editor
;; TODO: whitespaces
;; TODO: find way to write multiline comments (NOTE: works in go-mode already)
;; TODO: spelling + grammer
;; TODO: setup dired
;; TODO: Fix weird escape characters when building docker images
;; TODO: Fix bugs with emacs deamon and improve config
;; TODO: tsx, jsx, vue-files
;; TODO: refactor
;; TODO: use org-file for configuration
;; FIXME: missing output on docker build
;; FIXME: neofetch is displayed wrong
;; FIXME: a lot of ansi escape sequences when failing to install lsp-servers
;; TODO: configure Eshell


;; NOTE: do not use 'most-positive-fixnum', this will cause Emacs to freeze on the first start
(setq gc-cons-threshold (* 100 1024 1024)
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %.2f seconds 🚀" (string-to-number (emacs-init-time)))
            (setq gc-cons-threshold (* 10 1024 1024)
                  read-process-output-max (* 1024 1024)
                  gc-cons-percentage 0.1)
            ;; Run a garbage collection when everything else is done
            (garbage-collect)))


;; Install packages in ~/.local/share not ~/.config
(setq user-emacs-directory (expand-file-name "~/.local/share/emacs/")
      user-full-name "Bastian Hussi"
      user-mail-address "bastian@ipfso.de")

;; Set the working directory to home regardless of where Emacs was started from
(cd "~/")


;; Avoid outdated byte compiled
(setq load-prefer-newer t)

(with-eval-after-load 'gnutls
  (eval-when-compile
    (require 'gnutls))
  (setq gnutls-verify-error t
        gnutls-min-prime-bits 3072))


(require 'package)
(setq package-archives '(("elpa" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))


(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)

;; Prevent the startup message about GNU Emacs and the GNU system
;; SEE: https://lists.gnu.org/archive/html/bug-gnu-emacs/2012-12/msg00954.html
(put 'inhibit-startup-echo-area-message 'saved-value t)
(setq inhibit-startup-echo-area-message (user-login-name))

(setq inhibit-startup-message t
      initial-scratch-message ""
      initial-major-mode 'org-mode
      frame-title-format "GNU Emacs")

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'iso-latin-1)

;; Whether frames should be resized implicitly. Prevent Emacs from changing it's size during startup.
(setq frame-inhibit-implied-resize t)
(setq default-frame-alist '((undecorated . t) ;; No gtk Title bar
                            (alpha . (95 . 95)) ;; Transparency
                            (width . 120) ;; Width in columns
                            (height . 36) ;; Height in columns
                            (left-fringe . 20)
                            (right-fringe . 0)))


(setq use-default-font-for-symbols nil
      inhibit-compacting-font-caches t)


(defun set-font-faces ()
  "Setting up fonts + emoji support."

  ;; Default font size
  (defvar font-size 160)

  (set-face-attribute 'default nil :font "Fira Code Retina" :height font-size)
  (set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height font-size)
  (set-face-attribute 'variable-pitch nil :font "Cantarell" :height font-size :weight 'regular)

  ;; Colorful emojis
  (set-fontset-font t 'symbol "Noto Color Emoji")
  (set-fontset-font t 'symbol "Symbola" nil 'append))


(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'set-font-faces)
    (set-font-faces))

;; Toggle interface elements
(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)
(column-number-mode 1)

;; Setup the colorscheme and add a nice looking modeline
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-dracula t))

(use-package doom-modeline
  :ensure t
  :commands doom-modeline-mode
  :custom
  (doom-modeline-buffer-file-name-style 'truncate-except-project)
  ;; (doom-modeline-icon t)
  :custom-face
  (mode-line ((t (:height 0.90))))
  (mode-line-inactive ((t (:height 0.80))))
  :hook
  (after-init . doom-modeline-mode))

;; FIXME: refactor this. Are there other settings to make in the scope of this function?
(defun new-frame-setup (&optional frame)
  "lorem."
  (if (display-graphic-p frame)
      (setq doom-modeline-icon t)
    (setq doom-modeline-icon nil)))

;; Run for already-existing frames
(mapc 'new-frame-setup (frame-list))
;; Run when a new frame is created
(add-hook 'after-make-frame-functions 'new-frame-setup)
;; server-after-make-frame-hook

;; (unless (daemonp) (add-hook 'after-init-hook 'doom-modeline-setup))


;; FIXME: When is setq-default really necessary?
(setq-default cursor-in-non-selected-windows nil ; Hide the cursor in inactive windows
              show-help-function nil ; Disable help text everywher
              fill-column 99
              tab-width 4
              indent-tabs-mode nil
              tab-always-indent nil)


;; NOTE: fixed bug. Solution do not use global-display-line-numbers-mode at all.
;; Do not even set it -1, just ignore it.
(use-package display-line-numbers
  :commands display-line-numbers-mode
  :hook
  ((text-mode prog-mode) . display-line-numbers-mode)
  :config
  (setq display-line-numbers-type 'relative))


;; Highlight the current line.
(use-package hl-line
  :commands hl-line-mode
  :hook
  ((text-mode prog-mode) . hl-line-mode))

;; Display keywords like TODO, NOTE, FIXME in different colors.
(use-package hl-todo
  :ensure t
  :commands hl-todo-mode
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        '(("TODO"       warning bold)
          ("FIXME"      error bold)
          ("HACK"       font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DEPRECATED" font-lock-doc-face bold)))
  :hook (prog-mode . hl-todo-mode))

;; Mouse settings
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse t)

;; Turn of the bell
(setq ring-bell-function 'ignore)

;; Typing out yes / no is waaaaay to tedious
(fset 'yes-or-no-p 'y-or-n-p)


(setq large-file-warning-threshold nil
      vc-follow-symlinks t
      find-file-visit-truename t)

(global-auto-revert-mode 1)


;; Do not litter the emacs-user-directory with backups and autosaved files
(setq custom-file (expand-file-name "custom.el" user-emacs-directory)
      ;; NOTE: use /tmp instead?
      temporary-file-directory (expand-file-name "~/.cache/emacs/"))

;; Directory will not be created automatically
(make-directory temporary-file-directory t)
(setq backup-by-copying t
      delete-old-versions t
      delete-by-moving-to-trash t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      ;; Prevent issues with build-watchers
      create-lockfiles nil
      auto-save-list-file-prefix nil ;; Use ~/.cache directory instead?
      backup-directory-alist
      `(("." . ,temporary-file-directory))
      auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Insert text into new buffers based on their major mode
(auto-insert-mode -1)

;; Automatically insert closing pairs like ", ), ], }
(use-package elec-pair
  :commands electric-pair-mode
  :config
  (setq electric-pair-preserve-balance nil)
  :hook
  (prog-mode . electric-pair-mode))


;; Highlight matching parenthesis
(use-package paren
  :commands show-paren-mode
  :config
  (setq show-paren-delay 0.25
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t)
  :hook
  (prog-mode . show-paren-mode))


;; Highlight some non printable characters like tabs and trailing spaces
(use-package whitespace
  :commands whitespace-mode
  :config
  (setq whitespace-line-column fill-column
        whitespace-style '(face tabs trailing lines-tail))
  :hook
  ((prog-mode org-mode) . whitespace-mode))


;; Auto break lines when hitting the fill-column limit
(use-package simple
  :commands auto-fill-mode
  :hook ((prog-mode org-mode) . auto-fill-mode))


;; Use tabs within Emacs. The tabbar is only visible when two or more tabs are open
(use-package tab-bar
  :config
  (setq tab-bar-close-button-show nil
        tab-bar-new-button-show nil
        ;; New tabs will show the scratch-buffer
        tab-bar-new-tab-choice "*scratch*"
        ;; Always add new tabs to the rightmost position
        tab-bar-new-tab-to 'rightmost))

;; Emacs file manager
;; TODO: setup keymaps
(use-package dired
  :custom
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-recursive-copies 'always))


;; TODO: remove when Emacs v28 gets out.
(use-package undo-fu
  :ensure t
  :defer t)


;; Vim within Emacs.
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil
        evil-want-integration t
        evil-want-fine-undo nil
        evil-want-C-i-jump t
        ;; TODO: Use undo-redo when Emacs v28 gets released.
        evil-undo-system 'undo-fu
        evil-move-beyond-eol t
        ;; You can't escape vim
        evil-toggle-key "")
  (setq-default evil-shift-width tab-width)
  :config
  (evil-mode 1))

;; Useful vim keybindings for popular modes in Emacs.
(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

;; Tim Popes surround plugin for Emacs.
(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

;; Vim-Snipe plugin for Emacs.
(use-package evil-snipe
  :ensure t
  :after evil
  :config
  (setq evil-snipe-scope 'whole-visible
        evil-snipe-repeat-scope 'whole-visible
        evil-snipe-spillover-scope 'whole-buffer)
  ;; see: https://github.com/emacs-evil/evil-collection/tree/master/modes/magit#known-conflicts
  (push 'magit-mode evil-snipe-disabled-modes)
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1))

;; Tim Popes commentary plugin for Emacs.
;; TODO: use a hook to activate this mode only after prog-mode
(use-package evil-commentary
  :ensure t
  :after evil
  :config
  (evil-commentary-mode 1))

;; Convenient way to manage keybindings
(use-package general
  :ensure t
  :config
  (general-evil-setup t)
  (general-setq evil-search-module 'evil-search)
  (general-create-definer leader-key
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))

;; Displays key bindings following the currently entered incomplete command in a popup.
(use-package which-key
  :ensure t
  :config
  (setq which-key-idle-delay 0.75)
  (which-key-mode 1))

;; Ivy is a generic completion mechanism for Emacs.
(use-package ivy
  :ensure t
  :hook (after-init . ivy-mode)
  :config
  (setq ivy-wrap t
        ;; Add bookmarks and recentf to buffer list
        ivy-use-virtual-buffers t
        ivy-height 12
        ivy-auto-select-single-candidate t
        ;; Do not close the minibuffer with delete
        ivy-on-del-error-function nil
        ;; Always use fuzzy search except swiper
        ivy-re-builders-alist
        '((read-file-name-internal . ivy--regex-fuzzy)
          (t . ivy--regex-plus)))
  :bind
  (:map ivy-minibuffer-map
        ("RET" . ivy-done)
        ("TAB" . ivy-next-line)
        ("<backtab>" . ivy-previous-line)))


(use-package counsel
  :ensure t
  :after ivy
  :bind
  ;; Replace evil search with Swiper
  ([remap evil-ex-search-forward] . swiper)
  ([remap evil-ex-search-backward] . swiper-backward)
  :general
  (leader-key
    "f"  '(:ignore t :which-key "Find")
    "ff" 'counsel-fzf
    "fg" 'counsel-rg
    "fb" 'counsel-switch-buffer
    "fj" 'counsel-file-jump
    "fl" 'counsel-locate
    "fr" 'counsel-recentf)
  ;; Enabling counsel-mode remaps built-in Emacs functions that have counsel replacements
  :config
  (counsel-mode 1))


;; A Git Porcelain inside Emacs.
(use-package magit
  :ensure t
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :general
  (leader-key
    "g"   '(:ignore t :which-key "Git")
    "gs"  'magit-status
    "gd"  'magit-diff-unstaged
    "gc"  'magit-branch-or-checkout
    "gl"   '(:ignore t :which-key "Log")
    "glc" 'magit-log-current
    "glf" 'magit-log-buffer-file
    "gb"  'magit-branch
    "gP"  'magit-push-current
    "gp"  'magit-pull-branch
    "gf"  'magit-fetch
    "gF"  'magit-fetch-all
    "gr"  'magit-rebase))


;; A major mode for convenient plain text markup — and much more.
(use-package org
  :ensure t
  ;; :defer t
  :general
  ;; FIXME: doesn't work like this
  ;; (leader-key org-mode-map
  ;;   "o"   '(:ignore t :which-key "Org")
  ;;   "os"   '(:ignore t :which-key "Show")
  ;;   "osa" 'org-show-all))
  )


(use-package projectile
  :ensure t
  :general
  (leader-key "p"
    '(:prefix-map projectile-command-map :which-key "Project"))
  :config
  (setq projectile-switch-project-action #'projectile-dired
        projectile-sort-order 'recently-active)
  (projectile-mode 1))


(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook
  (prog-mode . yas-minor-mode)
  :bind
  (:map yas-minor-mode-map
        ("TAB" . nil))
  :config
  (yas-reload-all))

(use-package company
  :ensure t
  :commands company-mode
  :hook ((prog-mode org-mode) . company-mode)
  :bind
  (:map company-active-map
        ("RET" . company-complete-selection)
        ("TAB" . company-select-next)
        ("<backtab>" . company-select-previous))
  :custom
  (company-backends '(company-capf))
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-other-buffers nil)
  (company-global-modes '(not help-mode message-mode))
  (company-idle-delay 0)
  (company-minimum-prefix-length 1)
  (company-require-match nil)
  (company-selection-wrap-around t)
  (company-tooltip-align-annotations t)
  (company-tooltip-flip-when-above t)
  (company-tooltip-offset-display nil)
  (company-tooltip-width-grow-only t))

(use-package flyspell
  :commands (flyspell-mode flyspell-prog-mode)
  :config
  (setq flyspell-delay 0.25
        flyspell-issue-message-flag nil)
  :hook
  (text-mode . flyspell-mode)
  (prog-mode . flyspell-prog-mode))


(use-package ispell
  :after flyspell
  :config
  (setq ispell-program-name "hunspell"
        ispell-dictionary "en_US,de_DE")
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US,de_DE"))


(use-package flycheck
  :ensure t
  :commands flycheck-mode
  :bind (("C-j" . next-error) ("C-k" . previous-error))
  :hook ((prog-mode org-mode) . flycheck-mode))


(use-package lsp-mode
  :ensure t
  :commands lsp-deferred
  :hook
  ((rustic-mode
    go-mode
    python-mode
    js-mode
    typescript-mode
    web-mode
    css-mode
    sgml-mode
    yaml-mode
    dockerfile-mode)
   . lsp-deferred)
  (before-save . lsp-format-buffer)
  :general
  (leader-key "c"
    '(:keymap lsp-command-map :which-key "Code"))
  (general-define-key
   :states 'normal
   :keymaps 'lsp-mode-map
   "K" 'lsp-describe-thing-at-point
   "gi" 'lsp-goto-implementation
   "gr" 'lsp-find-references
   "gd" 'lsp-find-definition
   "gD" 'lsp-find-declaration)
  :custom
  (lsp-diagnostic-package :flycheck)
  (lsp-prefer-capf t)
  (read-process-output-max (* 1024 1024))
  (lsp-rust-server 'rust-analyzer))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :custom
  (lsp-ui-doc-max-width 80)
  (lsp-ui-doc-max-height 60)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-delay 0.25))

(use-package lsp-ivy
  :ensure t
  :after lsp-mode
  :commands lsp-ivy-workspace-symbol)


(use-package prettier
  :ensure t
  :commands prettier-mode
  :config
  ;; Speed up opening files
  (setq prettier-pre-warm "none")
  ;; using descripe-function for example or selecting themes
  :hook ((typescript-mode js-mode web-mode sgml-mode css-mode yaml-mode)
         . prettier-mode))


(use-package rustic
  :ensure t
  :mode ("\\.rs\\'" . rustic-mode))


(use-package go-mode
  :ensure t
  :mode "\\.go\\'")


(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))

(use-package pyvenv
  :ensure t
  :commands pyvenv-mode
  :hook (python-mode . pyvenv-mode))


(use-package js
  :mode ("\\.js\\'" . js-mode)
  :interpreter ("javascript" . js-mode))


(use-package typescript-mode
  :ensure t
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2))


(use-package web-mode
  :ensure t
  :mode ("\\.vue\\'" "\\.jsx\\'" "\\.tsx\\'")
  :hook
  (web-mode . sgml-electric-tag-pair-mode)
  :custom
  (web-mode-code-indent-offset 2)
  (web-mode-enable-auto-opening nil)
  (web-mode-enable-auto-pairing nil)
  (web-mode-enable-auto-quoting nil)
  (web-mode-markup-indent-offset 2)
  (web-mode-enable-auto-indentation nil))


(use-package sgml-mode
  :mode "\\.html?\\'"
  :custom
  (sgml-basic-offset 2))


(use-package css-mode
  :mode "\\.css\\'"
  :custom
  (css-indent-offset 2))


(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml\\'")

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")


(add-to-list 'auto-mode-alist '("\\.service\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.timer\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.target\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.mount\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.automount\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.slice\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.socket\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.path\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.netdev\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.network\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.link\\'" . conf-unix-mode))


;; Use the escape-key to quit prompts
(global-set-key (kbd "ESC") 'keyboard-escape-quit)
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))


(general-define-key
 :states '(normal insert)
 "C-+" 'text-scale-increase
 "C--" 'text-scale-decrease
 "C-0" 'text-scale-adjust)


;; TODO: use Hydra for some shortcuts
(leader-key
  "SPC" '(dired :which-key "Directory")
  "h" '(:keymap help-map :which-key "Help")
  "b" '(:keymap bookmark-map :which-key "Bookmarks")
  "n"   '(:ignore t :which-key "New")
  "nt" '(tab-new :which-key "Tab")
  "ne" '(eshell :which-key "Eshell")
  "nT" '(term :which-key "Term")
  "j"   '(:ignore t :which-key "Jump")
  "jt" '(tab-next :which-key "Next Tab")
  "jT" '(tab-previous :which-key "Previous Tab")
  "q"   '(:ignore t :which-key "Quit")
  "qq" '(save-buffers-kill-terminal :which-key "Emacs")
  "qb" '(kill-this-buffer :which-key "Buffer")
  "qt" '(tab-close :which-key "Tab")
  "qw" '(delete-window :which-key "Window"))


(defalias 'eb 'eval-buffer)
(defalias 'kb 'kill-buffer)
(defalias 'dr 'desktop-remove)
(defalias 'cp 'check-parens)
(defalias 'lt 'load-theme)
(defalias 'plp 'package-list-packages)


(defun move-frame-to-center ()
  "Center the Emacs frame on the desktop"

  (set-frame-position (selected-frame)
  (/ (- (x-display-pixel-width) (frame-pixel-width)) 2)
  (/ (- (x-display-pixel-height) (frame-pixel-height)) 2)))

;; Center the emacs frame on the desktop
;; FIXME: extremely hacky approach: Is the hook that can be used for this
(run-with-timer 1.1 nil 'move-frame-to-center)
