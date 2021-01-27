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
;; TODO: Create own keybindings with which-key description (e.g. leader+q)
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
            (message "Emacs loaded in %.2f seconds 🚀"
                     (float-time
                      (time-subtract after-init-time before-init-time)))
            (setq gc-cons-threshold (* 10 1024 1024)
                  read-process-output-max (* 1024 1024)
                  gc-cons-percentage 0.1)))


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


(setq inhibit-startup-message t
      initial-scratch-message ""
      initial-major-mode 'org-mode
      frame-title-format "GNU Emacs")

(setq user-full-name "Bastian Hussi"
      user-mail-address "bastian@ipfso.de")


(setq user-emacs-directory (expand-file-name "~/.local/share/emacs/")
      custom-file (expand-file-name ".custom.el" user-emacs-directory)
      temporary-file-directory (expand-file-name "~/.cache/emacs/"))


(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'iso-latin-1)


;; (add-to-list 'default-frame-alist '(font . "Fira Code Retina 16"))

;; No gtk-titlebar
;; (setq default-frame-alist '((undecorated . t)))

(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(set-frame-parameter (selected-frame) 'alpha '(95 . 95))
(add-to-list 'default-frame-alist `(alpha . (95 . 95)))

(setq use-default-font-for-symbols nil
      inhibit-compacting-font-caches t)


(set-face-attribute 'default nil :font "Fira Code Retina" :height 160)
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height 160)
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 160 :weight 'regular)
(set-fontset-font t 'symbol "Noto Color Emoji")
(set-fontset-font t 'symbol "Symbola" nil 'append)

;; (defun set-font-faces ()
;;   "Setting font faces."
;;   (set-face-attribute 'default nil :font "Fira Code Retina" :height 160)
;;   (set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height 160)
;;   (set-face-attribute 'variable-pitch nil :font "Cantarell" :height 160 :weight 'regular)
;;   (set-fontset-font t 'symbol "Noto Color Emoji")
;;   (set-fontset-font t 'symbol "Symbola" nil 'append))

;; (if (daemonp)
;;     (add-hook 'after-make-frame-functions
;;               (lambda (frame)
;;                 (setq doom-modeline-icon t)
;;                 (with-selected-frame frame
;;                   (set-font-faces))))
;;     (set-font-faces))


(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)
(column-number-mode 1)
(auto-insert-mode -1)


(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-dracula t))

;; NOTE: This costs us ~0.2 seconds startup time.
(use-package doom-modeline
  :ensure t
  :commands doom-modeline-mode
  :custom-face
  (mode-line ((t (:height 0.90))))
  (mode-line-active ((t (:height 0.90))))
  (mode-line-inactive ((t (:height 0.80))))
  :config
  (setq doom-modeline-buffer-file-name-style
        'truncate-except-project)
  :hook (after-init . doom-modeline-mode))



(setq-default fill-column 99
      tab-width 4
      ;; mode-line-format " %b (%m)"
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


(use-package hl-line
  :commands hl-line-mode
  :hook
  ((text-mode prog-mode) . hl-line-mode))


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


(setq mouse-wheel-scroll-amount '(2 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse t)

(setq ring-bell-function 'ignore)

(set-fringe-mode '(20 . 0))

(fset 'yes-or-no-p 'y-or-n-p)


(setq large-file-warning-threshold nil
      vc-follow-symlinks t
      find-file-visit-truename t)

(global-auto-revert-mode 1)


(make-directory temporary-file-directory t)
(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      ;; Prevent issues with build-watchers
      create-lockfiles nil
      backup-directory-alist
      `(("." . ,temporary-file-directory))
      auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


(use-package elec-pair
  :commands electric-pair-mode
  :config
  (setq electric-pair-preserve-balance nil)
  :hook
  (prog-mode . electric-pair-mode))


(use-package paren
  :commands show-paren-mode
  :config
  (setq show-paren-delay 0
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t)
  :hook
  (prog-mode . show-paren-mode))


(use-package whitespace
  :commands whitespace-mode
  :config
  (setq whitespace-line-column fill-column
        whitespace-style '(face tabs trailing lines-tail))
  :hook
  (prog-mode . whitespace-mode))


;; Auto break lines when hitting the fill-column limit
(use-package simple
  :commands auto-fill-mode
  :hook (prog-mode . auto-fill-mode))


(use-package tab-bar
  :config
  (setq tab-bar-close-button-show nil
        tab-bar-new-button-show nil
        ;; New tabs will show the scratch-buffer
        tab-bar-new-tab-choice "*scratch*"
        ;; Always add new tabs to the rightmost position
        tab-bar-new-tab-to 'rightmost))


;; TODO: remove when Emacs v28 gets out.
(use-package undo-fu
  :ensure t
  :defer t)


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

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

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


(use-package general
  :ensure t
  :config
  (general-evil-setup t)
  (general-setq evil-search-module 'evil-search)
  (general-create-definer leader-key
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))


(use-package which-key
  :ensure t
  ;; :defer 1
  :config
  (setq which-key-idle-delay 0.75)
  (which-key-mode 1))


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
        '((swiper . ivy--regex-plus)
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
    "fr" 'counsel-recentf))


(use-package magit
  ;; :defer 1
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


(use-package org
  :ensure t
  :defer t)


(use-package projectile
  :ensure t
  ;; :defer 1
  :general
  (leader-key "p"
    '(:prefix-map projectile-command-map :which-key "Project"))
  :config
  (setq projectile-switch-project-action #'projectile-dired
        projectile-sort-order 'recently-active)
  (projectile-mode 1))


(use-package yasnippet
  :ensure t
  :commands yas-minor-mode)


(use-package company
  :ensure t
  :commands company-mode
  :hook (prog-mode . company-mode)
  :bind
  (:map company-active-map
        ("RET" . company-complete-selection)
        ("TAB" . company-select-next)
        ("<backtab>" . company-select-previous))
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 1
        company-selection-wrap-around t))


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
  :hook (prog-mode . flycheck-mode))


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
   :keymaps 'lsp-mode-map
   :states 'normal
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
  :mode ("\\.vue\\'" "\\.jsx\\'" "\\.tsx\\'"))


(use-package sgml-mode
  :mode "\\.html?\\'")


(use-package css-mode
  :mode "\\.css\\'")


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


;; Simulate vim-commentary
(general-nmap
  "gc" 'comment-line)
(general-vmap
  "gc" 'comment-or-uncomment-region)


(defalias 'eb 'eval-buffer)
(defalias 'kb 'kill-buffer)
(defalias 'dr 'desktop-remove)
(defalias 'cp 'check-parens)
(defalias 'lt 'load-theme)
(defalias 'plp 'package-list-packages)
