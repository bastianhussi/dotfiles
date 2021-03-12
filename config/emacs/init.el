;; -*- lexical-binding: t; -*-
;; init.el --- Where it all begins...

(setq user-full-name "Bastian Hussi")


;; Setup GnuTLS for package downloads and sending mail with mu4e
(with-eval-after-load 'gnutls
  (eval-when-compile
    (require 'gnutls))
  (add-to-list 'gnutls-trustfiles "~/.config/ssl/certs/*.pem") ;; Path to self signed certificates.
  ;; Do not cause an error when the hostname doesn't match the certificate’s host name.
  (setq gnutls-verify-error :trustfiles
        gnutls-min-prime-bits 3072))


(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install and use use-package
(straight-use-package 'use-package)


;; Prevent the startup message about GNU Emacs and the GNU system
(fset 'display-startup-echo-area-message 'ignore)
;; Typing out yes / no is waaaaay to tedious
(fset 'yes-or-no-p 'y-or-n-p)


(setq inhibit-splash-screen nil ;; FIXME: gets overridden later
      inhibit-startup-screen t
      inhibit-startup-message t
      ;; inhibit-startup-echo-area-message t
      initial-scratch-message nil
      initial-major-mode 'org-mode)


(setq confirm-kill-emacs 'y-or-n-p)
(setq use-dialog-box nil ;; Do not use GTK-Dialogs (e.g. when for confirmation to kill Emacs)
      use-file-dialog nil
      pop-up-windows nil)


(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment   'utf-8)

;; ;; Underline line at descent position, not baseline position
;; (setq x-underline-at-descent-line t)

;; ;; No ugly button for checkboxes
;; (setq widget-image-enable nil)


(defun my/set-font-faces ()
  "Setting up fonts + emoji support."
  (defvar my/font-size 160)
  (set-face-attribute 'default nil :font "Fira Code Retina" :height my/font-size)
  (set-face-attribute 'fixed-pitch nil :font "Cascadia Code" :height my/font-size)
  (set-face-attribute 'variable-pitch nil :font "Cantarell" :height my/font-size :weight 'regular)
  ;; By default, Emacs will try to use the default face’s font for
  ;; displaying symbol and punctuation characters, disregarding the
  ;; fontsets, if the default font can display the character. Prevent this behavior:
  (setq use-default-font-for-symbols nil)
  ;; Colorful emojis
  (set-fontset-font t 'symbol "Noto Color Emoji")
  (set-fontset-font t 'symbol "Symbola" nil 'append))

(add-hook 'window-setup-hook #'my/set-font-faces)


(use-package ligature
  :if (>= emacs-major-version 28)
  :straight '(:host github :repo "mickeynp/ligature.el")
  :commands global-ligature-mode
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  (ligature-set-ligatures
   'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))
  ;; Enables ligature checks globally in all buffers.
  :hook
  (window-setup . global-ligature-mode))


;; Don't show a backslash when wrapping a line
(set-display-table-slot standard-display-table 'wrap ?\ )

(setq-default cursor-in-non-selected-windows nil ; Hide the cursor in inactive windows
              fill-column 99
              indent-tabs-mode nil
              tab-width 4)


;; Mouse settings
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse t)

;; Turn of the bell
;; (setq ring-bell-function 'ignore)
(setq visible-bell t) ;; To use this the variable above has to be commented out


;; Prefer the encrypted authinfo-file
(setq auth-sources '((:source "~/.authinfo.gpg")
                     (:source "~/.authinfo")))

(setq epa-file-cache-passphrase-for-symmetric-encryption t
      epa-file-select-keys nil)

(setq large-file-warning-threshold nil
      find-file-visit-truename t
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      backup-directory-alist
      `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq auto-save-list-file-prefix
      temporary-file-directory)

(setq vc-follow-symlinks t
      delete-by-moving-to-trash t
      ;; Prevent issues with build-watchers
      create-lockfiles nil)

(global-auto-revert-mode 1)

;; Setup the colorscheme and add a nice looking modeline
(use-package doom-themes
  :if (display-graphic-p)
  :straight t
  :init
  ;; Allow loading themes without a warning. Like themes would be the only packages to be malicious ...
  (setq custom-safe-themes t)
  (load-theme 'doom-dracula)
  (doom-themes-org-config)
  (doom-themes-visual-bell-config))

(use-package all-the-icons
  :if (display-graphic-p)
  :straight t
  :commands all-the-icons-install-fonts
  :init
  (unless (find-font (font-spec :name "all-the-icons"))
    (all-the-icons-install-fonts t)))

(use-package doom-modeline
  :if (display-graphic-p)
  :straight t
  :commands doom-modeline-mode
  :custom
  (doom-modeline-height 40) ;; Default value of 25 is too small
  (doom-modeline-buffer-file-name-style 'truncate-except-project)
  (doom-modeline-project-detection 'project)
  :custom-face
  (mode-line ((t (:height 0.90))))
  (mode-line-inactive ((t (:height 0.80))))
  :hook
  (window-setup . doom-modeline-mode))


(use-package display-line-numbers
  :commands display-line-numbers-mode
  :custom
  (display-line-numbers-type 'relative)
  :hook
  ((text-mode prog-mode) . display-line-numbers-mode))

;; Highlight the current line.
(use-package hl-line
  :if (display-graphic-p)
  :commands hl-line-mode
  :hook
  ((text-mode prog-mode) . hl-line-mode))

;; Display keywords like TODO, NOTE, FIXME in different colors.
(use-package hl-todo
  :straight t
  :commands hl-todo-mode
  :custom
  (hl-todo-highlight-punctuation ":")
  (hl-todo-keyword-faces
   '(("TODO"       warning bold)
     ("FIXME"      error bold)
     ("HACK"       font-lock-constant-face bold)
     ("REVIEW"     font-lock-keyword-face bold)
     ("NOTE"       success bold)
     ("DEPRECATED" font-lock-doc-face bold)))
  :hook (prog-mode . hl-todo-mode))


;; Sadly this is the only way Emacs will respect variables set by zsh.
;; FIXME: Is there really no other option? Is this still necessary on this Fedora machine?
(use-package exec-path-from-shell
  :straight t
  :custom
  (exec-path-from-shell-arguments '("-l")) ;; Speed things up a bit
  :config
  (exec-path-from-shell-initialize))


;; Vim within Emacs.
(use-package evil
  :straight t
  :custom
  (evil-want-keybinding nil)
  (evil-want-integration t)
  (evil-want-C-i-jump t) ;; jump forward in the jump-list
  (evil-want-fine-undo t) ;; actions are undone in several steps
  (evil-undo-system 'undo-redo)
  (evil-move-beyond-eol t)
  (evil-vsplit-window-right t)
  (evil-split-window-below t)
  ;; You can't escape vim
  (evil-toggle-key "")
  :config
  (setq-default evil-shift-width tab-width)
  (evil-mode 1))


;; Useful vim keybindings for popular modes in Emacs.
(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init))

;; Tim Popes surround plugin for Emacs.
(use-package evil-surround
  :straight t
  :after evil
  :config
  (global-evil-surround-mode 1))

;; Vim-Snipe plugin for Emacs.
(use-package evil-snipe
  :straight t
  :after evil
  :custom
  (evil-snipe-scope 'whole-visible)
  (evil-snipe-repeat-scope 'whole-visible)
  (evil-snipe-spillover-scope 'whole-buffer)
  ;; see: https://github.com/emacs-evil/evil-collection/tree/master/modes/magit#known-conflicts
  :config
  (push 'magit-mode evil-snipe-disabled-modes)
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1))


(use-package hydra
  :straight t)


;; Convenient way to manage keybindings
(use-package general
  :straight t
  :config
  (general-evil-setup t)
  (general-setq evil-search-module 'evil-search)
  ;; NOTE: This overrides SPC in every keymap
  (general-create-definer my/leader-key
    :states 'normal
    :keymaps 'override
    :prefix "SPC"))


;; Displays key bindings following the currently entered incomplete command in a popup.
(use-package which-key
  :straight t
  :custom
  (which-key-idle-delay 0.50)
  :hook
  (window-setup . which-key-mode))


;; Use tabs within Emacs. The tabbar is only visible when two or more tabs are open
;; NOTE: no need to enable tab-bar-mode. If there is more than one tab the mode will be enable automatically.
(use-package tab-bar
  :commands tab-bar-mode
  :custom
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  (tab-bar-new-tab-choice "*scratch*") ;; New tabs will show the scratch-buffer
  (tab-bar-new-tab-to 'rightmost)  ;; Always add new tabs to the rightmost position
  :config
  (defhydra my/hydra-tab-bar (:color amaranth)
    "Tab Bar Operations"
    ("t" tab-new "Create a new tab" :column "Creation")
    ("d" dired-other-tab "Open Dired in another tab")
    ("f" find-file-other-tab "Find file in another tab")
    ("0" tab-close "Close current tab")
    ("m" tab-move "Move current tab" :column "Management")
    ("r" tab-rename "Rename Tab")
    ("RET" tab-bar-select-tab-by-name "Select tab by name" :column "Navigation")
    ("l" tab-next "Next Tab")
    ("h" tab-previous "Previous Tab")
    ("q" nil "Exit" :exit t))
  :general
  (my/leader-key "t" '(my/hydra-tab-bar/body :which-key "Tabs")))


;; Automatically insert closing pairs like ", ), ], }
(use-package elec-pair
  :commands electric-pair-mode
  :custom
  (electric-pair-preserve-balance nil)
  :hook
  (prog-mode . electric-pair-mode))

;; Highlight matching parenthesis
(use-package paren
  :commands show-paren-mode
  :custom
  (show-paren-delay 0.25)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  :hook
  (prog-mode . show-paren-mode))

;; Highlight some non printable characters like tabs and trailing spaces
(use-package whitespace
  :commands whitespace-mode
  :custom
  (whitespace-line-column fill-column)
  (whitespace-style '(face tabs trailing lines-tail))
  :hook
  ((text-mode prog-mode) . whitespace-mode))

;; Auto break lines when hitting the fill-column limit
(use-package simple
  :commands auto-fill-mode
  :hook ((text-mode prog-mode) . auto-fill-mode))


;; Ivy is a generic completion mechanism for Emacs.
(use-package ivy
  :straight t
  :commands ivy-mode
  :custom
  (ivy-wrap t)
  ;; Add bookmarks and recentf to buffer list
  (ivy-use-virtual-buffers t)
  (ivy-height 12)
  (ivy-auto-select-single-candidate t)
  (ivy-extra-directories nil) ;; Don't show . and .. when selecting files
  ;; Do not close the minibuffer with delete
  (ivy-on-del-error-function nil)
  (ivy-re-builders-alist
   '((read-file-name-internal . ivy--regex-fuzzy)
     (t . ivy--regex-plus)))
  :bind
  (:map ivy-minibuffer-map
        ("RET" . ivy-done)
        ("TAB" . ivy-next-line)
        ("C-V" . yank) ;; insert with Ctrl+Shift+v just like in a terminal
        ("<backtab>" . ivy-previous-line))
  :hook (after-init . ivy-mode))

(use-package counsel
  :straight t
  :commands counsel-mode
  :bind
  ;; Replace evil search with Swiper
  ([remap evil-ex-search-forward] . swiper)
  ([remap evil-ex-search-backward] . swiper-backward)
  :general
  (my/leader-key
    "f"  '(:ignore t :which-key "Find")
    "ff" 'counsel-fzf
    "fg" 'counsel-rg
    "fb" 'counsel-switch-buffer
    "fj" 'counsel-file-jump
    "fl" 'counsel-locate
    "fr" 'counsel-recentf)
  ;; Enabling counsel-mode remaps built-in Emacs functions that have counsel replacements
  :hook
  (ivy-mode . counsel-mode))

;; TODO: configure
(use-package prescient
  :straight t
  :after (ivy-prescient company-prescient)
  :custom
  (prescient-history-length 3)
  (prescient-sort-length-enable nil) ;; Don't sort by shortest-first.
  (prescient-filter-method '(literal regexp fuzzy))
  :config
  (prescient-persist-mode 1))

;; TODO: improve the loading of these packages (are hooks a solution?)
(use-package ivy-prescient
  :straight t
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil) ;; Let ivy handle the filtering
  (ivy-prescient-retain-classic-highlighting t) ;; Keep classic highlighting
  :config
  (ivy-prescient-mode 1))

;; TODO: move to company-mode
(use-package company-prescient
  :straight t
  :after company
  :config
  (company-prescient-mode 1))


(use-package recentf
  :defer 10
  :commands recentf-mode
  :custom
  (recentf-max-menu-items 25)
  (recentf-max-saved-items 25)
  :config
  (recentf-mode 1)
  ;; Save every 15 minutes
  (run-at-time nil (* 15 60) 'recentf-save-list))


(use-package project
  :general
  (my/leader-key
    "p" '(:keymap project-prefix-map :which-key "Project")))


;; Emacs file manager
;; TODO: setup keymaps
(use-package dired
  :commands (dired dired-jump)
  :custom
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-listing-switches "-Ahlv --group-directories-first") ;; Change the arguments passed to ls
  (dired-hide-details-hide-symlink-targets nil) ;; ...
  (dired-recursive-copies 'always)
  :config
  ;; use dired-find-alternate-file instead of dired-find-file to prevent dired to create so many buffers.
  (put 'dired-find-alternate-file 'disabled nil) ;; Need to be enabled manually
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory ;; go up a directory
    "l" 'dired-find-alternate-file)) ;; go into the selected directory


;; TODO: is there a way to refactor this?
(use-package latex
  :straight auctex
  :commands LaTeX-mode
  :mode ("\\.tex\\'" . LaTeX-mode)
  :config
  (flyspell-mode 1)
  (LaTeX-math-mode 1)
  (reftex-mode 1))

(use-package tex
  :after latex
  :custom
  (TeX-PDF-mode t)
  (TeX-auto-save t)
  (TeX-parse-self t)
  :config
  (setq-default TeX-master nil)
  :hook
  (TeX-after-compilation-finished-functions . TeX-revert-document-buffer))

(use-package reftex
  :straight auctex
  :after latex
  :custom
  (reftex-plug-into-AUCTeX t)
  (reftex-use-external-file-finders t)
  :config
  (turn-on-reftex))

;; https://joostkremers.github.io/ebib/ebib-manual.html
(use-package ebib
  :straight t
  :after latex
  :custom
  (ebib-bibtex-dialect 'Biber))


;; A major mode for convenient plain text markup — and much more.
;; TODO: https://www.youtube.com/watch?v=PNE-mgkZ6HM
(use-package org
  :straight t
  :commands (org-mode org-agenda)
  ;; TODO: implement these on my own https://github.com/edwtjo/evil-org-mode
  :custom
  (org-directory "~/Nextcloud/Notes/")
  (org-log-done 'time) ;; Add timestamp whenever task is finished
  (org-log-into-drawer t)
  (org-hide-leading-stars t) ;; Don't show all the stars in front of the headings
  (org-agenda-files (list (expand-file-name "todo.org" org-directory)))
  (org-agenda-start-with-log-mode t)
  (org-agenda-window-setup 'other-tab) ;; Open org-agenda in a new tab.
  :config
  ;; =M-x customize-group RET org-appearance RET=
  ;; =M-x customize-group RET org-faces RET=
  (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  :general
  ;; Only show these bindings when in org-mode
  (my/leader-key
    "o"   '(:ignore t :which-key "Org")
    "oa" 'org-agenda))


(defvar my/term-history-size 5000)

;; TODO: configure
(use-package eshell
  :commands eshell
  :custom
  (eshell-history-size my/term-history-size)
  (eshell-buffer-maximum-lines my/term-history-size)
  (eshell-hist-ignoredups t))

(use-package vterm
  :straight t
  :commands vterm
  :bind
  ([remap term] . vterm)
  :custom
  (vterm-kill-buffer-on-exit t)
  (vterm-max-scrollback my/term-history-size)
  :config
  (defun my/vterm-adjust-evil-cursor ()
    (setq-local evil-insert-state-cursor 'box
                evil-move-cursor-back nil))
  :hook
  (vterm-mode . my/vterm-adjust-evil-cursor))


;; NOTE: The entire mu4e configuration resides in the private configuration-file.


;; A Git Porcelain inside Emacs.
(use-package magit
  :straight t
  :general
  (my/leader-key
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


;; REVIEW: Global really necessary?
(use-package yasnippet
  :straight t
  :commands yas-global-mode
  :custom (yas-prompt-functions '(yas-completing-prompt))
  :hook (after-init . yas-global-mode))

(use-package yasnippet-snippets
  :straight t
  :after yasnippet)


;; REVIEW: is global-company-mode necessary?
(use-package company
  :straight t
  :commands global-company-mode
  :custom
  (company-backends '((company-capf :with company-yasnippet)
                      (company-dabbrev-code company-keywords company-files company-dabbrev)))
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-other-buffers nil)
  (company-idle-delay 0)
  (company-minimum-prefix-length 1)
  (company-require-match nil)
  (company-selection-wrap-around t)
  (company-tooltip-width-grow-only t)
  :config
  (setq tab-always-indent 'complete
        completion-cycle-threshold 5)
  (add-to-list 'completion-styles 'initials t)
  :bind
  (:map company-active-map
        ("RET" . company-complete-selection)
        ("TAB" . company-select-next)
        ("<backtab>" . company-select-previous))
  :hook (after-init . global-company-mode))


(use-package flyspell
  :commands (flyspell-mode flyspell-prog-mode)
  :custom
  (flyspell-delay 0.25)
  (flyspell-issue-message-flag nil)
  :hook
  (text-mode . flyspell-mode)
  (prog-mode . flyspell-prog-mode))

(use-package ispell
  :after flyspell
  :custom
  (ispell-program-name "hunspell")
  (ispell-dictionary "en_US,de_DE")
  :config
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US,de_DE"))

(use-package flymake
  :commands flymake-mode
  :config
  (defhydra my/hydra-flymake
    (:pre (flymake-show-diagnostics-buffer)
          :post (quit-windows-on (format "*Flymake diagnostics for %s*" (buffer-name)))
          :hint nil)
    "Errors"
    ("j" flymake-goto-next-error "Next")
    ("k" flymake-goto-prev-error "Previous")
    ("gg" (progn (goto-char (point-max)) (flymake-goto-next-error)) "First")
    ("G" (progn (goto-char (point-max)) (flymake-goto-prev-error)) "Last")
    ("q" nil))
  :general
  (my/leader-key "e" '(my/hydra-flymake/body :which-key "Errors"))
  :hook (prog-mode . flymake-mode))


;; TODO: setup eldoc (build-in)
;; NOTE: yasnippet-mode needs to be active before eglot
(use-package eglot
  :straight t
  :commands eglot-ensure
  :config
  (setq eglot-stay-out-of '("company"))
  :general
  (general-define-key
   :states 'normal
   :keymaps 'eglot-mode-map
   "<f2>" 'eglot-rename
   "gd" 'xref-find-definitions
   "gr" 'xref-find-references
   "gD" 'eglot-find-declaration
   "gi" 'eglot-find-implementation
   "gt" 'eglot-find-typeDefinition
   "gh" 'eldoc)
  (my/leader-key
   :states 'normal
   :keymaps 'eglot-mode-map
   "c"   '(:ignore t :which-key "Code")
   "ca"  '(:ignore t :which-key "Actions")
   "caa" 'eglot-code-actions
   "caq" 'eglot-code-action-quickfix
   "cae" 'eglot-code-action-extract
   "cai" 'eglot-code-action-inline
   "car" 'eglot-code-action-rewrite
   "cao" 'eglot-code-action-organize-imports
   "cf" 'eglot-format-buffer
   "cr" 'eglot-rename
   "cs"  '(:ignore t :which-key "Server")
   "css" 'eglot-shutdown
   "csr" 'eglot-reconnect)
  (general-vmap
    :keymaps 'eglot-mode-map
    "ff" 'eglot-format)
  :hook
  (eglot-mode . eldoc-mode)
  (before-save . eglot-format))

(use-package rust-mode
  :straight t
  :mode ("\\.rs\\'" . rust-mode)
  :config
  (setq-local indent-tabs-mode nil)
  :hook (rust-mode . eglot-ensure))

(use-package go-mode
  :straight t
  :mode "\\.go\\'"
  :hook (go-mode . eglot-ensure))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :hook (python-mode . eglot-ensure))

(use-package pyvenv
  :straight t
  :commands pyvenv-mode
  :hook (python-mode . pyvenv-mode))

(use-package js
  :mode ("\\.js\\'" . js-mode)
  :config
  (setq-default js-indent-level 2)
  :hook (js-mode . eglot-ensure))

(use-package typescript-mode
  :straight t
  :mode "\\.ts\\'"
  :config
  (setq-default typescript-indent-level 2)
  :hook (typescript-mode . eglot-ensure))


(use-package web-mode
  :straight t
  :mode ("\\.vue\\'" "\\.jsx\\'" "\\.tsx\\'")
  :custom
  (web-mode-code-indent-offset 2)
  (web-mode-enable-auto-opening nil)
  (web-mode-enable-auto-pairing nil)
  (web-mode-enable-auto-quoting nil)
  (web-mode-markup-indent-offset 2)
  (web-mode-enable-auto-indentation nil)
  :hook
  (web-mode . sgml-electric-tag-pair-mode)
  (web-mode . eglot-ensure))

(use-package sgml-mode
  :mode "\\.html?\\'"
  :custom
  (sgml-basic-offset 2)
  :hook
  (sgml-mode . eglot-ensure))

(use-package css-mode
  :mode "\\.css\\'"
  :custom
  (css-indent-offset 2)
  :hook
  (css-mode . eglot-ensure))

(use-package json-mode
  :straight t
  :mode "\\.json\\'"
  :hook
  (json-mode . eglot-ensure))

(use-package yaml-mode
  :straight t
  :mode "\\.ya?ml\\'"
  :hook
  (yaml-mode . eglot-ensure))

(use-package nix-mode
  :straight t
  :mode "\\.nix\\'")

(use-package markdown-mode
  :straight t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package dockerfile-mode
  :straight t
  :mode "Dockerfile\\'"
  :hook
  (dockerfile-mode . eglot-ensure))


;; The default indentation of 4 is way to much
(setq sh-basic-offset 2)
(setq org-src-preserve-indentation nil)
(setq org-edit-src-content-indentation 0)

;; Systemd-files
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
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;; text-scale-adjust looks at the last key typed to determine which action to take.
(global-set-key (kbd "C-+") 'text-scale-adjust)
(global-set-key (kbd "C--") 'text-scale-adjust)
(global-set-key (kbd "C-0") 'text-scale-adjust)


(defhydra my/hydra-straight (:hint nil)
  "
_c_heck all       |_f_etch all     |_m_erge all      |_n_ormalize all   |p_u_sh all
_C_heck package   |_F_etch package |_M_erge package  |_N_ormlize package|p_U_sh package
----------------^^+--------------^^+---------------^^+----------------^^+------------||_q_uit||
_r_ebuild all     |_p_ull all      |_v_ersions freeze|_w_atcher start   |_g_et recipe
_R_ebuild package |_P_ull package  |_V_ersions thaw  |_W_atcher quit    |prun_e_ build"
  ("c" straight-check-all)
  ("C" straight-check-package)
  ("r" straight-rebuild-all)
  ("R" straight-rebuild-package)
  ("f" straight-fetch-all)
  ("F" straight-fetch-package)
  ("p" straight-pull-all)
  ("P" straight-pull-package)
  ("m" straight-merge-all)
  ("M" straight-merge-package)
  ("n" straight-normalize-all)
  ("N" straight-normalize-package)
  ("u" straight-push-all)
  ("U" straight-push-package)
  ("v" straight-freeze-versions)
  ("V" straight-thaw-versions)
  ("w" straight-watcher-start)
  ("W" straight-watcher-quit)
  ("g" straight-get-recipe)
  ("e" straight-prune-build)
  ("q" nil))

;; TODO: hydra for window management
;; use other-window windmove-left ...
;; winner-mode for undo, redo
(defhydra my/hydra-window (:color amaranth)
  "Window management"
  ("h" windmove-left "Left" :column "Navigation")
  ("j" windmove-down "Down")
  ("k" windmove-up "Up")
  ("l" windmove-right "Right")
  ("v" split-window-vertically "Vertical" :column "Split")
  ("x" split-window-horizontally "Horizontal")
  ("H" windmove-swap-states-left "Left" :column "Swap")
  ("J" windmove-swap-states-down "Down")
  ("K" windmove-swap-states-up "Up")
  ("L" windmove-swap-states-right "Right")
  ("u" winner-undo "Undo" :column "Action")
  ("r" winner-redo "Redo")
  ("dw" delete-window "Window" :column "Delete")
  ("db" kill-this-buffer "Kill")
  ("o" delete-other-windows "Other" :exit t)
  ("q" nil "Exit" :exit t))

(add-hook 'window-setup-hook #'winner-mode)

;; Simulate Tim Popes vim-commentary for Evil
(evil-define-operator my/evil-comment-region (start end)
  "Comment or uncomment the given region"
  (comment-or-uncomment-region start end))

(general-nmap
  "gc" (general-key-dispatch 'my/evil-comment-region
         "c" 'comment-line))
(general-vmap
  "gc" 'my/evil-comment-region)


;; TODO: use Hydra for some shortcuts
(my/leader-key
  "SPC" '(dired :which-key "Directory")
  "w" '(my/hydra-window/body :which-key "Windows")
  "s" '(my/hydra-straight/body :which-key "Straight")
  "h" '(:keymap help-map :which-key "Help")
  "b" '(:keymap bookmark-map :which-key "Bookmarks")
  "n"   '(:ignore t :which-key "New")
  "ne" '(eshell :which-key "Eshell")
  "nt" '(term :which-key "Term")
  "j"   '(:ignore t :which-key "Jump")
  "jd" '(dired-jump :which-key "Directory")
  "q"   '(:ignore t :which-key "Quit")
  "qq" '(save-buffers-kill-terminal :which-key "Emacs")
  "qb" '(kill-this-buffer :which-key "Buffer")
  "qf" 'suspend-frame)


(defalias 'eb 'eval-buffer)
(defalias 'kb 'kill-buffer)
(defalias 'dr 'desktop-remove)
(defalias 'cp 'check-parens)
(defalias 'lt 'load-theme)


(load-file "private.el")

(when (file-exists-p custom-file)
  (load custom-file))

;; set the default-directory to $HOME.
(cd "~/")

(provide 'init)
;;; init.el ends here
