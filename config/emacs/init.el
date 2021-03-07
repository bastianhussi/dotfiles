;;; init.el --- Emacs configuration                  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Bastian Hussi

;; Author: Bastian Hussi
;; Keywords: 

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

;;; Commentary:

;; 

;;; Code:

(add-hook 'after-init-hook
          (lambda ()
            ;; Always set the home directory to the current buffers default directory.
            ;; NOTE: invoking this earlier doesn't work
            (global-auto-revert-mode 1)
            ;; Sync mail in background
            (mu4e t)
            ;; Start server if its not already running
            (require 'server)
            (unless (server-running-p)
              (server-start))))


(setq user-full-name "Bastian Hussi")

;; The default directory should stay $XDG_CONFIG_HOME or ~/.config/emacs
;; Install packages in ~/.local/share not ~/.config
;; If the XDG_DATA_HOME variable is set use it. Otherwise fall back to ~/.local/share/
(setq default-directory user-emacs-directory
      user-emacs-directory (expand-file-name "emacs" (or (getenv "XDG_DATA_HOME") "~/.local/share"))
      ;; Save temporary file under /tmp/emacs<uid>
      temporary-file-directory (expand-file-name (format "emacs%d" (user-uid)) temporary-file-directory)
      custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))
;; This directory will not be created automatically
(make-directory temporary-file-directory t)

;; Do not report native compile warnings
(setq comp-async-report-warnings-errors nil)
;; Change the directory where the  natively compiled *.eln files will be cached
(when (boundp 'comp-eln-load-path)
  (setcar comp-eln-load-path
          (expand-file-name "cache/eln-cache/" user-emacs-directory)))


;; Setup GnuTLS for package downloads and sending mail with mu4e
(with-eval-after-load 'gnutls
  (eval-when-compile
    (require 'gnutls))
  (add-to-list 'gnutls-trustfiles "~/.config/ssl/certs/*.pem") ;; Path to self signed certificates.
  ;; Do not cause an error when the hostname doesn't match the certificate’s host name.
  (setq gnutls-verify-error :trustfiles
        gnutls-min-prime-bits 3072))


(setq straight-fix-flycheck t)

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


(setq inhibit-startup-message t
      initial-scratch-message ""
      initial-major-mode 'org-mode
      frame-title-format "GNU Emacs")

(setq confirm-kill-emacs 'y-or-n-p)
(setq use-dialog-box nil) ;; Do not use GTK-Dialogs (e.g. when for confirmation to kill Emacs)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'iso-latin-1)


;; FIXME: Why doesn't this work in the eary-init-file?
(set-scroll-bar-mode nil)

(defun set-font-faces ()
  "Setting up fonts + emoji support."
  (defvar my/font-size 160)
  (set-face-attribute 'default nil :font "Fira Code Retina" :height my/font-size)
  (set-face-attribute 'fixed-pitch nil :font "Fira Sans" :height my/font-size)
  (set-face-attribute 'variable-pitch nil :font "Cantarell" :height my/font-size :weight 'regular)
  ;; By default, Emacs will try to use the default face’s font for
  ;; displaying symbol and punctuation characters, disregarding the
  ;; fontsets, if the default font can display the character. Prevent this behavior:
  (setq use-default-font-for-symbols nil)
  ;; Colorful emojis
  (set-fontset-font t 'symbol "Noto Color Emoji")
  (set-fontset-font t 'symbol "Symbola" nil 'append))

;; Make sure the fonts are set: with daemon and without
(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'set-font-faces)
    (set-font-faces))

(use-package ligature
  :straight `(ligature :type git :host github :repo "mickeynp/ligature.el")
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
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
  (global-ligature-mode t))


(setq-default cursor-in-non-selected-windows nil ; Hide the cursor in inactive windows
              fill-column 99
              tab-width 4
              indent-tabs-mode nil)


;; Mouse settings
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse t)

;; Turn of the bell
;; (setq ring-bell-function 'ignore)
(setq visible-bell t) ;; To use this the variable above has to be commented out


(setq epa-file-cache-passphrase-for-symmetric-encryption t
      epa-file-select-keys nil)

;; Prefer the encrypted authinfo-file
(setq auth-sources '((:source "~/.authinfo.gpg")
                     (:source "~/.authinfo")))



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


;; Setup the colorscheme and add a nice looking modeline
(use-package doom-themes
  :if (display-graphic-p)
  :straight t
  :config
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
  (doom-modeline-buffer-file-name-style 'truncate-except-project)
  ;; (doom-modeline-project-detection 'projectile)
  :custom-face
  (mode-line ((t (:height 0.90))))
  (mode-line-inactive ((t (:height 0.80))))
  :config
  (defun doom-modeline-enable-icons ()
    (setq doom-modeline-icon (display-graphic-p)))
  :hook
  ;; FIXME: the right segment is displayed incorrectly when using the client
  (window-setup . doom-modeline-mode)
  (server-after-make-frame . doom-modeline-enable-icons))


(use-package display-line-numbers
  :commands display-line-numbers-mode
  :custom
  (display-line-numbers-type 'relative)
  :hook
  ((text-mode prog-mode) . display-line-numbers-mode))

;; Highlight the current line.
(use-package hl-line
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
  (evil-want-fine-undo nil)
  (evil-want-C-i-jump t)
  (evil-undo-system 'undo-redo)
  (evil-move-beyond-eol t)
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
  (general-create-definer leader-key
    :states 'normal
    :keymaps 'override
    :prefix "SPC"))
    ;; :global-prefix "C-SPC"))


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
  :custom
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  (tab-bar-new-tab-choice "*scratch*") ;; New tabs will show the scratch-buffer
  (tab-bar-new-tab-to 'rightmost)  ;; Always add new tabs to the rightmost position
  :config
  (defhydra hydra-tab-bar (:color amaranth)
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
  (leader-key "t" '(hydra-tab-bar/body :which-key "Tabs")))


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
  (leader-key
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
  :commands prescient-persist-mode
  :custom
  (prescient-history-length 3)
  (prescient-sort-length-enable nil) ;; Don't sort by shortest-first.
  (prescient-filter-method '(literal regexp fuzzy)))

(use-package ivy-prescient
  :straight t
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil) ;; Let ivy handle the filtering
  (ivy-prescient-retain-classic-highlighting t) ;; Keep classic highlighting
  :config
  (ivy-prescient-mode 1)
  (prescient-persist-mode 1)) ;; Enable saving the prescient results when loading this mode

(use-package company-prescient
  :straight t
  :after company
  :config
  (company-prescient-mode 1)
  (prescient-persist-mode 1))


(use-package recentf
  :commands recentf-mode
  :custom
  (recentf-max-menu-items 25)
  (recentf-max-saved-items 25)
  :config
  (run-at-time nil (* 15 60) 'recentf-save-list)) ;; Save every 15 minutes


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


(use-package pdf-tools
  :straight t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-loader-install))

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
  (TeX-view-program-selection '((output-pdf "PDF Tools")))
  (TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))
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
  (org-agenda-files (list (expand-file-name "todo.org" org-directory)))
  (org-agenda-start-with-log-mode t)
  (org-agenda-window-setup 'other-tab) ;; Open org-agenda in a new tab.
  :general
  ;; Only show these bindings when in org-mode
  (leader-key
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
  (defun vterm-adjust-evil-cursor ()
    (setq-local evil-insert-state-cursor 'box
                evil-move-cursor-back nil))
  :hook
  (vterm-mode . vterm-adjust-evil-cursor))


;; NOTE: The entire mu4e configuration resides in the private configuration-file.


;; A Git Porcelain inside Emacs.
(use-package magit
  :straight t
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


;; REVIEW: Global really necessary?
(use-package yasnippet
  :straight t
  :commands yas-global-mode
  :custom (yas-prompt-functions '(yas-completing-prompt))
  :hook (after-init . yas-global-mode))

(use-package yasnippet-snippets
  :straight t
  :after yasnippet)

(use-package company
  :straight t
  :commands company-mode
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
  :hook ((text-mode prog-mode) . company-mode))


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

(use-package flycheck
  :straight t
  :commands flycheck-mode
  :custom
  (flycheck-set-indication-mode 'left-margin)
  :config
  (defhydra hydra-flycheck
    (:pre (flycheck-list-errors)
          :post (quit-windows-on "*Flycheck errors*")
          :hint nil)
    "Errors"
    ("f" flycheck-error-list-set-filter "Filter")
    ("j" flycheck-next-error "Next")
    ("k" flycheck-previous-error "Previous")
    ("gg" flycheck-first-error "First")
    ("G" (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
    ("q" nil))
  :general
  (leader-key "e" '(hydra-flycheck/body :which-key "Errors"))
  :hook ((prog-mode org-mode) . flycheck-mode))


(use-package eglot
  :straight t
  :commands eglot-ensure
  :bind
  (:map eglot-mode-map
        ("<f2>" . eglot-rename)
        ("gd" . xref-find-definitions)))

(use-package rustic
  :straight t
  :mode ("\\.rs\\'" . rustic-mode)
  :hook (rustic-mode . eglot-ensure))

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

(use-package dockerfile-mode
  :straight t
  :mode "Dockerfile\\'"
  :hook
  (dockerfile-mode . eglot-ensure))

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


(defhydra hydra-straight-helper (:hint nil)
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
(winner-mode 1)
(defhydra hydra-window (:color amaranth)
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
(leader-key "w" '(hydra-window/body :which-key "Windows"))


;; Simulate Tim Popes vim-commentary for Evil
(evil-define-operator evil-comment-region (start end)
  "Comment or uncomment the given region"
  (comment-or-uncomment-region start end))

(general-nmap
  "gc" (general-key-dispatch 'evil-comment-region
         "c" 'comment-line))
(general-vmap
  "gc" 'evil-comment-region)


;; TODO: use Hydra for some shortcuts
(leader-key
  "SPC" '(dired :which-key "Directory")
  "h" '(:keymap help-map :which-key "Help")
  "b" '(:keymap bookmark-map :which-key "Bookmarks")
  "n"   '(:ignore t :which-key "New")
  "ne" '(eshell :which-key "Eshell")
  "nt" '(term :which-key "Term")
  "j"   '(:ignore t :which-key "Jump")
  "jd" '(dired-jump :which-key "Directory")
  "jt" '(hydra-jump-tabs/body :which-key "Tabs")
  "q"   '(:ignore t :which-key "Quit")
  "qq" '(save-buffers-kill-terminal :which-key "Emacs")
  "qb" '(kill-this-buffer :which-key "Buffer")
  "qt" '(tab-close :which-key "Tab")
  "qw" '(delete-window :which-key "Window")
  "qf" 'suspend-frame)


(defalias 'eb 'eval-buffer)
(defalias 'kb 'kill-buffer)
(defalias 'dr 'desktop-remove)
(defalias 'cp 'check-parens)
(defalias 'lt 'load-theme)


;; Don't split horizontally
;; (setq split-height-threshold nil)
;; (setq split-width-threshold 0)

;; TODO: https://github.com/daviwil/emacs-from-scratch/blob/master/show-notes/Emacs-Tips-DisplayBuffer-1.org
;; You can also customize those actions with an alist

;; TODO: org-agenda should use the whole buffer
;; TODO: help-buffers should not
;; TODO: mu4e compose should take the whole buffer
;; TODO: magit should take the whole buffer, commit the half
;; TODO: prefer vertical over horizontal splits
;; TODO: not more than two splits (horizontal / vertical) next to each other
;; (setq display-buffer-base-action
;;   '((display-buffer-reuse-window
;;      display-buffer-reuse-mode-window
;;      display-buffer-same-window
;;      display-buffer-in-previous-window)
;;     . ((inhibit-same-window nil)
;;        (reusable-frames . visible)
;;        (inhibit-switch-frame nil)
;;        (window-width nil) ;; don't change size of window
;;        (window-height nil)))) ;; don't change size of window


(load-file "private.el")

(cd "~/")

(provide 'init)
;;; init.el ends here
