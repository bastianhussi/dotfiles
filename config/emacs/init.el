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


;; TODO: Use org with: agenda and mu4e
;; TODO: Create a unified setting for character limit
;; TODO: find way to write multiline comments (NOTE: works in go-mode already)
;; TODO: spelling + grammer
;; TODO: Fix weird escape characters when building docker images
;; TODO: tsx, jsx, vue-files
;; TODO: refactor (reorder and group blocks, set a order for :hooks, :config, :custom, ...)
;; TODO: use org-file for configuration
;; FIXME: missing output on docker build
;; FIXME: a lot of ansi escape sequences when failing to install lsp-servers
;; TODO: remap C-y into C-shift-c in minibuffer to past passwords
;; REVIEW: reduce startup-time even more (better than 0.7-0.8, currently 1.3)?


;; NOTE: do not use 'most-positive-fixnum', this will cause Emacs to freeze on the first start
(setq gc-cons-threshold (* 100 1024 1024)
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            ;; Set the working directory to home regardless of where Emacs was started from
            (message "Emacs loaded in %.2f seconds 🚀" (string-to-number (emacs-init-time)))
            (cd "~/")
            (setq gc-cons-threshold (* 10 1024 1024)
                  read-process-output-max (* 1024 1024)
                  gc-cons-percentage 0.1)
            ;; Run a garbage collection when everything else is done
            (garbage-collect)
            ;; Sync mail in background
            (mu4e t)))

;; The default directory should stay $XDG_CONFIG_HOME or ~/.config/emacs
;; Install packages in ~/.local/share not ~/.config
;; If the XDG_DATA_HOME variable is set use it. Otherwise fall back to ~/.local/share/
(setq default-directory user-emacs-directory
      user-emacs-directory (expand-file-name "emacs" (or (getenv "XDG_DATA_HOME") "~/.local/share"))
      temporary-file-directory  (expand-file-name "emacs" (or (getenv "XDG_CACHE_HOME") "~/.cache"))
      ;; NOTE: use /tmp instead?
      custom-file (expand-file-name "custom.el" user-emacs-directory))

(load custom-file t)
;; This directory will not be created automatically
(make-directory temporary-file-directory t)

;; Other configuration files (private stuff, etc...)
(add-to-list 'load-path "lisp")

;; Setup GnuTLS for package downloads and sending mail with mu4e
(with-eval-after-load 'gnutls
  (eval-when-compile
    (require 'gnutls))
  (add-to-list 'gnutls-trustfiles "~/.ssl/certs/*.pem") ;; Path to self signed certificates.
  ;; Do not cause an error when the hostname doesn't match the certificate’s host name.
  (setq gnutls-verify-error :trustfiles
        gnutls-min-prime-bits 3072))



;; Avoid outdated byte compiled
(setq load-prefer-newer t)

(require 'package)
(setq package-archives '(("elpa" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/"))
      ;; REVIEW: is this order useful? Is this necessary at all?
      package-archive-priorities '(("elpa" . 5)
                                   ("melpa" . 10)
                                   ("org" . 15)))

;; Setup use-package
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
   (package-install 'use-package))
(require 'use-package)


;; Prevent the startup message about GNU Emacs and the GNU system
(fset 'display-startup-echo-area-message 'ignore)
;; Typing out yes / no is waaaaay to tedious
(fset 'yes-or-no-p 'y-or-n-p)

(setq inhibit-startup-message t
      initial-scratch-message ""
      initial-major-mode 'org-mode
      frame-title-format "GNU Emacs")

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'iso-latin-1)

;; Whether frames should be resized implicitly. Prevent Emacs from changing it's size during startup.
(setq frame-inhibit-implied-resize t
      default-frame-alist '((undecorated . t) ;; No gtk Title bar
                            (alpha . (95 . 95)) ;; Transparency
                            (fullscreen . maximized) ;; Width in columns
                            (left-fringe . 20)
                            (right-fringe . 0)))

(defun set-font-faces ()
  "Setting up fonts + emoji support."

  (setq use-default-font-for-symbols nil
        inhibit-compacting-font-caches t)

  ;; Default font size
  (defvar font-size 160)

  (set-face-attribute 'default nil :font "Fira Code Retina" :height font-size)
  (set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height font-size)
  (set-face-attribute 'variable-pitch nil :font "Cantarell" :height font-size :weight 'regular)

  ;; Colorful emojis
  (set-fontset-font t 'symbol "Noto Color Emoji")
  (set-fontset-font t 'symbol "Symbola" nil 'append))


;; Make sure the fonts are set: with daemon and without
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

;; Insert text into new buffers based on their major mode
(global-auto-revert-mode 1)

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


;; Setup the colorscheme and add a nice looking modeline
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-dracula t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :commands doom-modeline-mode
  :custom
  (doom-modeline-height 0)
  (doom-modeline-buffer-file-name-style 'truncate-except-project)
  (doom-modeline-project-detection 'projectile)
  :custom-face
  (mode-line ((t (:height 0.90))))
  (mode-line-inactive ((t (:height 0.80))))
  :hook
  ;; The daemon requires this. Only activate the icons in windowed version
  (server-after-make-frame . (lambda ()
                               (setq doom-modeline-icon (display-graphic-p))))
  (after-init . doom-modeline-mode))

;; NOTE: fixed bug. Solution do not use global-display-line-numbers-mode at all.
;; Do not even set it -1, just ignore it.
(use-package display-line-numbers
  :commands display-line-numbers-mode
  :hook
  ((text-mode prog-mode) . display-line-numbers-mode)
  :custom
  (display-line-numbers-type 'relative))

;; Highlight the current line.
(use-package hl-line
  :commands hl-line-mode
  :hook
  ((text-mode prog-mode) . hl-line-mode))

;; Display keywords like TODO, NOTE, FIXME in different colors.
(use-package hl-todo
  :ensure t
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

;; Use tabs within Emacs. The tabbar is only visible when two or more tabs are open
(use-package tab-bar
  :custom
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  (tab-bar-new-tab-choice "*scratch*") ;; New tabs will show the scratch-buffer
  (tab-bar-new-tab-to 'rightmost)) ;; Always add new tabs to the rightmost position


(use-package epa-file
  :custom
  (epa-file-cache-passphrase-for-symmetric-encryption t)
  (epa-file-select-keys nil))

;; Prefer the encrypted authinfo-file
(setq auth-sources
      '((:source "~/.authinfo.gpg")
        (:source "~/.authinfo")))


(use-package files
  :custom
  (large-file-warning-threshold nil)
  (find-file-visit-truename t)
  (backup-by-copying t)
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 2)
  (version-control t)
  (backup-directory-alist
   `(("." . ,temporary-file-directory)))
  (auto-save-file-name-transforms
   `((".*" ,temporary-file-directory t))))


(setq vc-follow-symlinks t
      delete-by-moving-to-trash t
      ;; Prevent issues with build-watchers
      create-lockfiles nil)


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
  :custom
  (evil-snipe-scope 'whole-visible)
  (evil-snipe-repeat-scope 'whole-visible)
  (evil-snipe-spillover-scope 'whole-buffer)
  ;; see: https://github.com/emacs-evil/evil-collection/tree/master/modes/magit#known-conflicts
  :config
  (push 'magit-mode evil-snipe-disabled-modes)
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1))

;; TODO: create own lightweight evil-commentary

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
  :custom
  (which-key-idle-delay 0.75)
  :config
  (which-key-mode 1))

;; Ivy is a generic completion mechanism for Emacs.
(use-package ivy
  :ensure t
  :hook (after-init . ivy-mode)
  :custom
  (ivy-wrap t)
  ;; Add bookmarks and recentf to buffer list
  (ivy-use-virtual-buffers t)
  (ivy-height 12)
  (ivy-auto-select-single-candidate t)
  ;; Do not close the minibuffer with delete
  (ivy-on-del-error-function nil)
  ;; Always use fuzzy search except swiper
  (ivy-re-builders-alist
   '((read-file-name-internal . ivy--regex-fuzzy)
     (t . ivy--regex-plus)))
  :bind
  (:map ivy-minibuffer-map
        ("RET" . ivy-done)
        ("TAB" . ivy-next-line)
        ("<backtab>" . ivy-previous-line)))

;; TODO: https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-ivy.el
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


;; Emacs file manager
;; TODO: setup keymaps
(use-package dired
  :commands (dired dired-jump)
  :custom
  (dired-auto-revert-buffer t) ;; NOTE: describe variable
  (dired-dwim-target t) ;; NOTE: describe variable
  (dired-hide-details-hide-symlink-targets nil) ;; ...
  (dired-recursive-copies 'always))

;; TODO: Add shortcuts for image-increate-size and image-decrease-size and also always select image
;; on entering this mode
;; REVIEW: is this config necessary?
(use-package image-mode
  :commands image-mode
  :custom
  (image-auto-resize-on-window-resize t)
  :bind
  (:map image-mode-map
        ("+" . image-increase-size)
        ("-" . image-decrease-size)))

;; DEPRECATED: Last update on 12.5.20
(use-package pdf-tools
  :ensure t
  :mode
  ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-loader-install)
  :custom
  (pdf-view-display-size 'fit-page))


;; ein for interacting with notebooks
;; (use-package ein
;;   :ensure t
;;   :defer t)

;; (use-package tex
;;   :defer t
;;   :ensure auctex
;;   :config
;;   (TeX-source-correlate-mode)
;;   :custom
;;   (TeX-command-extra-options "--shell-escape")
;;   (TeX-source-correlate-start-server t))
;; SEE: https://www.gnu.org/software/auctex/manual/auctex.html
;; (use-package latex
;;   :mode ("\\.tex\\'" . latex-mode)
;;   :custom
;;   (LaTeX-section-hook
;;    '(LaTeX-section-heading
;; 	 LaTeX-section-title
;; 	 LaTeX-section-toc
;; 	 LaTeX-section-section
;; 	 LaTeX-section-label)))

;; (use-package tex
;;   :after latex
;;   :custom
;;   (TeX-auto-save t)
;;   (TeX-parse-self t)
;;   (TeX-view-program-selection '((output-pdf "PDF Tools")))
;;   :config
;;   (setq-default TeX-master nil))

;; https://joostkremers.github.io/ebib/ebib-manual.html
;; (use-package ebib
;;   :ensure t
;;   :defer t)


;; A major mode for convenient plain text markup — and much more.
;; TODO: https://www.youtube.com/watch?v=PNE-mgkZ6HM
(use-package org
  :ensure t
  :commands org-mode
  ;; TODO: implement these on my own https://github.com/edwtjo/evil-org-mode
  :custom
  (org-directory "~/Nextcloud/Notes")
  (org-agenda-files '("~/Nextcloud/Notes/" "~/Dokumente/"))
  :general
  (leader-key
    "o"   '(:ignore t :which-key "Org")
    "oa" 'org-agenda)
  ;; Only show these bindings when in org-mode
  (leader-key
    :states 'normal
    :keymaps 'org-mode-map
    "os"   '(:ignore t :which-key "Show")
    "osa" 'org-show-all
    "ost" 'org-show-todo-tree
    "osc" 'org-show-children
    "oss" 'org-show-subtree))


;; TODO: configure
(use-package eshell
  :commands eshell)

(use-package vterm
  :ensure t
  :commands vterm
  :bind
  ([remap term] . vterm)
  :hook
  (vterm-mode . (lambda ()
                  (setq-local evil-insert-state-cursor 'box
                              evil-move-cursor-back nil)))
  :custom
  (vterm-kill-buffer-on-exit t)
  (vterm-max-scrollback 5000))


(use-package mu4e
  :load-path "/usr/local/share/emacs/site-lisp/mu4e/"
  :commands mu4e
  :init
  ;; use mu4e for Emails in Emacs
  (setq mail-user-agent 'mu4e-user-agent)
  :general
  (leader-key
    "m"   '(mu4e :which-key "Mail"))
  :custom
  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  (mu4e-change-filenames-when-moving t)
  ;; Refresh mail using isync every 15 minutes
  (mu4e-update-interval (* 15 60))
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-compose-in-new-frame nil) ;; Default value
  ;; don't save message to Sent Messages, IMAP takes care of this
  (mu4e-sent-messages-behavior 'delete)
  (mu4e-context-policy 'pick-first)
  (mu4e-compose-context-policy 'always-ask)
  ;; Format composed mails
  (mu4e-compose-format-flowed t)
  (mu4e-view-show-images nil);; Default value
  (mu4e-confirm-quit t) ;; Default value
  (mu4e-maildir "~/.mail")
  (mu4e-bookmarks
   '((:name "Unread messages" :query "flag:unread AND NOT flag:trashed" :key ?i)
     (:name "Today's messages" :query "date:today..now" :key ?t)
     (:name "Last 7 days" :query "date:7d..now" :hide-unread t :key ?w))))





(use-package smtpmail
  :after mu4e-context ;; FIXME: Find more efficient way
  :custom
  (message-send-mail-function 'smtpmail-send-it))


;; A Git Porcelain inside Emacs.
;; FIXME: doesn't work right now (issue with 'with-editor')
(use-package magit
  :ensure t
  :commands (magit-status magit-get-current-branch)
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


(use-package projectile
  :ensure t
  :general
  (leader-key "p"
    '(:prefix-map projectile-command-map :which-key "Project"))
  :custom
  (projectile-switch-project-action #'projectile-dired)
  (projectile-sort-order 'recently-active)
  :config
  (projectile-mode 1))


;; REVIEW: Global really necessary?
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)


(use-package company
  :ensure t
  :commands company-mode
  :hook ((text-mode prog-mode) . company-mode)
  :bind
  (:map company-active-map
        ("RET" . company-complete-selection)
        ("TAB" . company-select-next)
        ("<backtab>" . company-select-previous))
  :custom
  ;; TODO: which backends to use?
  (company-backends '((company-capf :with company-yasnippet)
                      (company-dabbrev-code company-keywords company-files company-dabbrev)))
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-other-buffers nil)
  (company-idle-delay 0)
  (company-minimum-prefix-length 1)
  (company-require-match nil)
  (company-selection-wrap-around t)
  (company-tooltip-width-grow-only t))


(use-package flyspell
  :commands (flyspell-mode flyspell-prog-mode)
  :custom
  (flyspell-delay 0.25)
  (flyspell-issue-message-flag nil)
  :hook
  (text-mode . flyspell-mode)
  (prog-mode . flyspell-prog-mode))

;; FIXME: Other way to do this or using aspell? Recater this anyway!
(use-package ispell
  :after flyspell
  :custom
  (ispell-program-name "hunspell")
  (ispell-dictionary "en_US,de_DE")
  :config
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
  (leader-key
    :states 'normal
    :keymaps 'lsp-mode-map
    "c" '(:keymap lsp-command-map :which-key "Code"))
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


(use-package rustic
  :ensure t
  :mode ("\\.rs\\'" . rustic-mode))


(use-package go-mode
  :ensure t
  :mode "\\.go\\'")


(use-package python
  :mode ("\\.py\\'" . python-mode))

(use-package pyvenv
  :ensure t
  :commands pyvenv-mode
  :hook (python-mode . pyvenv-mode))


(use-package js
  :mode ("\\.js\\'" . js-mode))

(use-package typescript-mode
  :ensure t
  :mode "\\.ts\\'"
  :custom
  (typescript-indent-level 2))

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


;; Load the private configuration
(require 'private)


(provide 'init)
;;; init.el ends here

