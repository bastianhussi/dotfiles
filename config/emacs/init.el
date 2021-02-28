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
;; TODO: use org-file for configuration
;; FIXME: missing output on docker build
;; FIXME: a lot of ansi escape sequences when failing to install lsp-servers
;; TODO: remap C-y into C-shift-c in minibuffer to past passwords
;; REVIEW: reduce startup-time even more (better than 0.7-0.8, currently 1.3)?


(add-hook 'after-init-hook
          (lambda ()
            ;; Always set the home directory to the current buffers default directory.
            ;; NOTE: invoking this earlier doesn't work
            (cd "~/")
            (global-auto-revert-mode 1)
            ;; Sync mail in background
            (mu4e t)
            ;; Start server if its not already running
            (require 'server)
            (unless (server-running-p)
              (server-start))))


(setq user-full-name "Bastian Hussi")

(setq default-directory user-emacs-directory
      user-emacs-directory (expand-file-name "emacs" (or (getenv "XDG_DATA_HOME") "~/.local/share"))
      ;; Save temporary file under /tmp/emacs<uid>
      temporary-file-directory (expand-file-name (format "emacs%d" (user-uid)) temporary-file-directory)
      custom-file (expand-file-name "custom.el" user-emacs-directory))


;; The default directory should stay $XDG_CONFIG_HOME or ~/.config/emacs
;; Install packages in ~/.local/share not ~/.config
;; If the XDG_DATA_HOME variable is set use it. Otherwise fall back to ~/.local/share/

(when (file-exists-p custom-file)
  (load custom-file))
;; This directory will not be created automatically
(make-directory temporary-file-directory t)


;; Setup GnuTLS for package downloads and sending mail with mu4e
(with-eval-after-load 'gnutls
  (eval-when-compile
    (require 'gnutls))
  (add-to-list 'gnutls-trustfiles "~/.ssl/certs/*.pem") ;; Path to self signed certificates.
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
  (defvar font-size 160)
  (set-face-attribute 'default nil :font "Fira Code Retina" :height font-size)
  (set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height font-size)
  (set-face-attribute 'variable-pitch nil :font "Cantarell" :height font-size :weight 'regular)
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
   'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
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


;; Setup the colorscheme and add a nice looking modeline
(use-package doom-themes
  :straight t
  :config
  (load-theme 'doom-dracula t)
  (doom-themes-org-config)
  (doom-themes-visual-bell-config))

(use-package doom-modeline
  :straight t
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

;; Use tabs within Emacs. The tabbar is only visible when two or more tabs are open
(use-package tab-bar
  :custom
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  (tab-bar-new-tab-choice "*scratch*") ;; New tabs will show the scratch-buffer
  (tab-bar-new-tab-to 'rightmost)) ;; Always add new tabs to the rightmost position


(setq epa-file-cache-passphrase-for-symmetric-encryption t
      epa-file-select-keys nil)

;; Prefer the encrypted authinfo-file
(setq auth-sources '((:source "~/.authinfo.gpg")
                     (:source "~/.authinfo")))


;; Sadly this is the only way Emacs will respect variables set by zsh.
;; FIXME: Is there really no other option?
(use-package exec-path-from-shell
  :straight t
  :custom
  (exec-path-from-shell-arguments '("-l")) ;; Speed things up a bit
  :config
  (if (daemonp)
      (add-hook 'server-after-make-frame-hook #'exec-path-from-shell-initialize)
    (exec-path-from-shell-initialize)))


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


;; Vim within Emacs.
(use-package evil
  :straight t
  :custom
  (evil-want-keybinding nil)
  (evil-want-integration t)
  (evil-want-fine-undo nil)
  (evil-want-C-i-jump t)
  ;; TODO: Use undo-redo when Emacs v28 gets released.
  (evil-undo-system 'undo-fu)
  (evil-move-beyond-eol t)
  ;; You can't escape vim
  (evil-toggle-key "")
  :config
  ;; TODO: remove when Emacs v28 gets out.
  (use-package undo-fu :straight t)
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


;; Convenient way to manage keybindings
(use-package general
  :straight t
  :config
  (general-evil-setup t)
  (general-setq evil-search-module 'evil-search)
  (general-create-definer leader-key
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))

;; Displays key bindings following the currently entered incomplete command in a popup.
(use-package which-key
  :straight t
  :custom
  (which-key-idle-delay 0.75)
  :config
  (which-key-mode 1))


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
  ;; FIXME: improve this settings (fuzzy doesn't work for files, recentf, ...)
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

;; TODO: https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-ivy.el
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
  (ivy-mode . counsel-mode)) ;; NOTE: Only calling this in :config doesn't work

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
  (dired-auto-revert-buffer t) ;; NOTE: describe variable
  (dired-dwim-target t) ;; NOTE: describe variable
  (dired-listing-switches "-Ahlv --group-directories-first") ;; Change the arguments passed to ls
  (dired-hide-details-hide-symlink-targets nil) ;; ...
  (dired-recursive-copies 'always)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory ;; go up a directory
    "l" 'dired-find-file)) ;; go into the selected directory


(use-package tex-site
  :mode ("\\.tex\\'" . LaTeX-mode))

(use-package latex
  :commands (LaTeX-mode)
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
  (TeX-view-program-selection
   '((output-pdf "xdg-open")
     (output-html "xdg-open")))
  :config
  (setq-default TeX-master nil))

(use-package reftex
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
  (ebib-bibtex-dialect 'BibTeX))


;; A major mode for convenient plain text markup — and much more.
;; TODO: https://www.youtube.com/watch?v=PNE-mgkZ6HM
(use-package org
  :straight t
  :commands (org-mode org-agenda)
  ;; TODO: implement these on my own https://github.com/edwtjo/evil-org-mode
  :custom
  (org-directory "~/Nextcloud/Notes/")
  (org-agenda-files (list (expand-file-name "todo.org" org-directory)))
  (org-log-done 'time) ;; Add timestamp whenever task is finished
  (org-log-into-drawer t)
  (org-agenda-start-with-log-mode t)
  :general
  ;; Only show these bindings when in org-mode
  (leader-key
    "o"   '(:ignore t :which-key "Org")
    "oa" 'org-agenda))


;; TODO: configure
(use-package eshell
  :commands eshell)

(use-package vterm
  :straight t
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


(use-package projectile
  :straight t
  :general
  (leader-key "p"
    '(:prefix-map projectile-command-map :which-key "Project"))
  :custom
  (projectile-switch-project-action #'projectile-dired)
  (projectile-sort-order 'recently-active)
  :hook
  (after-init . projectile-mode))


;; REVIEW: Global really necessary?
(use-package yasnippet
  :straight t
  :commands yas-global-mode
  :hook (after-init . yas-global-mode))

(use-package yasnippet-snippets
  :straight t
  :after yasnippet)

(use-package company
  :straight t
  :commands company-mode
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
  :straight t
  :commands flycheck-mode
  :bind (("C-j" . next-error) ("C-k" . previous-error))
  :hook ((prog-mode org-mode) . flycheck-mode))


(use-package lsp-mode
  :straight t
  :commands lsp-deferred
  :custom
  (lsp-diagnostic-package :flycheck)
  (lsp-prefer-capf t)
  (read-process-output-max (* 1024 1024))
  (lsp-rust-server 'rust-analyzer)
  :bind
  (:map lsp-mode-map
        ("K" . lsp-describe-thing-at-point)
        ("gi" . lsp-goto-implementation)
        ("gr" . lsp-find-references)
        ("gd" . lsp-find-definition)
        ("gD" . lsp-find-declaration))
  :general
  (leader-key
    :states 'normal
    :keymaps 'lsp-mode-map
    "c" '(:keymap lsp-command-map :which-key "Code"))
  :hook
  (before-save . lsp-format-buffer))

(use-package lsp-ui
  :straight t
  :after lsp-mode
  :custom
  (lsp-ui-doc-max-width 80)
  (lsp-ui-doc-max-height 60)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-delay 0.25))

(use-package lsp-ivy
  :straight t
  :after lsp-mode
  :commands lsp-ivy-workspace-symbol)


(use-package rustic
  :straight t
  :mode ("\\.rs\\'" . rustic-mode)
  :hook (rustic-mode . lsp-deferred))

(use-package go-mode
  :straight t
  :mode "\\.go\\'"
  :hook (go-mode . lsp-deferred))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :hook (python-mode . lsp-deferred))

(use-package pyvenv
  :straight t
  :commands pyvenv-mode
  :hook (python-mode . pyvenv-mode))

(use-package js
  :mode ("\\.js\\'" . js-mode)
  :hook (js-mode . lsp-deferred))

(use-package typescript-mode
  :straight t
  :mode "\\.ts\\'"
  :custom
  (typescript-indent-level 2)
  :hook (typescript-mode . lsp-deferred))


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
  (web-mode . lsp-deferred))

(use-package sgml-mode
  :mode "\\.html?\\'"
  :custom
  (sgml-basic-offset 2)
  :hook
  (sgml-mode . lsp-deferred))

(use-package css-mode
  :mode "\\.css\\'"
  :custom
  (css-indent-offset 2)
  :hook
  (css-mode . lsp-deferred))

(use-package json-mode
  :straight t
  :mode "\\.json\\'"
  :hook
  (json-mode . lsp-deferred))

(use-package yaml-mode
  :straight t
  :mode "\\.ya?ml\\'"
  :hook
  (yaml-mode . lsp-deferred))

(use-package nix-mode
  :straight t
  :mode "\\.nix\\'")

(use-package dockerfile-mode
  :straight t
  :mode "Dockerfile\\'"
  :hook
  (dockerfile-mode . lsp-deferred))

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


(general-nmap
  "gc" 'comment-line)
(general-vmap
  "gc" 'comment-or-uncomment-region)


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
  "jd" '(dired-jump :which-key "Directory")
  "q"   '(:ignore t :which-key "Quit")
  "qq" '(save-buffers-kill-terminal :which-key "Emacs")
  "qb" '(kill-this-buffer :which-key "Buffer")
  "qt" '(tab-close :which-key "Tab")
  "qw" '(delete-window :which-key "Window"))


(defalias 'eb 'eval-buffer)
(defalias 'kb 'kill-buffer)
(defalias 'dr 'desktop-remove)
(defalias 'cp 'check-parens)
(defalias 'lt 'load-theme t)

(load-file "private.el")

(provide 'init)
;;; init.el ends here
