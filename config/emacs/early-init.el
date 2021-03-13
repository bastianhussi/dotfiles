;; -*- lexical-binding: t; -*-
;;; early-init.el ---  This file is loaded before the package system and Frame is initialized

;; NOTE: do not use 'most-positive-fixnum', this will cause Emacs to freeze on the first start
(setq gc-cons-threshold (* 100 1024 1024)
      gc-cons-percentage 0.6)


(defun my/reset-gc-settings ()
  "Reset the garbage collection settings."
  (message "Emacs loaded in %.2f seconds 🚀" (string-to-number (emacs-init-time)))
  (setq gc-cons-threshold (* 10 1024 1024)
        read-process-output-max (* 1024 1024)
        gc-cons-percentage 0.1)
  ;; Run a garbage collection when everything else is done
  (garbage-collect))

(add-hook 'emacs-startup-hook #'my/reset-gc-settings)


;; Toggle interface elements
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(column-number-mode 1)


;; Whether frames should be resized implicitly. Prevent Emacs from changing it's size during startup.
(setq frame-inhibit-implied-resize t
      default-frame-alist '((title . "GNU Emacs")
                            (width . 150)
                            (height . 50)
                            (left-fringe . 25)
                            (right-fringe . 0)))


;; Make sure that the default-directory stays this current one. This way loading other conif-files
;; can be done without specifying the entire path.
(setq default-directory user-emacs-directory
      ;; Install packages in ~/.local/share not ~/.config
      ;; If the XDG_DATA_HOME variable is set use it. Otherwise fall back to ~/.local/share/
      user-emacs-directory (expand-file-name "emacs" (or (getenv "XDG_DATA_HOME") "~/.local/share"))
      ;; Save temporary file under /tmp/emacs<uid>
      temporary-file-directory (expand-file-name (format "emacs%d" (user-uid)) temporary-file-directory)
      ;; Write to /dev/null
      custom-file null-device)

;; This directory will not be created automatically
(make-directory temporary-file-directory t)


;; prevent package.el loading packages prior to their init-file loading.
(setq package-enable-at-startup nil)


(require 'comp)
;; Max optimization level for compiling packages.
(setq comp-speed 2
      ;; Do not report native compile warnings
      comp-async-report-warnings-errors nil)

;; Change the directory where the  natively compiled *.eln files will be cached
(when (boundp 'comp-eln-load-path)
  (setcar comp-eln-load-path
          (expand-file-name "cache/eln-cache/" user-emacs-directory)))


(provide 'early-init)
;;; early-init.el ends here
