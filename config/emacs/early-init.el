;; -*- lexical-binding: t; -*-
;;; early-init.el ---  This file is loaded before the package system and Frame is initialized

;; NOTE: do not use 'most-positive-fixnum', this will cause Emacs to freeze on the first start
(setq gc-cons-threshold (* 100 1024 1024)
      gc-cons-percentage 0.6)

(defun my/reset-gc-settings ()
  "Reset the garbage collection settings."
  (message "Emacs loaded in %.2f seconds 🚀" (string-to-number (emacs-init-time)))
  (setq gc-cons-threshold (* 10 1024 1024)
        gc-cons-percentage 0.1
        read-process-output-max (* 1024 1024))
  ;; Run a garbage collection when everything else is done
  (garbage-collect))

(add-hook 'emacs-startup-hook #'my/reset-gc-settings)

;; Make sure that the default-directory stays this current one. This way loading other conif-files
;; can be done without specifying the entire path.
(setq default-directory user-emacs-directory
      ;; Install packages in ~/.local/share not ~/.config
      ;; If the XDG_DATA_HOME variable is set use it. Otherwise fall back to ~/.local/share/
      user-emacs-directory (expand-file-name "emacs" (or (getenv "XDG_DATA_HOME") "~/.local/share"))
      ;; Write to /dev/null
      custom-file null-device
      ;; Save temporary file under /tmp/emacs<uid>
      temporary-file-directory (expand-file-name (format "emacs%d" (user-uid)) temporary-file-directory))

;; This directory will not be created automatically
(make-directory temporary-file-directory t)


(require 'comp)
(setq comp-async-report-warnings-errors nil
      comp-speed 2) ;; Maximal optimizations; default value

(setq
 load-prefer-newer t
 ;; prevent package.el loading packages prior to their init-file loading.
 package-enable-at-startup nil)


;; Toggle interface elements
(customize-set-variable 'menu-bar-mode  nil)
(customize-set-variable 'tool-bar-mode nil)
(customize-set-variable 'scroll-bar-mode nil)
(customize-set-variable 'tooltip-mode nil)
(customize-set-variable 'blink-cursor-mode nil)
(customize-set-variable 'column-number-mode nil)

;; Whether frames should be resized implicitly. Prevent Emacs from changing it's size during startup.
(setq frame-inhibit-implied-resize t
      initial-frame-alist '((title . "GNU Emacs")
                            (width . 150)
                            (height . 50)
                            (alpha . (100 . 95))
                            (left-fringe . 20)
                            (right-fringe . 0))
      default-frame-alist initial-frame-alist)


(provide 'early-init)
;;; early-init.el ends here
