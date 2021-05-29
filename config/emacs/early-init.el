;; -*- lexical-binding: t; -*-
;;; early-init.el ---  This file is loaded before the package system and Frame is initialized

(when (fboundp 'native-compile-async)
  (setq comp-deferred-compilation nil
        comp-speed 2)) ; Maximal optimizations; default value

;; NOTE: do not use 'most-positive-fixnum', this will cause Emacs to freeze on the first start
(custom-set-variables
 '(gc-cons-threshold (* 100 1024 1024))
 '(gc-cons-percentage 0.6))

(defun my/reset-gc-settings ()
  "Reset the garbage collection settings."
  (setq gc-cons-threshold (* 10 1024 1024)
        read-process-output-max (* 1024 1024)
        gc-cons-percentage 0.1)
  ;; Run a garbage collection when everything else is done
  (garbage-collect))

(add-hook 'emacs-startup-hook #'my/reset-gc-settings)


;; Make sure that the default-directory stays this current one.
;; This way loading other conif-files can be done without specifying
;; the entire path.
(setq default-directory user-emacs-directory
      ;; Install packages in ~/.local/share not ~/.config
      user-emacs-directory (expand-file-name
                            "emacs"
                            (or (getenv "XDG_DATA_HOME") "~/.local/share"))
      ;; Write to /dev/null
      custom-file null-device
      ;; Save temporary file under /tmp/emacs<uid>
      temporary-file-directory (expand-file-name
                                (format "emacs%d" (user-uid))
                                temporary-file-directory))

;; This directory will not be created automatically
(make-directory temporary-file-directory t)

;; Toggle interface elements
(custom-set-variables
 '(menu-bar-mode nil)
 '(tool-bar-mode nil)
 '(scroll-bar-mode nil)
 '(tooltip-mode nil)
 '(blink-cursor-mode nil)
 '(cursor-in-non-selected-windows nil)
 ;; No need for line- and column numbers in the mode-line
 '(column-number-mode nil)
 '(line-number-mode nil))

;; Prevent the frame from changing size during startup.
(customize-set-variable 'frame-inhibit-implied-resize t)

(setq initial-frame-alist '((title        . "GNU Emacs")
                            (width        . 150)
                            (height       . 50)
                            (alpha        . (100 . 90))
                            (left-fringe  . 20)
                            (right-fringe . 0))
      default-frame-alist initial-frame-alist)

;; prevent package.el loading packages prior to their init-file loading.
(customize-set-variable 'package-enable-at-startup nil)

(provide 'early-init)
;;; early-init.el ends here
