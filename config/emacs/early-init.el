;;; early-init.el ---  This file is loaded before the package system and GUI is initialized  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Bastian Hussi

;; Author: Bastian Hussi <bastian@ipfso.de>
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

;; NOTE: do not use 'most-positive-fixnum', this will cause Emacs to freeze on the first start
(setq gc-cons-threshold (* 100 1024 1024)
      gc-cons-percentage 0.6)


(add-hook 'emacs-startup-hook
          (lambda ()
            ;; Set the working directory to home regardless of where Emacs was started from
            (message "Emacs loaded in %.2f seconds 🚀" (string-to-number (emacs-init-time)))
            (setq gc-cons-threshold (* 10 1024 1024)
                  read-process-output-max (* 1024 1024)
                  gc-cons-percentage 0.1)
            ;; Run a garbage collection when everything else is done
            (garbage-collect)))


;; Toggle interface elements
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)
(column-number-mode 1)

;; Whether frames should be resized implicitly. Prevent Emacs from changing it's size during startup.
(setq frame-inhibit-implied-resize t
      default-frame-alist '((undecorated . t) ;; No gtk Title bar
                            (alpha . (95 . 95)) ;; Transparency
                            (fullscreen . maximized) ;; Width in columns
                            (left-fringe . 20)
                            (right-fringe . 0)))


;; tell the native-comp system to automatically generate the natively compiled files when Emacs loads a new .elc file
(setq comp-deferred-compilation t)

;; prevent package.el loading packages prior to their init-file loading.
(setq package-enable-at-startup nil)


(provide 'early-init)
;;; early-init.el ends here
