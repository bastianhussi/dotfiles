;; -*- lexical-binding: t; -*-
;; init.el --- Where it all begins...

;; Literate config-files and their lisp counterpart.
;; Org-files should be tangled to the matching location and loaded from there.

(defcustom my/config-files-alist
  (list (cons (expand-file-name "config.org" default-directory)
              (locate-user-emacs-file "config.el"))
        (cons (expand-file-name "private.org" default-directory)
              (locate-user-emacs-file "private.el")))
  "List of config files."
  :type
  '(alist :key-type (file :tag "File" :must-match t)
          :value-type (file :tag "Target"))
  :group 'my/config-file)

(defun my/config-file-sync (file target)
  "Tangle the FILE and load the TARGET."
  (require 'ob-tangle)
  (org-babel-tangle-file file target)
  (load-file target))

(defun my/config-rebuild ()
  "Rebuild all config-files in `my/config-files-alist'."
  (interactive)
  (mapcar (lambda (element)
            (let ((file (car element))
                  (target (cdr element)))
              (my/config-file-sync file target)))
          my/config-files-alist))

(defun my/config-rebuild-on-save ()
  "Rebuild a config-file in `my/config-files-alist' when saving the buffer."
  (let ((element (assoc (buffer-file-name) my/config-files-alist)))
    (when element
      (let ((file (car element))
            (target (cdr element)))
        (add-hook 'after-save-hook
                  (lambda () (org-babel-tangle-file file target)) 100 t)))))

(add-hook 'find-file-hook #'my/config-rebuild-on-save)

;; Check if lisp-files defined in `my/config-files-alist' exits.
;; If so load them. Otherwiese tangle the org-files if they exists and load the results.
(let ((file-name-handler-alist nil)
      (reload-org nil))
  (mapc (lambda (element)
            (let ((file (car element))
                  (target (cdr element)))
              (if (file-exists-p target)
                  (load-file target)
                (setq reload-org t)
                (my/config-file-sync file target))))
        my/config-files-alist)
  ;; NOTE: prevent a mixed-install of Orgmode.
  (when reload-org
    (org-reload)))

;; Set the default directory to $HOME
(cd "~/")
