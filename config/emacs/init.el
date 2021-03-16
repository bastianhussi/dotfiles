;; -*- lexical-binding: t; -*-
;; init.el --- Where it all begins...

(org-babel-load-file "config.org")

(defvar my/private-config "private.org")
(when (file-exists-p my/private-config)
  (org-babel-load-file my/private-config))

;; set the default-directory to $HOME.
(cd "~/")

(provide 'init)
;;; init.el ends here
