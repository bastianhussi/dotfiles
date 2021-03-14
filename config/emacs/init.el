;; -*- lexical-binding: t; -*-
;; init.el --- Where it all begins...

(org-babel-load-file "config.org")
(org-babel-load-file "private.org")

;; set the default-directory to $HOME.
(cd "~/")

(provide 'init)
;;; init.el ends here
