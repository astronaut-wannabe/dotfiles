(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Bootstrap 'use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; move all the auto-generated stuff to a separate file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(org-babel-load-file (expand-file-name "~/.emacs.d/emacs.org"))
