(setq inhibit-startup-message t)

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(
                         ;; ("org"              . "http://orgmode.org/elpa/") 
                         ("gnu"             . "http://elpa.gnu.org/packages/")
                         ("melpa"         . "http://melpa.milkbox.net/packages/")))
                         ;;("marmalade" . "http://marmalade-repo.org/packages/")))

(package-initialize)

;; Bootstrap 'use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; try packages out before installing them
(use-package try
  :ensure t)

;; open a help menu if you pause in the middle
;; of a command sequence
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; org-mode stuff
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
 
