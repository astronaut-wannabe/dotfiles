(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(tool-bar-mode -1)

;; spaces, not tabs
(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(setq-default tab-always-indent 'complete)

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

;; set up ido for fuzzy auto-complete of everything (buffers, commands, etc.)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; alias list-buffers to more useful mode
;; (defalias 'list-buffers 'ibuffer)
(defalias 'list-buffers 'ibuffer-other-window)

;; ace-window for buffer switching
(use-package ace-window
  :ensure t
  :diminish ace-window-mode
  :init
  (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l ?o))
  (global-set-key (kbd "C-x o") 'ace-window)
  (custom-set-faces '(aw-leading-char-face ((t (:foreground "red" :height 3.0))))))

;; auto-generated stuff
;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (railscasts)))
 '(custom-safe-themes
   (quote
    ("3b0a350918ee819dca209cec62d867678d7dac74f6195f5e3799aa206358a983" default)))
 '(package-selected-packages (quote (lorem-ipsum))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:foreground "red" :height 3.0)))))
