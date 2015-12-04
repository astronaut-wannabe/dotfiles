;; get rid of welcome message
(setq inhibit-startup-message t)

;; In case this is the first time running this on a computer, we need to make sure the following directories have been created.
(defconst mb/emacs-directory (concat (getenv "HOME") "/.emacs.d/"))
(defun mb/emacs-subdirectory (d) (expand-file-name d mb/emacs-directory))

(let* ((subdirs '("elisp" "backups" "snippets" "ac-dict"))
       (fulldirs (mapcar (lambda (d) (mb/emacs-subdirectory d)) subdirs)))
  (dolist (dir fulldirs)
    (when (not (file-exists-p dir))
      (message "Make directory: %s" dir)
      (make-directory dir))))

;; Extra packages not available via the package manager go here
(add-to-list 'load-path (mb/emacs-subdirectory "elisp"))

;; set up the package manager repos
(require 'package)
(setq package-archives '(;;("org"       . "https://orgmode.org/elpa/") org is slow
                         ("gnu"       . "https://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")
                         ;;("marmalade" . "https://marmalade-repo.org/packages/") marmalade's https cert is weird...
                         ))

;; This must come before configurations installed packages.
(package-initialize)
(package-refresh-contents)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))

;; default to unified diffs
(setq diff-switches "-u")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (deeper-blue)))
 '(package-selected-packages
   (quote
    (spaceline fancy-narrow ruby-tools inf-ruby rvm json-mode json-reformat projectile org-beautify-theme org-bullets gh-md markdown-mode markdown-mode+ magit))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; set lisp to use with slime
(setq inferior-lisp-program "/usr/bin/clisp")

;; save all auto-backups in a single directory
(setq backup-directory-alist `(("." . ,(expand-file-name "~/.emacs-backups"))))

(global-set-key "\M-?" 'help-command)

;; define keys to move between buffer windows
(global-set-key "\C-x\C-n" 'other-window)
(defun other-window-backward (&optional n)
  "Select the Nth previous window."
  (interactive "P")
  (other-window (- (prefix-numeric-value n))))
(global-set-key "\C-x\C-p" 'other-window-backward)

;; add single line scrolling
(defalias 'scroll-ahead 'scroll-up)
(defalias 'scroll-behind 'scroll-down)
(defun scroll-n-lines-ahead (&optional n)
  "Scroll ahead N lines (1 by default)."
  (interactive "P")
  (scroll-ahead (prefix-numeric-value n)))
(defun scroll-n-lines-behind (&optional n)
  "Scroll ahead N lines (1 by default)."
  (interactive "P")
  (scroll-behind (prefix-numeric-value n)))
(global-set-key "\C-q" 'scroll-n-lines-behind)
(global-set-key "\C-z" 'scroll-n-lines-ahead)
(global-set-key "\C-x\C-q" 'quoted-insert)

;; spaces, not tabs
(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(setq-default tab-always-indent 'complete)

;; turn on css mode for sass
(setq auto-mode-alist
      (append '(("\\.scss$" . css-mode))
              auto-mode-alist))
(setq css-indent-offset 2)

;; aspell/ispell setup
(setq ispell-dictionary "american")
(setq ispell-program-name "/usr/local/bin/aspell")
(setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))

;; Some visual tweaks
(setq initial-scratch-message "")

(when (window-system)
  ;; (tool-bar-mode 0) uncomment if I get sick of toolbar
  (when (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))
  (scroll-bar-mode -1))

(require 'spaceline-config)
(spaceline-emacs-theme)

;; Programming Languages...

(if (file-exists-p "~/.init-ruby.el")
    (load "~/.init-ruby.el")
  (message "Missing modules file %s" "init-ruby.el")
  (message "You can get started by copying the bundled example file"))
(require 'init-ruby)
