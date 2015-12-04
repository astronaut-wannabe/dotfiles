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

;; default to better frame titles

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))

;; default to unified diffs
(setq diff-switches "-u")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (deeper-blue))))
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

;; add additional package repositories (gnu is the only one that comes built in)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

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

;; turn on css mode for sass
(setq auto-mode-alist
      (append '(("\\.scss$" . css-mode))
              auto-mode-alist))
(setq css-indent-offset 2)

;; aspell/ispell setup
(setq ispell-dictionary "american")
(setq ispell-program-name "/usr/local/bin/aspell")
(setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))
