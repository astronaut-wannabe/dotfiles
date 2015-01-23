;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))

;; default to unified diffs
(setq diff-switches "-u")

;; save all auto-backups in a single directory
(setq backup-directory-alist `(("." . ,(expand-file-name "~/.emacs-backups"))))

;; add additional package repositories (gnu is the only one that comes built in)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
