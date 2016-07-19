(require 'package)

(setq package-archives '(
                         ;; ("org"              . "http://orgmode.org/elpa/") 
                         ("gnu"             . "http://elpa.gnu.org/packages/")
                         ("melpa"         . "http://melpa.milkbox.net/packages/")))
                         ;;("marmalade" . "http://marmalade-repo.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; This must come before configurations installed packages.
(package-refresh-contents)

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
 '(coffee-tab-width 2)
 '(custom-enabled-themes (quote (railscasts)))
 '(custom-safe-themes
   (quote
    ("3b0a350918ee819dca209cec62d867678d7dac74f6195f5e3799aa206358a983" default)))
 '(explicit-shell-file-name "/usr/local/bin/bash")
 '(global-auto-revert-mode t)
 '(js-indent-level 2)
 '(magit-pull-arguments (quote ("--rebase")))
 '(package-selected-packages
   (quote
    (pdf-tools rspec-mode markdown-preview-eww twittering-mode ox-jira rails-log-mode flymake-haml projectile ace-jump-buffer rust-mode zone-sl writegood-mode writeroom-mode yaml-mode sourcemap coffee-mode flymake-coffee haml-mode paradox ace-jump-mode rainbow-blocks rainbow-delimiters zone-rainbow evil-leader highlight-blocks projectile-rails evil csv-mode rubocop flycheck thingatpt+ expand-region mustache-mode restclient ace-window railscasts-theme flx-ido ag yasnippet company-inf-ruby ruby-block ruby-end bundler company company-quickhelp spaceline fancy-narrow ruby-tools inf-ruby rvm json-mode json-reformat org-beautify-theme org-bullets gh-md markdown-mode markdown-mode+ magit)))
 '(paradox-automatically-star t)
 '(projectile-global-mode t)
 '(rspec-use-spring-when-possible nil)
 '(tool-bar-mode nil))

;; set lisp to use with slime
(load (expand-file-name "~/quicklisp/slime-helper.el"))
;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program "/usr/local/bin/sbcl")

;; save all auto-backups in a single directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (mb/emacs-subdirectory "backups")))))

(global-set-key "\M-?" 'help-command)

;; define keys to move between buffer windows
(global-set-key "\C-x\C-n" 'other-window)
(defun other-window-backward (&optional n)
  "Select the Nth previous window."
  (interactive "P")
  (other-window (- (prefix-numeric-value n))))
(global-set-key "\C-x\C-p" 'other-window-backward)

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
  (tool-bar-mode 0)
  (when (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))
  (scroll-bar-mode -1))

;; Programming Languages...

(if (file-exists-p "~/.init-ruby.el")
    (load "~/.init-ruby.el")
  (message "Missing modules file %s" "init-ruby.el")
  (message "You can get started by copying the bundled example file"))
(require 'init-ruby)

(add-hook 'after-init-hook 'global-company-mode)
(company-quickhelp-mode 1)

;; set up coffeescript mode
(use-package coffee-mode
  :ensure t
  :pin melpa
  :config (custom-set-variables '(coffee-tab-width 2))
  )
;; generating sourcemap by '-m' option. And you must set '--no-header' option
(setq coffee-args-compile '("-c" "--no-header" "-m"))
(add-hook 'coffee-after-compile-hook 'sourcemap-goto-corresponding-point)

;; If you want to remove sourcemap file after jumping corresponding point
;; (defun my/coffee-after-compile-hook (props)
;;   (sourcemap-goto-corresponding-point props)
;;   (delete-file (plist-get props :sourcemap)))
;; (add-hook 'coffee-after-compile-hook 'my/coffee-after-compile-hook)

;; set up yasnippets
(use-package yasnippet
  :ensure t
  :pin melpa)
(require 'yasnippet)
(yas-global-mode 1)
(add-to-list 'yas-snippet-dirs (mb/emacs-subdirectory "snippets"))

;; set up ag (grep replacement)
(use-package ag
  :init      (setq ag-highlight-search t))
;;  :config    (add-to-list 'ag-arguments "--word-regexp"))

(setenv "PATH"  "/Users/***REMOVED***/.nvm/versions/node/v4.4.6/bin:/Users/***REMOVED***/.rbenv/shims:/opt/android-sdk-linux/tools:/opt/android-sdk-linux/platform-tools:/opt/gradle-2.2/bin:/usr/local/bin:/Users/***REMOVED***/.rbenv/shims:/opt/android-sdk-linux/tools:/opt/android-sdk-linux/platform-tools:/opt/gradle-2.2/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin")
(setq exec-path (split-string "/Users/***REMOVED***/.nvm/versions/node/v4.4.6/bin:/Users/***REMOVED***/.rbenv/shims:/opt/android-sdk-linux/tools:/opt/android-sdk-linux/platform-tools:/opt/gradle-2.2/bin:/usr/local/bin:/Users/***REMOVED***/.rbenv/shims:/opt/android-sdk-linux/tools:/opt/android-sdk-linux/platform-tools:/opt/gradle-2.2/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin" path-separator))

(use-package ace-window
  :ensure t
  :init
    (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l ?o))
    (global-set-key (kbd "C-x o") 'ace-window)
  :diminish ace-window-mode)

(use-package expand-region
  :ensure t
  :pin melpa)
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(setq paradox-github-token "06d54f23fdfcda346251c9fb692a9881d1f77586")

(use-package evil  :ensure t  :pin melpa)
(use-package evil-leader :ensure t  :pin melpa)

(global-set-key (kbd "C-`") 'evil-mode)

(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key-for-mode 'ruby-mode
                              "rf" 'rspec-verify
                              "ra" 'rspec-verify-all
                              "rf" 'rspec-run-last-failed
                              "rs" 'rspec-verify-single)

(evil-leader/set-key
  "pf" 'projectile-find-file
  "pg" 'projectile-ag
  "hs" 'split-window-vertically
  "vs" 'split-window-horizontally
  "zc" 'hs-hide-block
  "zo" 'hs-show-block
  "zlc" 'hs-hide-level
  "zlo" 'hs-show-all)

(pdf-tools-install)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
