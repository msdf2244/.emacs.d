(require 'package) ;; Emacs builtin

;; set package.el repositories
(setq package-archives
'(
   ("org" . "https://orgmode.org/elpa/")
   ("gnu" . "https://elpa.gnu.org/packages/")
   ("melpa" . "https://melpa.org/packages/")
))

;; initialize built-in package management
(package-initialize)

;; update packages list if we are on a new install
(unless package-archive-contents (package-refresh-contents))

;; a list of pkgs to programmatically install
;; ensure installed via package.el
(setq my-package-list '(use-package))

;; programmatically install/ensure installed
;; pkgs in your personal list
(dolist (package my-package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; -- Packages --

;; Vim Emulation
(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil ;; Important
  :config
  (evil-collection-init))

(use-package evil-commentary
  :config
  (evil-commentary-mode))

(use-package evil-surround
  :config
  (evil-surround-mode))

(use-package general
  :config
  (general-create-definer my-leader-def :prefix "SPC")
  (general-create-definer my-local-leader-def
			  :prefix "SPC m")
  (my-leader-def 'normal
		 "SPC" 'counsel-M-x
		 "ff" 'find-file
		 "fr" 'counsel-recentf
		 "pp" 'projectile-switch-project
		 "pf" 'projectile-find-file
		 "pc" 'projectile-cleanup-known-projects
		 "pd" 'projectile-discover-projects-in-directory
		 "pd" 'projectile-discover-projects-in-directory
		 "pi" 'projectile-invalidate-cache
		 "bb" 'counsel-switch-buffer
		 "bd" 'evil-delete-buffer
		 "bn" 'evil-next-buffer
		 "bp" 'evil-prev-buffer
		 "wj" 'evil-window-down
		 "wk" 'evil-window-up
		 "wl" 'evil-window-right
		 "wh" 'evil-window-left
		 "wo" 'delete-other-windows
		 "cc" 'compile
		 "hv" 'counsel-describe-variable
		 "hk" 'describe-key
		 "hb" 'counsel-descbinds
		 "hf" 'counsel-describe-function
		 "hm" 'describe-mode)
  (my-local-leader-def :states 'normal
    :keymaps 'emacs-lisp-mode-map
    "es" 'eval-last-sexp
    "ee" 'eval-expression
    "er" 'eval-region
    "ef" 'eval-defun
    "eb" 'eval-buffer)
  (my-local-leader-def :states 'normal
    :keymaps 'lisp-mode-map
    "'" 'sly-mrepl
    "es" 'sly-eval-last-expression
    "er" 'sly-eval-region
    "ef" 'sly-eval-defun
    "eb" 'sly-eval-buffer))

;; - SEARCH - 
(use-package ivy
  :config
  (ivy-mode))

(use-package counsel
  :config
  (counsel-mode))

(use-package swiper)

;; - GIT -
(use-package magit)

;; - COMPLETION - 
(use-package company
  :hook (prog-mode . company-mode))

(use-package smartparens
  :config
  (smartparens-global-mode))

;; - LANGS -

;; Lisp IDE
(use-package sly
  :hook (lisp-mode . (lambda () (sly "sbcl"))))

;; - UTILITY -
(use-package projectile)

;; - COSMETICS -
(use-package which-key
  :config
  (which-key-mode))

(use-package powerline
  :config
  (powerline-center-theme))

(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox-dark-soft t))

;; -- SETTINGS -- 

;;; Set cmd key to meta
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta)


;; Disable tool-bar
(tool-bar-mode -1)

;; Get relative line numbers in prog-mode
(add-hook 'prog-mode-hook 'menu-bar--display-line-numbers-mode-relative)

;; Font
(set-face-attribute 'default nil :font "UbuntuMonoDerivativePowerline Nerd Font"
		    :height 140)

;; Set escape to C-g
(global-set-key (kbd "<escape>") (kbd "C-g"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("2ff9ac386eac4dffd77a33e93b0c8236bb376c5a5df62e36d4bfa821d56e4e20" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
