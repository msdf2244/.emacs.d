;; --- TODOs --- 

;; TODO make portable
;; TODO Transfer stuff from old config
;; TODO LSP
;; FIXME Issues with random pausing (GC?)

;; --- OPTIMIZATION --- 
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024));; 1mb

;; --- VARIABLES ---
(defvar config-file "~/.emacs.d/init.el")

;; --- FUNCTIONS ---
(defun goto-org ()
  "Open dired at the value of org-directory"
  (interactive)
  (dired org-directory))

(defun goto-config ()
  "Goto configuration file for Emacs"
  (interactive)
  (find-file config-file))

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  "Get colored output in compilation buffer"
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))

;; --- PACKAGES ---

;; -- SETUP -- 
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

;; -- DECLARATIONS --

;; - VIM EMULATION -
(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-set-undo-system 'undo-redo)
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

(use-package org-evil
  :hook 
  (org-mode . org-evil-mode))

;; Used for leader keys
(use-package general
  :config
  (general-create-definer my-leader-def :prefix "SPC")
  (general-create-definer my-local-leader-def
			  :prefix "SPC m")
  (my-leader-def 'normal
		 "SPC" 'counsel-M-x
		 ;; SEARCH
		 "/" 'swiper
		 "?" 'swiper-backward
		 "*" 'swiper-thing-at-point
		 ;; ORG
		 "oa" 'org-agenda
		 "oh" 'goto-org
		 ;; FIND
		 "ff" 'find-file
		 "fr" 'counsel-recentf
		 "fp" 'goto-config
		 ;; CODE
		 "cr" 'lsp-rename
		 "ct" '(lambda () (interactive)
			 (swiper "TODO\\|FIXME"))
		 ;; PACKAGE
		 "ui" 'package-install
		 "ud" 'package-delete
		 "ul" 'package-list-packages
		 ;; PROJECT
		 "pp" 'projectile-switch-project
		 "pf" 'projectile-find-file
		 "pc" 'projectile-cleanup-known-projects
		 "pd" 'projectile-discover-projects-in-directory
		 "pi" 'projectile-invalidate-cache
		 "pt" 'projectile-run-term
		 ;; BUFFER
		 "bb" 'counsel-switch-buffer
		 "bd" 'evil-delete-buffer
		 "bn" 'evil-next-buffer
		 "bp" 'evil-prev-buffer
		 "bm" 'ibuffer
		 ;; WINDOWS
		 "wj" 'evil-window-down
		 "wk" 'evil-window-up
		 "wl" 'evil-window-right
		 "wh" 'evil-window-left
		 "wo" 'delete-other-windows
		 "cc" 'compile
		 ;; HELP
		 "hv" 'counsel-describe-variable
		 "hP" 'describe-package
		 "hk" 'describe-key
		 "hb" 'counsel-descbinds
		 "hf" 'counsel-describe-function
		 "hm" 'describe-mode
		 )
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
    "eb" 'sly-eval-buffer)
  (my-local-leader-def :states 'normal
    :keymaps 'org-mode-map
    "d" 'org-deadline
    "s" 'org-schedule))

;; - SEARCH - 
(use-package ivy
  :config
  (ivy-mode))

(use-package ivy-rich
  :after ivy
  :config
  (ivy-rich-mode))

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

;; - CODE -

;; Lisp IDE
(use-package flycheck :ensure t
  :ensure t
  :hook
  (prog-mode . flycheck-mode))

(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (c-mode . lsp)
         (c++-mode . lsp)
	 (rust-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui :ensure t
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable nil))

;; if you are ivy user
(use-package lsp-ivy :ensure t :commands lsp-ivy-workspace-symbol)

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

(use-package sly)

(use-package tree-sitter :ensure t
  :hook
  (python-mode . tree-sitter-hl-mode)
  (c-mode . tree-sitter-hl-mode)
  (lisp-mode . treee-sitter-hl-mode))

(use-package tree-sitter-langs :ensure t)

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
  :config
  (load-theme 'gruvbox-dark-soft t))

(use-package hl-todo
  :config
  (global-hl-todo-mode))

(use-package highlight-numbers
  :config
  (highlight-numbers-mode))

(use-package smooth-scrolling
  :config (smooth-scrolling-mode))

;; ALL THE ICONS
(use-package all-the-icons-dired
  :config
  (all-the-icons-dired-mode))

(use-package all-the-icons-ivy-rich
  :after ivy
  :config
  (all-the-icons-ivy-rich-mode))

(use-package all-the-icons-ivy
  :after ivy
  :config)

(use-package all-the-icons-completion
  :config
  (all-the-icons-completion-mode))

(use-package company-box
  :after company
  :hook (company-mode . company-box-mode))

;; --- SETTINGS ---

;; No startup
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

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
		    :height 160)

;; Set escape to C-g
(global-set-key (kbd "<escape>") (kbd "C-g"))

;; Increase/Decrease font scale
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)


;; Properly colored output in compilation buffers
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; -- ORG --
;; Auto fill paragraph
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;; Org agenda
(setq org-directory "~/Dropbox/org/")
(setq org-agenda-files (list org-directory))

;; --- END ---
