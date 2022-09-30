;; --- TODOs --- 
;; TODO Compilation buffer changes
;; FIXME Issues with random pausing
;; FIXME Issues with font not loading

;; --- OPTIMIZATION --- 
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024));; 1mb

;; --- VARIABLES ---

(when (string= "darwin" system-type)
 (defvar config-file "~/.emacs.d/init.el"))

(when (string= "gnu/linux" system-type)
 (defvar config-file "~/.config/.emacs.d/init.el"))

(make-variable-buffer-local 'my-compilation-start-time)

;; --- FUNCTIONS ---
(defun goto-org ()
  "Open dired at the value of org-directory"
  (interactive)
  (dired org-directory))

(defun goto-config ()
  "Goto configuration file for Emacs"
  (interactive)
  (find-file config-file))

(defun my-org-hook ()
  (turn-on-auto-fill)
  (org-superstar-mode))

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  "Get colored output in compilation buffer"
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))

;; Get elapsed time for compilation in buffer
(defun my-compilation-start-hook (proc) 
  (setq my-compilation-start-time (current-time)))

(defun my-compilation-finish-function (buf why)
  (let* ((elapsed  (time-subtract nil my-compilation-start-time))
         (msg (format "Elapsed: %s" (format-time-string "%T.%N" elapsed t))))
    (save-excursion (goto-char (point-max)) (insert msg))
    (message "Compilation %s: %s" (string-trim-right why) msg)))

;; -- HOOKS -- 
(add-hook 'compilation-start-hook #'my-compilation-start-hook)
(add-hook 'compilation-finish-functions #'my-compilation-finish-function)
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
(add-hook 'org-mode-hook 'my-org-hook)

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
(setq use-package-always-ensure t)

;; -- CONFIG --

;; Vim emulation
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
  (global-evil-surround-mode))

(use-package org-evil
  :hook 
  (org-mode . org-evil-mode))

;; Used for leader keybindings
(use-package general
  :config
  (general-create-definer my-leader-def :prefix "SPC")
  (general-create-definer my-local-leader-def
			  :prefix "SPC m")
  (my-leader-def 'normal
		 "SPC" 'counsel-M-x
		 ":" 'eval-expression
		 ";" 'shell-command
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
		 "pc" 'projectile-compile-project
		 "pd" 'projectile-dired
		 "pi" 'projectile-invalidate-cache
		 "pt" 'projectile-run-term
		 ;; BUFFER
		 "x" '(switch-to-buffer "*scratch*")
		 "bb" 'counsel-switch-buffer
		 "bd" 'evil-delete-buffer
		 "bn" 'evil-next-buffer
		 "bp" 'evil-prev-buffer
		 "bm" 'ibuffer
		 ;; DIRED
		 "d" 'dired
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

;; - VC -
(use-package magit)

;; - COMPLETION - 
(use-package company
  :hook (prog-mode . company-mode))

(use-package smartparens
  :config
  (smartparens-global-mode))

;; - CODE -

;; Error checking
(use-package flycheck :ensure t
  :ensure t
  :hook
  (prog-mode . flycheck-mode))

;; LSP -> Language Server Protocol (Code completion, error checking, etc...)
(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (c-mode . lsp)
         (c++-mode . lsp)
	 (rust-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui :ensure t
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable nil))

(use-package lsp-ivy :ensure t :commands lsp-ivy-workspace-symbol)

;; Python
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

;; Rust
(use-package rust-mode)

;; Haskell
(use-package haskell-mode)

;; Common Lisp
(use-package sly)

;; Better syntax highlighting
(use-package tree-sitter :ensure t
  :hook
  (python-mode . tree-sitter-hl-mode)
  (c-mode . tree-sitter-hl-mode))

(use-package tree-sitter-langs :ensure t)

;; - MISC -
(use-package projectile)

(use-package which-key
  :config
  (which-key-mode))

(use-package smooth-scrolling
  :config (smooth-scrolling-mode))

;; - APPEARANCE -
(use-package powerline
  :config
  (setq powerline-gui-use-vcs-glyph t)
  (powerline-center-theme))

(use-package gruber-darker-theme
  :config
  (load-theme 'gruber-darker t))

(use-package org-superstar)

(use-package hl-todo
  :config
  (setq hl-todo-keyword-faces
	'(("TODO" . "#7669d6")
	  ("FIXME". "#ff4d67")))
  (global-hl-todo-mode))

(use-package highlight-numbers
  :config
  (highlight-numbers-mode))


;; ICONS
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

;; --- SETTINGS ---

;; No startup screen
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;;; Set cmd key to meta
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta)

(tool-bar-mode -1)

(menu-bar-mode -1)

(scroll-bar-mode -1)

;; Get relative line numbers in prog-mode
(add-hook 'prog-mode-hook 'menu-bar--display-line-numbers-mode-relative)

;; Font
(set-face-attribute 'default nil :font "Hack"
		    :height 130)

;; Set escape to C-g
(global-set-key (kbd "<escape>") (kbd "C-g"))

;; Increase/Decrease font scale
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Much better scrolling
;; (pixel-scroll-precision-mode)


;; -- ORG -- 

;; Org agenda
(setq org-directory "~/Dropbox/org/")
(setq org-agenda-files (list org-directory))

(setq org-todo-keyword-faces
  '(("TODO" . (:foreground "gold" :weight bold))))

;; --- END ---
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("b02eae4d22362a941751f690032ea30c7c78d8ca8a1212fdae9eecad28a3587f" "2ff9ac386eac4dffd77a33e93b0c8236bb376c5a5df62e36d4bfa821d56e4e20" default))
 '(org-agenda-files
   '("~/Dropbox/org/COSC3503.org" "/Users/marcusdefreitas/Dropbox/org/ENGL3213.org" "/Users/marcusdefreitas/Dropbox/org/COSC4303.org" "/Users/marcusdefreitas/Dropbox/org/THEO3133.org"))
 '(package-selected-packages
   '(org-superstar leetcode monokai-pro-theme gnuplot gnuplot-mode which-key use-package smartparens sly projectile powerline magit gruvbox-theme general evil-surround evil-commentary evil-collection counsel company))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
