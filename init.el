;;; config -- Summary -*- lexical-binding: t; -*-

;;; Commentary:
;; I highly recommend going through the readme.org if you'd like to know more
;; about my configuration.
;;
;; My config uses `use-package` heavily, you can traverse through sections of
;; the config by jumping through various outlines (e.g. `consult-outline`).
;;
;; This config is organized in three broad sections:
;; - Package & Initial Set Up
;;   - Setup for `package.el`, installing packages, and any code I want loaded
;;     before everything else.
;;
;; - Built-In Packages
;;   - `use-package` declarations for built-in packages
;;
;; - Installed Packages
;;   - `use-package` declarations for installed packages

;;; Code:

;;; Package & Initial Set Up

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq package-selected-packages
      '(
	use-package
	org-mode
	corfu
	minions
	whitespace-mode
	whitespace-cleanup-mode
	smart-mode-line
	dashboard
	god-mode
	vertico
	orderless
	savehist
	darcula-theme
	restart-emacs
	flymake
	marginalia
	embark
	embark-consult
	consult
	consult-dir
	which-key
	magit
	rg
	xref
	chruby
	nerd-icons
	nerd-icons-completion
	nerd-icons-corfu
	nerd-icons-dired

	;; Required for magit
	dash
	transient
	with-editor
	))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(package-initialize)

(eval-when-compile
  (require 'use-package))

(when (file-exists-p "~/.emacs.d/private.el")
    (load "~/.emacs.d/private.el"))

(defun cyr-reload-packages ()
  "Reload packages by calling applicable package.el commands."
  (interactive)
  (package-refresh-contents)
  (package-install-selected-packages)
  (package-autoremove))

(defun cyr-load-custom ()
  "Set the custom var and load the custom file."
  (setq custom-file "~/.emacs.d/custom.el")
  (load custom-file))

;;; Built-In Packages

(use-package emacs
  :custom
  (scroll-bar-mode nil)
  (tool-bar-mode nil)

  (mode-require-final-newline 'visit-save)
  (require-final-newline 'visit-save)

  (tooltip-mode nil)
  (tooltip-use-echo-area t)
  (tooltip-resize-echo-area t)

  (display-fill-column-indicator t)
  (display-fill-column-indicator-column 80)

  (sentence-end-double-space nil)

  (whitespace-style '(face empty tabs lines-tail trailing))

  :init
  ;; Ask y or n instead of yes or no
  (fset 'yes-or-no-p 'y-or-n-p)

  :hook
  ((emacs-startup . cyr-load-custom)
   (after-init . global-hl-line-mode)
   (after-init . global-whitespace-mode)
   (after-init . nerd-icons-completion-mode)
   (prog-mode . display-line-numbers-mode)
   (prog-mode . display-fill-column-indicator-mode)))

(use-package flymake
  :hook
  (prog-mode))

(use-package flyspell
  :hook
  (prog-mode . flyspell-prog-mode))

(use-package org-mode
  :custom
  ((org-startup-folded t)

   (org-refile-use-outline-path 'file)
   (org-refile-targets '((private-org-refile-targets :maxlevel . 1))))

  :hook
  (org-mode . flyspell-mode))

(use-package whitespace)

(use-package xref)

(use-package eglot)

(use-package delsel
  :hook
  (after-init . delete-selection-mode))

;;; Installed Packages

;; https://github.com/minad/corfu
(use-package corfu
  :ensure t

  :custom
  (corfu-auto t)

  :hook
  (after-init . global-corfu-mode))

;; https://github.com/tarsius/minions
(use-package minions
  :ensure t

  :hook (after-init))

;; https://github.com/purcell/whitespace-cleanup-mode
(use-package whitespace-cleanup-mode
  :ensure t

  :hook
  (after-init . global-whitespace-cleanup-mode))

;; https://github.com/Malabarba/smart-mode-line
(use-package smart-mode-line
  :ensure t
  :init
  (setq sml/no-confirm-load-theme t)
  (sml/setup))

;; https://github.com/emacs-dashboard/emacs-dashboard
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

;; https://github.com/emacsorphanage/god-mode
(use-package god-mode
  :ensure t

  :custom
  ;; Ensure that no buffer is skipped, so god-mode is set everywhere
  (god-exempt-major-modes nil)
  (god-exempt-predicates nil)


  :config
  ;; Change the visual of the cursor when god-mode is on/off
  ;; When god-mode is off, sets it to a bar
  ;; When god mode is on, sets it to a box
  (defun my-god-mode-update-cursor-type ()
    (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))
  :hook ((post-command . my-god-mode-update-cursor-type)
	 (after-init . god-mode))

  ;; Toggle god-mode using the escape key
  :bind (("<escape>" . god-local-mode)
	 ("<escape>" . god-mode-all)
	 ("C-x C-1" . delete-other-windows)
	 ("C-x C-2" . split-window-below)
	 ("C-x C-3" . split-window-right)
	 ("C-x C-0" . delete-window)
	 ;; Turn off god-mode locally with the i key (like Vim/Evil)
	 :map god-local-mode-map
	 ("i" . god-local-mode)))

;; https://github.com/minad/vertico
(use-package vertico
  :ensure t

  :custom
  (vertico-scroll-margin 0)
  (vertico-count 20)
  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (vertico-cycle t)

  :hook
  (after-init)

  :defines
  (vertico-cycle))

;; https://github.com/oantolin/orderless
(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Persist history over Emacs restarts. Vertico sorts by history position.
;; https://github.com/emacs-mirror/emacs/blob/master/lisp/savehist.el
(use-package savehist
  :ensure t
  :config
  (savehist-mode))

;; https://gitlab.com/fommil/emacs-darcula-theme
(use-package darcula-theme
  :ensure t
  :config
  (set-frame-font "Jetbrains Mono-14")

  :init
  (load-theme 'darcula t))

;; https://github.com/iqbalansari/restart-emacs
(use-package restart-emacs
  :ensure t)

;; https://github.com/minad/marginalia
(use-package marginalia
  :ensure t

  :hook (after-init)
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle)))

;; https://github.com/oantolin/embark
(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; https://github.com/minad/consult
(use-package consult
  :ensure t
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x t b" . consult-buffer-other-tab)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
	 ;; Alternative: consult-org-heading
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
	 ;; needed by consult-line to detect isearch
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))

  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config

  ;; https://github.com/minad/consult?tab=readme-ov-file#live-previews
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any)))

(use-package embark-consult
  :ensure t)

(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :ensure t
  :hook (after-init))

(use-package dash
  :ensure t)

(use-package transient
  :ensure t)

(use-package with-editor
  :ensure t)

;; The above packages are required for magit
;; https://magit.vc/
(use-package magit
  :ensure t)

;; https://github.com/dajva/rg.el
(use-package rg
  :ensure t
  :config
  (rg-enable-default-bindings))

;; https://github.com/plexus/chruby.el
(use-package chruby
  :ensure t
  :hook
  ((ruby-mode . chruby-use-corresponding)
   (ruby-ts-mode . chruby-use-corresponding)))

;; https://github.com/rainstormstudio/nerd-icons.el
(use-package nerd-icons
  :ensure t)

;; https://github.com/rainstormstudio/nerd-icons-completion
(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :hook
  (marginalia-mode-hook . nerd-icons-completion-marginalia-setup))

;; https://github.com/LuigiPiucco/nerd-icons-corfu
(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :hook
  (corfu-margin-formatters . nerd-icons-corfu-formatter))

;; https://github.com/rainstormstudio/nerd-icons-dired
(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))


(provide 'init)
;;; init.el ends here
