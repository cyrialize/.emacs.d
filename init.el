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
	toc-org
	pkg-info
	exec-path-from-shell
	jinx
	helm
	eldoc
	eldoc-box
	corfu
	cape
	minions
	whitespace-mode
	whitespace-cleanup-mode
	smart-mode-line
	dashboard
	god-mode
	treemacs
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
	treesit-auto
	xref
	chruby
	nerd-icons
	nerd-icons-completion
	nerd-icons-corfu
	nerd-icons-dired
	inf-ruby
	yard-mode
	yasnippet
	yasnippet-snippets

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

(defun cyr-exec-path-from-shell ()
  "Run `exec-path-from-shell-initialize` with extra messaging."
  (let ((exec-path-from-shell-installed (featurep 'exec-path-from-shell))
        (shell-system (memq window-system '(mac ns x))))
    (if (and exec-path-from-shell-installed shell-system)
        (progn
          (message "Initializing exec path from shell")
          (exec-path-from-shell-initialize))
      (message
       "Unable to init exec path from shell. Installed %s, Shell system %s"
       exec-path-from-shell-installed
       shell-system))))

(defun cyr-reload-packages ()
  "Reload packages by calling applicable package.el commands."
  (interactive)
  (progn
    (package-refresh-contents)
    (package-install-selected-packages)
    (package-autoremove)))

(defun cyr-load-private ()
  "Load the private.el file if it exists."
  (let ((private "~/.emacs.d/private.el"))
    (if (file-exists-p private)
	(load private)
      (message "Private file does not exist: %s" private))))

(defun cyr-load-custom (custom)
  "Set the CUSTOM variable and load the custom file."
  (if (file-exists-p custom)
      (progn
	(setq custom-file custom)
	(load custom-file))
    (message "Custom file does not exist: %s" custom)))

(defun cyr-load-whitespace-style ()
  "Set whitespace style locally before calling whitespace mode."
  (progn
    (if (eq major-mode 'org-mode)
	(setq-local whitespace-style '(face empty tabs trailing))
      (setq-local whitespace-style '(face empty tabs lines-tail trailing)))
    (whitespace-mode)))

(add-hook 'find-file-hook 'cyr-load-whitespace-style)

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

  (whitepsace-line-column 80)

  :init
  ;; Ask y or n instead of yes or no
  (fset 'yes-or-no-p 'y-or-n-p)

  :hook
  ((emacs-startup . (lambda () (cyr-load-custom "~/.emacs.d/custom.el")))
   (emacs-startup . cyr-load-private)
   (emacs-startup . cyr-exec-path-from-shell)
   (after-init . tab-bar-mode)
   (after-init . global-hl-line-mode)
   (after-init . nerd-icons-completion-mode)
   (prog-mode . display-line-numbers-mode)
   (prog-mode . display-fill-column-indicator-mode)))

(use-package flymake
  :hook
  (prog-mode))

(use-package org-mode
  :custom
  ((org-startup-folded t)
   (org-startup-truncated nil)
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

;; https://github.com/emacsorphanage/pkg-info/tree/master
(use-package pkg-info
  :ensure t)

;; https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t)

;; https://github.com/minad/jinx
(use-package jinx
  :ensure t
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

;; https://github.com/emacs-helm/helm
(use-package helm
  :disabled
  :ensure t

  :custom
  ((helm-M-x-fuzzy-match t)
   (helm-buffer-fuzzy-matching t)
   (helm-recentf-fuzzy-match t))

  :bind
  (("M-x" . helm-M-x)
   ("C-x r b" . helm-filtered-bookmarks)
   ("C-x C-f" . helm-find-files)
   ("C-x b" . helm-mini))

  :hook
  (after-init . helm-mode))

;; https://github.com/snosov1/toc-org
(use-package toc-org
  :ensure t)

;; https://elpa.gnu.org/packages/eldoc.html
(use-package eldoc
  :ensure t
  :hook
  (after-init . global-eldoc-mode))

;; https://github.com/casouri/eldoc-box
(use-package eldoc-box
  :ensure t)

;; https://github.com/minad/corfu
(use-package corfu
  :ensure t

  :custom
  (corfu-auto t)

  :hook
  (after-init . global-corfu-mode))

;; https://github.com/minad/cape
(use-package cape
  :ensure t

  :bind ("C-c p" . cape-prefix-map)

  :custom
  (cape-dabbrev-min-length 3)

  :config
  (setq-local completion-at-point-functions
              (list (cape-capf-super
		     #'cape-dabbrev
		     #'cape-keyword
		     #'cape-elisp-symbol
		     #'cape-dict))))

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

;; https://github.com/Alexander-Miller/treemacs
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-files-by-mouse-dragging    t
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-tab-bar
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

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

;; https://github.com/renzmann/treesit-auto
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; https://github.com/plexus/chruby.el
(use-package chruby
  :ensure t)

;; https://github.com/nonsequitur/inf-ruby
(use-package inf-ruby
  :ensure t)

;; https://github.com/pd/yard-mode.el
(use-package yard-mode
  :ensure t
  :hook (ruby-mode ruby-ts-mode))

;; https://github.com/joaotavora/yasnippet
(use-package yasnippet
  :ensure t
  :hook (after-init . yas-global-mode))

;; https://github.com/AndreaCrotti/yasnippet-snippets
(use-package yasnippet-snippets
  :ensure t)

(use-package ruby-mode
  :mode "\\.rb\\'"
  :interpreter "ruby"
  :hook (chruby-use-corresponding))

(use-package ruby-ts-mode
  :mode "\\.rb\\'"
  :interpreter "ruby"
  :hook (chruby-use-corresponding))

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
