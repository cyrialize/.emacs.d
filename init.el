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
	format-all
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
	hl-todo
	web-mode
	zenburn-theme
	ef-themes
	markdown-mode
	lua-mode
	tempel
	ws-butler

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
    (cond
     ((eq major-mode 'org-mode)
      (setq-local whitespace-style '(face empty tabs trailing)))
     ((eq major-mode 'markdown-mode)
      (setq-local whitespace-style '(face empty tabs trailing)))
     (t (setq-local whitespace-style '(face empty tabs lines-tail trailing))))
    (whitespace-mode)))

(add-hook 'find-file-hook 'cyr-load-whitespace-style)

(defun cyr-favorite-commands ()
  "A list of my favorite commands tied to this function for quick reference."
  (interactive)
  (call-interactively
   (intern (completing-read "Choose one: " '(consult-bookmark
					     consult-flymake
					     consult-goto-line
					     consult-outline
					     consult-imenu
					     comment-or-uncomment-region
					     treemacs
					     whitespace-cleanup
					     highlight-phrase)))))

(global-set-key (kbd "C-x c f") 'cyr-favorite-commands)

(defun cyr-org-new-log-heading ()
  "Insert a new heading for my preferred log formatting.

For example:
* 2025
** January
*** Monday 01/01/25

This should be called on the current heading type you want, because we're using
`org-insert-heading-after-current. For example, if I want to insert 2026, I'd
call this function on '* 2025'"
  (interactive)
  (org-insert-heading-after-current)
  (progn
    (let ((heading (completing-read "Choose heading type: "
				    '("day" "month" "year")
				    nil
				    t)))
      (cond
       ((string= "day" heading) (cyr-org-log-day-text))
       ((string= "month" heading) (insert (format-time-string "%B")))
       ((string= "year" heading) (insert (format-time-string "%Y")))))))

(defun cyr-org-log-day-text ()
  "Insert text that follows my preferred day format."
  (interactive)
  (insert (format "%s %s"
		  (format-time-string "%A")
		  (format-time-string "%m/%d/%y"))))

(defun cyr-ticket-file-name-from-string (ticket-title)
  "Remove invalid characters and correct spacing for TICKET-TITLE."
  (let ((valid-char-only-title (replace-regexp-in-string
				"[^a-zA-Z0-9 \\-]"
				""
				ticket-title)))
    (let ((space-correct-title (replace-regexp-in-string
				"\s+"
				"-"
				(string-trim valid-char-only-title))))
      space-correct-title)))

(defun cyr-new-ticket ()
  "Create a new org file for a ticket."
  (interactive)
  (if (and private-cyr-new-ticket-dir
           (file-exists-p (expand-file-name private-cyr-new-ticket-dir)))
      (progn
        (let ((ticket-title (read-string "Enter ticket name: ")))
          (let ((ticket-file-name
                 (cyr-ticket-file-name-from-string ticket-title)))
            (with-temp-buffer
              (insert ticket-file-name)
	      (insert "\n\n* ")
	      (cyr-org-log-day-text)
              (write-region
               (point-min)
               (point-max)
               (concat
                (expand-file-name private-cyr-new-ticket-dir)
		ticket-file-name
                ".org"))))))
    (message "Ticket dir not defined or found!")))

(defun cyr-log-inserts ()
  "Select a file for a `completing-read` menu of contents."
  (interactive)
  (let ((chosen-file (completing-read
		      "Choose one: "
		      (directory-files
		       (expand-file-name "~/.emacs.d/logs/")
		       nil
		       "\\`[^.]")
		      nil
		      t)))
    (progn
      (let ((log (completing-read
		  "Choose one: "
		  (cyr-list-from-file
		   (expand-file-name chosen-file "~/.emacs.d/logs/"))
		  nil
		  t)))
	(insert log)))))

(defun cyr-list-from-file (file)
  "Given a FILE return a list of the contents."
  (if (file-exists-p (expand-file-name file))
      (progn
	(with-temp-buffer
	  (insert-file-contents file)
	  (split-string (buffer-string) "\n" t)))
    (progn
      (message (format "File %s does not exist!" file))
      nil)))

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
   (after-init . column-number-mode)
   (after-init . ws-butler-global-mode)
   (prog-mode . display-line-numbers-mode)
   (prog-mode . display-fill-column-indicator-mode)))

(use-package flymake
  :hook
  (prog-mode))

(use-package org-mode
  :custom
  (org-startup-folded t)
  (org-startup-truncated nil)
  (org-refile-use-outline-path 'file)
  (org-refile-targets '((private-org-refile-targets :maxlevel . 1)))
  (org-agenda-files private-org-agenda-files)

  ;; By default this is "B", which can make org-sort confusing as explicitly
  ;; marked tasks with [#B] will not be moved. [#A] is 65, and the default
  ;; lowest, [#C], is 67. Making the default 68 means that tasks without
  ;; explicit priority will always be below those that have explicit priority.
  (org-priority-default 68)

  :hook
  (org-mode . jinx-mode))

(use-package whitespace)

(use-package xref)

(use-package eglot)

(use-package delsel
  :hook
  (after-init . delete-selection-mode))

(use-package desktop
  :custom
  (desktop-path '("~/.emacs.d/.cache/")))

;;; Installed Packages

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

;; https://github.com/minad/tempel
(use-package tempel
  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))

  :init
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  :hook
  (conf-mode . tempel-setup-capf)
  (prog-mode . tempel-setup-capf)
  (text-mode . tempel-setup-capf)
  (after-init . global-tempel-abbrev-mode))

;; https://github.com/abo-abo/hydra
(use-package hydra
  :ensure t)

(defun cyr-org-tasks ()
  "Load tasks desktop file and call hydra-org."
  (interactive)
  (if (file-exists-p (expand-file-name
		      (format "%s/.emacs.desktop" private-tasks-desktop-path)))
      (progn
	(delete-other-windows)
	(desktop-change-dir (expand-file-name private-tasks-desktop-path))
	(desktop-read)
	(hydra-org/body))
    (message "Desktop file in %s does not exist!" private-tasks-desktop-path)))

(defun hydra-org-insert-task ()
  "Prompt and insert task, used for hydra-org."
  (interactive)
  (let ((new-task (read-string "Enter new task: ")))
    (end-of-line)
    (newline)
    (insert (format "* TODO %s" (string-trim new-task)))
    (org-indent-line)))

(defun hydra-org-sort ()
  "Select entire buffer and call org-sort, used for hydra-org."
  (interactive)
  (progn
    (mark-whole-buffer)
    (org-sort)))

(defhydra hydra-org (:hint nil
		     :pre (setq which-key-inhibit t)
                     :post (setq which-key-inhibit nil))
  "
  Movement:             Org:
  _n_: next-line        _i_: hydra-org-insert-task    _a_: hydra-org-sort
  _p_: previous-line    _r_: org-refile
  _k_: kill-whole-line  _t_: org-todo
  _o_: other-window     _s_: org-save-all-org-buffers
  _u_: undo             _t_: org-priority
  "

  ("n" next-line)
  ("p" previous-line)
  ("k" kill-whole-line)
  ("o" other-window)
  ("u" undo)

  ("i" hydra-org-insert-task)
  ("r" org-refile)
  ("t" org-todo)
  ("s" org-save-all-org-buffers)
  ("t" org-priority)
  ("a" hydra-org-sort)

  ("q" nil "quit"))

(global-set-key (kbd "C-x c o") 'hydra-org/body)

(defhydra hydra-org-log (:hint nil
			 :pre (setq which-key-inhibit t)
			 :post (setq which-key-inhibit nil))
  "
  _n_: org-next-visible-heading
  _p_: org-previous-visible-heading
  _e_: end-of-buffer
  _b_: beginning-of-buffer
  _l_: cyr-org-new-log-heading
  "

  ("n" org-next-visible-heading)
  ("p" org-previous-visible-heading)
  ("e" end-of-buffer)
  ("b" beginning-of-buffer)
  ("l" cyr-org-new-log-heading)

  ("q" nil "quit"))

(global-set-key (kbd "C-x c l") 'hydra-org-log/body)

(defhydra hydra-spelling (:hint nil
			  :pre (setq which-key-inhibit t)
			  :post (setq which-key-inhibit nil))
  "
  _n_: jinx-next
  _p_: jinx-previous
  _c_: jinx-correct
  "

  ("n" jinx-next)
  ("p" jinx-previous)
  ("c" jinx-correct)

  ("q" nil "quit"))

(global-set-key (kbd "C-x c s") 'hydra-spelling/body)

(defhydra hydra-errors (:hint nil
		        :pre (setq which-key-inhibit t)
			:post (setq which-key-inhibit nil))

  "
  _c_: consult-flymake
  _n_: flymake-goto-next-error
  _p_: flymake-goto-prev-error
  "
  ("c" consult-flymake)
  ("n" flymake-goto-next-error)
  ("p" flymake-goto-prev-error)

  ("q" nil "quit"))

(global-set-key (kbd "C-x c e") 'hydra-errors/body)

;; https://github.com/emacsorphanage/pkg-info/tree/master
(use-package pkg-info
  :ensure t)

;; https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t)

;; https://github.com/lassik/emacs-format-all-the-code
(use-package format-all
  :ensure t
  :commands format-all-mode
  :config
  ;; Prevents the errors buffer from popping up
  (add-to-list 'display-buffer-alist
	       '("*format-all-errors*"
		 (display-buffer-no-window)
		 (allow-no-window . t))))

;; Prior to using, be sure to update your local version of tidy
;; See: https://www.html-tidy.org/
;;
;; I use prettier on a case by case basis via .dir-locals.el.
;; Ex: ((nil . ((format-all-formatters . (("HTML" (prettier)))))))
;;
;; It's best to use prettier if you have HTML with a templating language.
(eval-after-load 'format-all
  '(add-hook 'web-mode-hook
             (lambda ()
               (setq format-all-formatters
                     '(("HTML" (html-tidy
				"--gnu-emacs"
				"yes"
				"--indent"
				"yes"
				"--indent-spaces"
				"2"
				"--indent-attributes"
				"yes"
				"--wrap"
				"80"
				"--vertical-space"
				"yes"
				"--break-before-br"
				"yes"
				"--tidy-mark"
				"no"
				"-q"
				)))))))

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
  :ensure t
  :init
  (cyr-load-private))

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

;; https://github.com/Alexander-Miller/treemacs
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (treemacs-follow-mode -1)
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
  :disabled

  :init
  (load-theme 'darcula t))

;; https://github.com/bbatsov/zenburn-emacs
(use-package zenburn-theme
  :ensure t
  :disabled

  :init
  (load-theme 'zenburn t))

;; https://github.com/protesilaos/ef-themes
(use-package ef-themes
  :ensure t

  :init
  (load-theme 'ef-dream t))

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

  :custom
  (embark-prompter 'embark-completing-read-prompter)
  (embark-indicators '(embark-minimal-indicator
		       embark-highlight-indicator
		       embark-isearch-highlight-indicator))

  :config
  ;; Custom embark keymaps
  (defvar-keymap cyr-embark-code-map
    :doc "Commands always used while programming"
    "c" #'comment-or-uncomment-region
    "w" #'whitespace-cleanup-region
    "d" #'delete-region
    "j" #'json-pretty-print
    "e" #'elisp-eval-region-or-buffer)

  (add-to-list 'embark-keymap-alist '(region . cyr-embark-code-map))

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

;; https://github.com/tarsius/hl-todo
(use-package hl-todo
  :ensure t
  :hook (prog-mode))

;; https://web-mode.org/
(use-package web-mode
  :ensure t

  :custom
  (web-mode-code-indent-offset 2)
  (web-mode-attr-indent-offset 2)
  (web-mode-attr-value-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-markup-comment-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-enable-current-element-highlight t)

  :mode
  (("\\.phtml\\'" . web-mode)
   ("\\.html\\'" . web-mode)
   ("\\.css\\'" . web-mode)
   ("\\.php\\'" . web-mode)
   ("\\.tpl\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode)))

;; https://github.com/jrblevin/markdown-mode
(use-package markdown-mode
  :ensure t)

;; https://github.com/immerrr/lua-mode
(use-package lua-mode
  :ensure t)

(provide 'init)
;;; init.el ends here
