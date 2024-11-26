;;; custom -- Summary  -*- lexical-binding: t; -*-

;;; Commentary:
;; My file for customizations from `customize`.
;;
;; I mostly keep all configurations in init.el, with the exception of some -
;; namely themes and fonts. I find it much easier to set those within customize
;; instead.

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("3f1dcd824a683e0ab194b3a1daac18a923eed4dba5269eecb050c718ab4d5a26" "45631691477ddee3df12013e718689dafa607771e7fd37ebc6c6eb9529a8ede5" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Hack" :foundry "SRC" :slant normal :weight normal :height 128 :width normal))))
 '(bold ((t (:foreground "yellow green" :weight bold))))
 '(org-code ((t (:foreground "deep sky blue" :box (:line-width (1 . 1) :color "dark gray") :family "hack"))))
 '(org-level-1 ((t (:inherit default :extend nil :foreground "#DFAF8F" :weight bold :height 1.25))))
 '(org-level-2 ((t (:inherit default :extend nil :foreground "#BFEBBF" :weight bold :height 1.2))))
 '(org-level-3 ((t (:inherit default :extend nil :foreground "#7CB8BB" :weight bold :height 1.15))))
 '(org-level-4 ((t (:inherit default :extend nil :foreground "#D0BF8F" :weight bold :height 1.1))))
 '(org-level-5 ((t (:inherit default :extend nil :foreground "#93E0E3" :weight bold :height 1.07))))
 '(org-level-6 ((t (:inherit default :extend nil :foreground "#9FC59F" :weight bold :height 1.05))))
 '(org-level-7 ((t (:inherit default :extend nil :foreground "#8C5353" :weight bold :height 1.03))))
 '(org-level-8 ((t (:inherit default :extend nil :foreground "#4C7073" :weight bold :height 1.01))))
 '(org-list-dt ((t (:foreground "gold" :weight bold))))
 '(org-todo ((t (:background "white" :foreground "red" :weight bold :height 1.0 :width normal))))
 '(org-verbatim ((t (:foreground "deep sky blue" :box (:line-width (1 . 1) :color "light gray") :family "hack"))))
 '(tooltip ((t (:background "gray30" :foreground "DarkOrange3" :slant normal :height 2.0 :width extra-expanded)))))

(provide 'custom)
;;; custom.el ends here
