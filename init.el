;; my emacs config - its a work in progress

(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

;; use use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;; auto package update
(use-package auto-package-update
  :ensure t)

;; set window size on open
(when window-system (set-frame-size (selected-frame) 135 35))

;; highlight current line
(global-hl-line-mode t)

;; set font
(set-default-font "Source Code Pro 11")

;; THEMES
;; gruvbox
(use-package gruvbox-theme
  :ensure t)
;; blackboard
(use-package blackboard-theme
  :ensure t)
;; dracula
(use-package dracula-theme
  :ensure t)
;; zenburn
(use-package zenburn-theme
  :ensure t)
;; moe
(use-package moe-theme
  :ensure t)
;; monokai
(use-package monokai-theme
  :ensure t)
;; solarized
(use-package solarized-theme
 :ensure t)

;; SELECT THEME TO LOAD
(load-theme 'gruvbox t)

;; use Evil mode
(use-package evil
  :ensure t)
(evil-mode t)

;; spaces by default instead of tabs!
(setq-default indent-tabs-mode nil)

;; show matching parenthesis
(setq show-paren-delay 0)
(show-paren-mode 1)

;; remove trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; better defaults!
(use-package better-defaults
  :ensure t)

;;;; ivy stuff
(use-package ivy
  :ensure t)

(use-package swiper
  :ensure t)

(use-package counsel
  :ensure t)

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key read-expression-map (kbd "C-r") 'counsel-expression-history)

;;;; helm stuff

;;(ido-mode t) ;; turn on ido for auto-complete
;; C-x C-b is ibuffer
;; C-s and C-r are swapped with regex-aware incremenetal search functionality

;; add helm - interactive buffer menus
;; (use-package helm
;;   :ensure t)
;; (global-set-key (kbd "M-x") 'helm-M-x)

;; add helm-ag - implements ag searching in helm
;; (use-package helm-ag
;;   :ensure t)
;; USAGE:
;; helm-ag:                    input search w/ ag command (use C-u to cd)
;; helm-ag-this-file:          same as helm-ag, search only current file
;; helm-do-ag:                 search with ag w/ grep
;; helm-do-ag-this-file:       same as do-ag, w/ this file
;; helm-ag-project-root:       search only root dir of this project
;; helm-ag-buffers:            search buffers by helm-ag
;; see file content temporarily by persistent action(C-j)
;; set helm-follow-mode-persistent to non-nil if you want to use helm-follow-mode by default. You must set it before loading helm-ag.el
;; (custom-set-variables
;; '(helm-follow-mode-persistent t))

;; auto-complete support
(use-package auto-complete
  :ensure t)
;; enable auto-complete at open
(global-auto-complete-mode t)

;; markdown support
(use-package markdown-mode ;; markdown support
  :ensure t)
;; markdown mode for md files
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; powerline (like VIM!!)
(use-package powerline ;; powerline (like VIM!!!)
  :ensure t)
(use-package airline-themes ;; vim airline themes for powerline
  :ensure t)

;; set airline theme for powerline
(load-theme 'airline-dark t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("b9cbfb43711effa2e0a7fbc99d5e7522d8d8c1c151a3194a4b176ec17c9a8215" "962dacd99e5a99801ca7257f25be7be0cebc333ad07be97efd6ff59755e6148f" default)))
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; evil mode for vim-style shortcuts
(use-package evil
  :ensure t)

;; show line numbers
(use-package nlinum
  :ensure t)
(global-linum-mode t)
;; (setq linum-format "%4d \u2502 ")
;; (setq linum-format "%d ")

;; auto matching brackets
(use-package autopair
  :ensure t)
(autopair-global-mode t)

;; show indent guides
(use-package indent-guide ;; use to show indent guides
  :ensure t)
(indent-guide-global-mode)
(setq indent-guide-recursive t)

;; minimap
;;(use-package sublimity
;;  :ensure t)
;;(sublimity-mode 1)
(use-package minimap
  :ensure t)

;; magit - for git
(use-package magit
  :ensure t)

;; which-key
(use-package which-key
  :ensure t)
(which-key-mode t)
(which-key-setup-minibuffer)

;; highlight instances of word when hovering
(use-package idle-highlight-mode
  :ensure t)
(idle-highlight-mode t)

;; ace jump mode
(use-package ace-jump-mode
  :ensure t)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode) ;; C-c SPC to enable

;; org mode stuff
;; pretty bullets for org mode
(use-package org-bullets
  :ensure t)
(add-hook 'org-mode-hook
          (lambda ()
            (org-bullets-mode t)))
;; syntax highlighting in source blocks when editing
(setq org-src-fontify-natively t)

;; log when TODO is completed in org mode
(setq org-log-done 'time)
