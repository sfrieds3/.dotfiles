;;;; package --- summary
;;;; Commentary:
;;;; my Emacs config - its a work in progress
;;;;
;;;; requirements:
;;;;   silversearcher - sudo apt install silversearcher-ag


;;; Code:
;;;; GENERAL PACKAGE SETTINGS
(require 'package)

(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;;(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

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

;;(use-package cl-lib
;;  :ensure t)

;; ////////////////////////////////////////////////////////////

;;;; STARTUP SETTINGS

;; inhibit startup screen
(setq inhibit-startup-screen t)

;; set window size on open
(when window-system (set-frame-size (selected-frame) 135 35))

;; start with split windows
;; Open split shell on launch
(add-hook 'emacs-startup-hook
  (lambda ()
    (split-window-horizontally)
    (split-window-vertically)))

;; highlight current line
(global-hl-line-mode t)

;; spaces by default instead of tabs!
(setq-default indent-tabs-mode nil)

;; Remove trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; set font
(set-frame-font "Source Code Pro 11")

;; move between windows with shift-arrow keys
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; turn on recent file mode
(recentf-mode t)

;; ////////////////////////////////////////////////////////////

;;;; THEMES
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

;; theme to load
(load-theme 'gruvbox-dark-hard t)

;; ////////////////////////////////////////////////////////////

;;;; LANGUAGE SPECIFIC

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

;; mips assembly
(use-package mips-mode
  :ensure t)

;; ////////////////////////////////////////////////////////////

;;;; GENERAL KEY REMAPPINGS
(global-set-key (kbd "C-c b") 'eval-buffer) ;; C-c b to eval buffer
(global-set-key (kbd "C-c e") 'eval-defun) ;; C-c e to eval defun

;; open recent files
(global-set-key (kbd "C-c r") 'recentf-open-files)
(global-set-key (kbd "C-c l") 'recentf-open-most-recent-file)

;; ////////////////////////////////////////////////////////////

;;;; EMACS PACKAGES
;; use Evil mode
(use-package evil
  :ensure t)
(evil-mode t)

;;; evil nerd commentator
(use-package evil-nerd-commenter
  :ensure t)
;; Emacs key bindings
(global-set-key (kbd "C-c c") 'evilnc-comment-or-uncomment-lines)
(global-set-key (kbd "C-c / l") 'evilnc-quick-comment-or-uncomment-to-the-line)
(global-set-key (kbd "C-c / c") 'evilnc-copy-and-comment-lines)
(global-set-key (kbd "C-c p") 'evilnc-comment-or-uncomment-paragraphs)

;; better defaults!
(use-package better-defaults
  :ensure t)

;; smooth scrolling
(use-package smooth-scrolling
  :ensure t)
(smooth-scrolling-mode 1)

;; auto-complete support
(use-package auto-complete
  :ensure t)
(ac-config-default)
(setq ac-auto-show-menu 0)   ; show menu immediately
(setq ac-ignore-case 'smart) ; use smart case
(global-auto-complete-mode t)

;; flycheck for syntax checking
(use-package flycheck
  :ensure t
  :init (flycheck-mode))

;; ag - searching
;; dependent on silversearcher - sudo apt install silversearcher-ag
(use-package ag
  :ensure t)
(global-set-key (kbd "C-c a") 'ag)

;; google this: C-c g to google
(use-package google-this
  :ensure t)
(google-this-mode 1)

;; yasnippet
(use-package yasnippet
  :ensure t)
(yas-global-mode 1)

;; smart mode line
(use-package smart-mode-line
  :ensure t)
;; (display-time-mode)
(rich-minority-mode 1)
(setf rm-blacklist "")
(sml/setup)

;; show line numbers
(use-package nlinum
  :ensure t)
;; (global-linum-mode t)
;; Make linums relative by default
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

;; aggressive indent mode
(use-package aggressive-indent
  :ensure t)
(global-aggressive-indent-mode 1)
(add-to-list 'aggressive-indent-excluded-modes 'html-mode)

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
(which-key-setup-side-window-bottom)

;; highlight instances of word when hovering
(use-package idle-highlight-mode
  :ensure t)
(add-hook 'prog-mode-hook (lambda () (idle-highlight-mode t)))

;; ace jump mode
(use-package ace-jump-mode
  :ensure t)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode) ;; C-c SPC to enable

;; undo tree
(use-package undo-tree
  :ensure t)
(global-undo-tree-mode)

;; whitespace
(use-package whitespace
  :ensure t)

(use-package rainbow-delimiters
  :ensure t)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; ////////////////////////////////////////////////////////////

;;;; IVY
(use-package ivy
  :ensure t)
(use-package counsel
  :ensure t)
(use-package swiper
  :ensure t)
(use-package flx
  :ensure t)

;; use fuzzy matching
(setq ivy-re-builders-alist
      '((ivy-switch-buffer . ivy--regex-plus)
        (t . ivy--regex-fuzzy)))
(setq ivy-initial-inputs-alist nil)

;; ivy settings
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

;; ////////////////////////////////////////////////////////////

;;;; ORG MODE
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" default)))
 '(sml/mode-width
   (if
       (eq
        (powerline-current-separator)
        (quote arrow))
       (quote right)
     (quote full)))
 '(sml/pos-id-separator
   (quote
    (""
     (:propertize " " face powerline-active1)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (car powerline-default-separator-dir)))
                   (quote powerline-active1)
                   (quote powerline-active2))))
     (:propertize " " face powerline-active2))))
 '(sml/pos-minor-modes-separator
   (quote
    (""
     (:propertize " " face powerline-active1)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (cdr powerline-default-separator-dir)))
                   (quote powerline-active1)
                   (quote sml/global))))
     (:propertize " " face sml/global))))
 '(sml/pre-id-separator
   (quote
    (""
     (:propertize " " face sml/global)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (car powerline-default-separator-dir)))
                   (quote sml/global)
                   (quote powerline-active1))))
     (:propertize " " face powerline-active1))))
 '(sml/pre-minor-modes-separator
   (quote
    (""
     (:propertize " " face powerline-active2)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (cdr powerline-default-separator-dir)))
                   (quote powerline-active2)
                   (quote powerline-active1))))
     (:propertize " " face powerline-active1))))
 '(sml/pre-modes-separator (propertize " " (quote face) (quote sml/modes))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
