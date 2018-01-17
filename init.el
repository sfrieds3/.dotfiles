;;;; package --- summary
;;;; Commentary:
;;;; my Emacs config - its a work in progress
;;;;
;;;; requirements:
;;;;   silversearcher - sudo apt install silversearcher-ag
;;;;   jdee - Java backend (https://github.com/jdee-emacs/jdee-server)


;;; Code:
;;;; GENERAL PACKAGE SETTINGS
(require 'package)

(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

;; use use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;;(use-package cl-lib
;;  :ensure t)

;; ////////////////////////////////////////////////////////////

;;;; STARTUP SETTINGS

;; inhibit startup screen
(setq inhibit-startup-screen t)

;; inhibit visual bells
(setq visible-bell nil
      ring-bell-function #'ignore)

;; set window size on open
(when window-system (set-frame-size (selected-frame) 135 35))

;; start with split windows
;; Open split shell on launch
(add-hook 'emacs-startup-hook
          (lambda ()
            (split-window-horizontally)
            (other-window 1)
            (split-window-vertically)
            (other-window 2)))

;; highlight current line
(global-hl-line-mode t)

;; spaces by default instead of tabs!
(setq-default indent-tabs-mode nil)

;; Remove trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; set font
(set-frame-font "Hack 11")

;; move between windows with shift-arrow keys
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; move between windows with Alt-arrow
(global-set-key [M-left] 'windmove-left)          ; move to left window
(global-set-key [M-right] 'windmove-right)        ; move to right window
(global-set-key [M-up] 'windmove-up)              ; move to upper window
(global-set-key [M-down] 'windmove-down)          ; move to lower window

;; turn on recent file mode
(recentf-mode t)

;; ////////////////////////////////////////////////////////////

;;;; DEPENDENCIES

;; add load path
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; other required files
(require 'theme.el)
(require 'cmode.el)

;; ////////////////////////////////////////////////////////////

;;;; THEMES
;;;; gruvbox
;; (use-package gruvbox-theme
;;  :ensure t)
;;;; blackboard
;;(use-package blackboard-theme
;;:ensure t)
;;;; dracula
;;(use-package dracula-theme
;;:ensure t)
;;;; zenburn
;;(use-package zenburn-theme
;;:ensure t)
;;;; moe
;;(use-package moe-theme
;;:ensure t)
;;;; monokai
;;(use-package monokai-theme
;;:ensure t)
;;;; solarized
;;(use-package solarized-theme
;;:ensure t)
;;
;;;; theme to load
;;(load-theme 'solarized-dark t)

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

;; flyspell commands (spellcheck)
(global-set-key (kbd "C-c s") 'flyspell-buffer)
(global-set-key (kbd "C-c w") 'flyspell-auto-correct-word)
(global-set-key (kbd "C-c n") 'flyspell-goto-next-error)

;; window resizing
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

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

;; flycheck for syntax checking
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

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
(use-package minimap
  :ensure t)

;; magit - for git
(use-package magit
  :ensure t)
(use-package ghub
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

(provide 'init.el)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(jdee-server-dir ""))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
