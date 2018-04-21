;;;; package --- summary
;;;; Commentary:
;;;; my Emacs config - its a work in progress

;;; Code:
;;;; GENERAL PACKAGE SETTINGS
(cond
 ((>= 24 emacs-major-version)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  )
 )

;; bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;;; USE PACKAGE
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path "<path where use-package is installed>")
  (require 'use-package))

;; always ensure packages are installed
(setq use-package-always-ensure t)

;; backup settings
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; ////////////////////////////////////////////////////////////

;;;; THEME SETTINGS

;; Set theme here
(use-package material-theme)

(use-package nord-theme
  :config
  (setq nord-comment-brightness 15)
  (load-theme 'nord t))

;; ////////////////////////////////////////////////////////////

;;;; STARTUP SETTINGS

;; inhibit startup screen
(setq inhibit-startup-screen t)

;; no toolbar
(tool-bar-mode -1)

;; no scrollbar
(scroll-bar-mode -1)

;; no menubar
(menu-bar-mode -1)

;; inhibit visual bells
(setq visible-bell nil
      ring-bell-function #'ignore)

;; use y/n for all yes/no prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; highlight current line
(global-hl-line-mode t)
(defvar hl-line-face)
(set-face-attribute hl-line-face nil :underline nil)

;; spaces by default instead of tabs!
(setq-default indent-tabs-mode nil)

;; Remove trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; set font
(set-frame-font "Ubuntu Mono 14")

;; show matching parens
(show-paren-mode 1)

;; turn on recent file mode
(recentf-mode t)

;; filename in titlebar
(setq frame-title-format '((:eval (if (buffer-file-name)
(abbreviate-file-name (buffer-file-name)) "%b"))))

;; ////////////////////////////////////////////////////////////

;; mode line format
(setq-default mode-line-format
      (list
       ;; `mode-name'
       "%m: "
       ;; current buffer name
       "%& %b"
       ;; % buffer above top of window
       " | %P"
       ;; current line number
       " | line %l"
       ;; current column number
       " | col %c"
       ))

;; unique buffer names in mode line
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; ////////////////////////////////////////////////////////////

;;;; GENERAL KEY REMAPPINGS

;; eval lisp code settings
(global-set-key (kbd "C-c b") 'eval-buffer) ;; C-c b to eval buffer
(global-set-key (kbd "C-c e") 'eval-defun) ;; C-c e to eval defun

;; open recent files
(global-set-key (kbd "C-c r") 'recentf-open-files)
(global-set-key (kbd "C-c l") 'recentf-open-most-recent-file)

;; flyspell commands (spellcheck)
(global-set-key (kbd "C-c s") 'flyspell-buffer)
(global-set-key (kbd "C-c w") 'flyspell-auto-correct-word)
(global-set-key (kbd "C-c n") 'flyspell-goto-next-error)

;; git-gutter mode
(global-set-key (kbd "C-c g") 'global-git-gutter-mode)

;; window resizing
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; move between windows with shift-arrow keys
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; window splits
(global-set-key (kbd "C-c |") 'split-window-horizontally)
(global-set-key (kbd "C-c -") 'split-window-vertically)
(global-set-key (kbd "C-c d") 'delete-window)

;; neotree toggle
(global-set-key (kbd "C-c t") 'neotree-toggle)

;; zap up to char
(autoload 'zap-up-to-char "misc"
  'interactive)
(global-set-key "\M-z" 'zap-up-to-char)

;; chords
(key-chord-define-global ",z" 'zap-up-to-char)
(key-chord-define-global ",s" 'swiper)
(key-chord-define-global ",t" 'neotree-toggle)

;; Ivy settings
(global-set-key (kbd "C-s") 'swiper)
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
(global-set-key (kbd "C-c C-r") 'ivy-resume)

;; ////////////////////////////////////////////////////////////

;;;; PACKAGES

;; git-gutter+
(use-package git-gutter
  :config
  (global-git-gutter-mode t))

;; git time machine
(use-package git-timemachine)

;; magit- for git
(use-package magit
  :bind ("C-x g" . magit-status))

;; chords
(use-package use-package-chords
  :config
  (key-chord-mode 1))

;; smooth-scrolling
(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode 1))

;; neotree (file tree)
(use-package neotree)

;; highlight-symbol
(use-package highlight-symbol)

;; flycheck for syntax checking
(use-package flycheck
  :config
  (global-flycheck-mode t))

;; ag - searching
;; dependent on silversearcher - sudo apt install silversearcher-ag
(use-package ag
  :init
  (global-set-key (kbd "C-c a") 'ag))

;; fzf file finder
(use-package fzf)

;; dumb jump- attempts to search for source like IDE
(use-package dumb-jump
  :diminish dumb-jump-mode
  :bind (("C-M-g" . dumb-jump-go)
         ("C-M-p" . dumb-jump-back)
         ("C-M-q" . dumb-jump-quick-look)))

;; ivy
(use-package counsel
  :config
  (counsel-mode 1))

(use-package ivy
  :demand
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d ")
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

;; smartparens
(use-package smartparens
  :diminish smartparens-mode
  :config
  (add-hook 'prog-mode-hook 'smartparens-mode))

;; rainbow delimiters
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; rainbow mode- highlight strings that represent colors
(use-package rainbow-mode
  :config
  (setq rainbow-x-colors nil)
  (add-hook 'prog-mode-hook 'rainbow-mode))

;; indent-guide
(use-package indent-guide
  :config
  (indent-guide-global-mode))

;; which-key
(use-package which-key
  :config
  (which-key-mode t)
  (which-key-setup-side-window-bottom))

;; idle-highlight-mode
(use-package idle-highlight-mode
  :config
  (add-hook 'prog-mode-hook (lambda () (idle-highlight-mode t))))

;; avy
(use-package avy
  :chords (("jj" . avy-goto-char-2)
           ("jl" . avy-goto-line)))

;; ace window
(use-package ace-window
  :chords ("jk" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; undo tree
(use-package undo-tree
  :config
  (global-undo-tree-mode))

;; visual undo-tree
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (progn (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

;; drag stuff mode (M-<arrow> to move lines of text)
(use-package drag-stuff
  :config
  (drag-stuff-global-mode t)
  (drag-stuff-define-keys))

;; ////////////////////////////////////////////////////////////

;;;; COMPANY MODE

(use-package company
  :commands company-mode
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  (global-company-mode '(not markdown-mode)))

;; customize
(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
   `(company-tooltip ((t (:inherit default :background
                                   ,(color-lighten-name bg 2)))))
   `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
   `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
   `(company-tooltip-selection ((t (:inherit
                                    font-lock-function-name-face))))
   `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))

;; ////////////////////////////////////////////////////////////

;;;; LANGUAGE SETTINGS AND PACKAGES

;; markdown mode for md files
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; SCALA
(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))
;; SBT for Scala
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))
(use-package ensime)

;; JAVA
(use-package jdee)

;; GOLANG
(use-package go-mode)
(use-package company-go)
(add-hook 'go-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-go))
                          (company-mode)))

;; ////////////////////////////////////////////////////////////

;;;; IDO SETTINGS

(global-set-key
 "\M-x"
 (lambda ()
   (interactive)
   (call-interactively
    (intern
     (ido-completing-read
      "M-x "
      (all-completions "" obarray 'commandp))))))

;; ido vertical mode
(use-package ido-vertical-mode
  :ensure t
  :init
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))

;; flx for fuzzy matching
(use-package flx-ido
  :ensure t
  :init
  (flx-ido-mode 1)
  ;; disable ido faces to see flx highlights.
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil))

(provide 'init.el)
;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("7527f3308a83721f9b6d50a36698baaedc79ded9f6d5bd4e9a28a22ab13b3cb1" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
