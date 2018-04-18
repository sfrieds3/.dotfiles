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

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; ////////////////////////////////////////////////////////////

;;;; THEME SETTINGS

;; Set theme here
(use-package material-theme
  :ensure t)

(use-package nord-theme
  :ensure t
  :init
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

;; zap up to char
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.

  \(fn arg char)"
  'interactive)
(global-set-key "\M-z" 'zap-up-to-char)

;; key-chords
;; (defun jc/switch-to-previous-buffer ()
;;   (interactive)
;;   (switch-to-buffer (other-buffer (current-buffer) 1)))

;; (key-chord-define-global "JJ" 'jc/switch-to-previous-buffer)

;; ////////////////////////////////////////////////////////////

;;;; PACKAGES

;; git-gutter+
(use-package git-gutter
  :ensure t)
(global-git-gutter-mode t)

;; git time machine
(use-package git-timemachine
  :ensure t)

;; magit- for git
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))


;; chords
(use-package use-package-chords
  :ensure t
  :config
  (key-chord-mode 1))

;; crux - C-a move to first non-whitespace char
(use-package crux
  :ensure t
  :bind (("C-a" . crux-move-beginning-of-line)))

;; smooth-scrolling
(use-package smooth-scrolling
  :ensure t
  :init
  (smooth-scrolling-mode 1))

;; neotree (file tree)
(use-package neotree
  :ensure t)
(global-set-key (kbd "C-c t") 'neotree-toggle)

;; highlight-symbol
(use-package highlight-symbol
  :ensure t
  :init)

;; flycheck for syntax checking
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))

;; ag - searching
;; dependent on silversearcher - sudo apt install silversearcher-ag
(use-package ag
  :ensure t
  :init
  (global-set-key (kbd "C-c a") 'ag))

;; fzf file finder
(use-package fzf
  :ensure t)

;; dumb jump- attempts to search for source like IDE
(use-package dumb-jump
  :ensure t
  :diminish dumb-jump-mode
  :bind (("C-M-g" . dumb-jump-go)
         ("C-M-p" . dumb-jump-back)
         ("C-M-q" . dumb-jump-quick-look)))

;; ido
(use-package ido
  :ensure t
  :init
  (ido-mode t)
  (ido-everywhere 1))

;; smartparens
(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (add-hook 'prog-mode-hook 'smartparens-mode))

;; rainbow delimiters
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; rainbow mode- highlight strings that represent colors
(use-package rainbow-mode
  :ensure t
  :config
  (setq rainbow-x-colors nil)
  (add-hook 'prog-mode-hook 'rainbow-mode))

;; indent-guide
(use-package indent-guide
  :ensure t
  :init
  (indent-guide-global-mode))

;; which-key
(use-package which-key
  :ensure t
  :init
  (which-key-mode t)
  (which-key-setup-side-window-bottom))

;; idle-highlight-mode
(use-package idle-highlight-mode
  :ensure t)
(add-hook 'prog-mode-hook (lambda () (idle-highlight-mode t)))

;; avy
(use-package avy
  :ensure t
  :chords (("jj" . avy-goto-char-2)
           ("jl" . avy-goto-line)))

;; ace window
(use-package ace-window
  :ensure t
  :chords ("jk" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; undo tree
(use-package undo-tree
  :ensure t
  :init
(global-undo-tree-mode))

;; visual undo-tree
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

;; drag stuff mode (M-<arrow> to move lines of text)
(use-package drag-stuff
  :ensure t
  :init
  (drag-stuff-global-mode t)
  (drag-stuff-define-keys))

;; ////////////////////////////////////////////////////////////

;;;; COMPANY MODE

(use-package company
  :ensure t
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
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; SCALA
(use-package scala-mode
  :ensure t
  :interpreter
  ("scala" . scala-mode))
;; SBT for Scala
(use-package sbt-mode
  :ensure t
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))
(use-package ensime
  :ensure t)

;; JAVA
(use-package jdee
  :ensure t)

;; GOLANG
(use-package go-mode
  :ensure t)
(use-package company-go
  :ensure t)
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
