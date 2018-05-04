;;;; package --- summary
;;;; Commentary:

;;; Code:
;;;; GENERAL PACKAGE SETTINGS
  (require 'package)
  (setq package-enable-at-startup nil)
  (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (package-initialize)

;; bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;;; USE PACKAGE
 (eval-when-compile
   (add-to-list 'load-path "~/.emacs.d/elpa")
   (require 'use-package))

;; always ensure packages are installed
(setq use-package-always-ensure t)

;; backup settings
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; temp file for custom settings
(setq custom-file (make-temp-file "emacs-custom"))

;; ////////////////////////////////////////////////////////////

;;;; THEME SETTINGS

;; set font
(set-frame-font "Ubuntu Mono 14")

;; Set theme here
(use-package material-theme)

(use-package nord-theme
  :config
  (setq nord-comment-brightness 15)
  (load-theme 'nord t))

;; ////////////////////////////////////////////////////////////

;;;; STARTUP SETTINGS

;; cursor always blinks
(setq blink-cursor-blinks -1)

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

;; move between windows with shift-arrow keys
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; zap up to char
(autoload 'zap-up-to-char "misc"
  'interactive)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-S-z") 'zap-up-to-char)

;; key customizations
(global-set-key (kbd "C-c b") 'eval-buffer)
(global-set-key (kbd "C-c e") 'eval-defun)
(global-set-key (kbd "C-c r") 'recentf-open-files)
(global-set-key (kbd "C-c l") 'recentf-open-most-recent-file)
(global-set-key (kbd "C-c s") 'flyspell-buffer)
(global-set-key (kbd "C-c w") 'flyspell-auto-correct-word)
(global-set-key (kbd "C-c n") 'flyspell-goto-next-error)
(global-set-key (kbd "C-c g") 'global-git-gutter-mode)
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
(global-set-key (kbd "C-c t") 'neotree-toggle)
(global-set-key (kbd "C-c |") 'split-window-horizontally)
(global-set-key (kbd "C-c -") 'split-window-vertically)
(global-set-key (kbd "C-c D") 'delete-window)
(global-set-key (kbd "C-c t") 'neotree-toggle)
(global-set-key (kbd "C-c g") 'magit-diff)
(global-set-key (kbd "C-c c") 'avy-goto-char)
(global-set-key (kbd "C-c C") 'avy-goto-char-2)
(global-set-key (kbd "C-c l") 'avy-goto-line)
(global-set-key (kbd "C-c \\") 'diff-buffer-with-file)
(global-set-key (kbd "C-c k") 'kill-whole-line)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-M-g") 'dumb-jump-go)
(global-set-key (kbd "C-M-p") 'dumb-jump-back)
(global-set-key (kbd "C-M-q") 'dumb-jump-quick-look)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c a") 'counsel-ag)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-c m") 'counsel-mark-ring)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "C-S-k") 'kill-whole-line)
(global-set-key (kbd "C-S-b") 'left-word)
(global-set-key (kbd "C-S-f") 'right-word)
(global-set-key (kbd "C-c L") 'goto-line)
(global-set-key (kbd "C-S-d") 'kill-word)
(global-set-key (kbd "C-c SPC") 'comment-line)

;; ////////////////////////////////////////////////////////////

;;;; PACKAGES

;; evil mode
(use-package evil
  :config
  (evil-mode 1))

;; git-gutter+
(use-package git-gutter
  :config
  (global-git-gutter-mode t))

;; git time machine
(use-package git-timemachine)

;; magit- for git
(use-package magit)

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
(use-package ag)

;; fzf file finder
(use-package fzf)

;; dumb jump- attempts to search for source like IDE
(use-package dumb-jump
  :diminish dumb-jump-mode)

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
(use-package avy)

;; ace window
(use-package ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

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


;; ////////////////////////////////////////////////////////////

;;;; EVIL MODE SETTINGS

(add-hook 'neotree-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look)
            (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
            (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

;; ////////////////////////////////////////////////////////////

(provide 'init.el)
;;; init.el ends here
