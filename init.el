
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


;; turn on recent file mode
(recentf-mode t)

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

;; ////////////////////////////////////////////////////////////

;;;; PACKAGES

;; magit
(use-package magit
  :ensure t)

;; git-gutter+
(use-package git-gutter
  :ensure t)
(global-git-gutter-mode t)

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

;; ido
(use-package ido
  :ensure t
  :init
  (ido-mode t)
  (ido-everywhere 1))

;; autopair
(use-package autopair
  :ensure t
  :init
  (autopair-global-mode t))

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

;; ace jump mode
(use-package ace-jump-mode
  :ensure t)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode) ;; C-c SPC to enable

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
