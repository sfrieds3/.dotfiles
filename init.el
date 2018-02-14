;;;; package --- summary
;;;; Commentary:
;;;; my Emacs config - its a work in progress
;;;;
;;;; requirements:
;;;;   silversearcher - sudo apt install silversearcher-ag
;;;;   jdee - Java backend (https://github.com/jdee-emacs/jdee-server)

;;; Code:
;;;; GENERAL PACKAGE SETTINGS
(cond
 ((>= 24 emacs-major-version)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (package-refresh-contents)
 )
)

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
(set-face-attribute hl-line-face nil :underline nil)

;; spaces by default instead of tabs!
(setq-default indent-tabs-mode nil)

;; Remove trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; set font
(set-frame-font "Ubuntu Mono 12")

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
(require 'autocomplete.el)
(require 'packageload.el)

;; ////////////////////////////////////////////////////////////

;;;; LANGUAGE SPECIFIC

;; markdown mode for md files
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(autoload 'gfm-mode "markdown-mode"
  "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

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

;; use Evil mode
(evil-mode t)

;;; evil nerd commentator
(global-set-key (kbd "C-c c") 'evilnc-comment-or-uncomment-lines)
(global-set-key (kbd "C-c / l") 'evilnc-quick-comment-or-uncomment-to-the-line)
(global-set-key (kbd "C-c / c") 'evilnc-copy-and-comment-lines)
(global-set-key (kbd "C-c p") 'evilnc-comment-or-uncomment-paragraphs)

;; smooth-scrolling
(smooth-scrolling-mode 1)

;; flycheck for syntax checking
(global-flycheck-mode t)

;; ag - searching
;; dependent on silversearcher - sudo apt install silversearcher-ag
(global-set-key (kbd "C-c a") 'ag)

;; google-this: C-c g to google
(google-this-mode 1)

;; yasnippet
;; (yas-global-mode 1)

;; autopair
(autopair-global-mode t)

;; indent-guide
(indent-guide-global-mode)
(setq indent-guide-recursive t)

;; aggressive-indent
(global-aggressive-indent-mode 1)
(add-to-list 'aggressive-indent-excluded-modes 'html-mode)

;; which-key
(which-key-mode t)
(which-key-setup-side-window-bottom)

;; idle-highlight-mode
(add-hook 'prog-mode-hook (lambda () (idle-highlight-mode t)))

;; ace jump mode
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode) ;; C-c SPC to enable

;; undo tree
(global-undo-tree-mode)

;; rainbow-delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; ////////////////////////////////////////////////////////////

;;;; Settings for ivy, counsel, swiper, flx

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
