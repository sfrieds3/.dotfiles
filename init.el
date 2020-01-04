;;;; package --- summary
;;; Commentary:
;;;     place 'local-settings.el' file (provide 'local-settings)
;;;     in .emacs.d directory to overwrite settings (loaded at end)

;;; Code:
;;;; GENERAL PACKAGE SETTINGS
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      '(("gnu-elpa"     . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("melpa-stable" . 2)
        ("gnu-elp"      . 1)
        ("melpa"        . 0)))
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

;; ////////////////////////////////////////////////////////////

;; set default preferred fonts
(defvar platform-default-font)
(setq platform-default-font
      (cond ((eq system-type 'windows-nt) "Hack 11")
            ((eq system-type 'gnu/linux) "DejaVu Sans Mono 10")
            (t nil)))

(when platform-default-font
  (set-frame-font platform-default-font nil t))

(use-package base16-theme)
(load-theme 'base16-gruvbox-dark-hard t)

;; ////////////////////////////////////////////////////////////

;;;; STARTUP SETTINGS

;; ////////////////////////////////////////////////////////////

;; always start emacsclient
(server-start)

;; make scrolling work like it should
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

;; cursor always blinks
(setq blink-cursor-blinks -1)

;; visuals
(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; inhibit visual bells
(setq visible-bell nil
      ring-bell-function #'ignore)

;; use y/n for all yes/no prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; highlight current line
(global-hl-line-mode -1)
(defvar hl-line-face)
(set-face-attribute hl-line-face nil :underline nil)

;; spaces by default instead of tabs!
(setq-default indent-tabs-mode nil)

;; show matching parens
(show-paren-mode 1)

;; smart parens
(electric-pair-mode t)

;; turn on recent file mode
(recentf-mode t)

;; filename in titlebar
(setq frame-title-format '((:eval (if (buffer-file-name)
(abbreviate-file-name (buffer-file-name)) "%b"))))

;; C-x w h [REGEX] <RET> <RET> to highlight all occurances of [REGEX], and C-x w r [REGEX] <RET> to unhighlight them again.
(global-hi-lock-mode 1)

;; highlight TODOs
(highlight-phrase "TODO" 'hi-yellow)

;; ////////////////////////////////////////////////////////////

;; PERSONAL FUNCTIONS

;; ////////////////////////////////////////////////////////////

;; grep in current directory
(defun dir-grep ()
  "Run grep recursively from the directory of the current buffer or the default directory."
  (interactive)
  (let ((dir (file-name-directory (or load-file-name buffer-file-name default-directory))))
    (let ((command (read-from-minibuffer "Run grep (like this): "
                                         (cons (concat "grep --color --null -nH -ir -e  " dir) 32))))
      (grep command))))

;; grep in current file
(defun file-grep ()
  "Run grep in the current file."
  (interactive)
  (let ((fname (buffer-file-name)))
    (let ((command (read-from-minibuffer "Run grep (like this): "
                                         (cons (concat "grep --color --null -nH -ir -e  " fname) 32))))
      (grep command))))

;; ////////////////////////////////////////////////////////////

;; EVIL CONFIG

;; ////////////////////////////////////////////////////////////

;; evil leader
(use-package evil-leader
      :commands (evil-leader-mode)
      :init
      (global-evil-leader-mode)
      :config
      (progn
        (evil-leader/set-leader ",")
        (evil-leader/set-key "," 'other-window)
        (evil-leader/set-key "W" 'delete-trailing-whitespace)))

;; evil mode
(use-package evil
  :init
  (evil-mode)
  :config
  (progn
    ;; Make movement keys work like they should
    (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
    (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
    (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
    (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
    ;; easy window switching
    (define-key evil-normal-state-map (kbd "M-h") 'evil-window-left)
    (define-key evil-normal-state-map (kbd "M-j") 'evil-window-down)
    (define-key evil-normal-state-map (kbd "M-k") 'evil-window-up)
    (define-key evil-normal-state-map (kbd "M-l") 'evil-window-right))

  ;; Make horizontal movement cross lines
  (setq-default evil-cross-lines t)

  ;; evil bindings for occur mode
  (add-hook 'occur-mode-hook
            (lambda ()
              (evil-add-hjkl-bindings occur-mode-map 'emacs
                (kbd "/")       'evil-search-forward
                (kbd "n")       'evil-search-next
                (kbd "N")       'evil-search-previous
                (kbd "C-d")     'evil-scroll-down
                (kbd "C-u")     'evil-scroll-up
                (kbd "C-w C-w") 'other-window))))

;; make evil understand word correctly (i.e. include '-' and '_')
(with-eval-after-load 'evil
    (defalias #'forward-evil-word #'forward-evil-symbol)
    ;; make evil-search-word look for symbol rather than word boundaries
    (setq-default evil-symbol-word-search t))

;; ////////////////////////////////////////////////////////////

;; PACKAGES

;; ////////////////////////////////////////////////////////////

;; modeline
(use-package mood-line
  :config
  (mood-line-mode 1))

;; git-gutter
(use-package git-gutter
  :config
  (global-git-gutter-mode t))

(use-package magit)

;; dumb jump- attempts to search for source like IDE
(use-package dumb-jump
  :diminish dumb-jump-mode)

;; ivy
(use-package ivy
  :demand
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-re-builders-alist
        '((ivy-switch-buffer . ivy--regex-plus)
          (swiper . regexp-quote)
          (t . ivy--regex-fuzzy)))
  (setq ivy-initial-inputs-alist nil))

(use-package counsel
  :config
  (counsel-mode 1))

;; indent-guide
(use-package indent-guide
  :config
  (indent-guide-global-mode))

;; which-key
(use-package which-key
  :config
  (which-key-mode t)
  (which-key-setup-side-window-bottom))

;; avy - go to characters
(use-package avy)

;; ace window
(use-package ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; drag stuff mode (M-<arrow> to move lines of text)
(use-package drag-stuff
  :config
  (drag-stuff-global-mode t))

;; ////////////////////////////////////////////////////////////

;;;; LANGUAGE SETTINGS AND PACKAGES

;; ////////////////////////////////////////////////////////////

;; c++
(defun my-c++-mode-hook ()
  (defvar c-basic-offset)
  (setq c-basic-offset 4)
  (c-set-offset 'substatement-open 0))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

;; common lisp
(use-package slime
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq slime-contribs '(slime-fancy)))

;; clojure
(use-package cider)

;; ////////////////////////////////////////////////////////////

;;;; GENERAL KEY REMAPPINGS

;; move between windows with shift-arrow keys
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
(global-set-key (kbd "M-o") 'ace-window)
;; key customizations
;; custom functions
(global-set-key (kbd "C-c g d") 'dir-grep)
(global-set-key (kbd "C-c g s") 'file-grep)
(global-set-key (kbd "C-c g f") 'find-dired)
;; general customizations
(global-set-key (kbd "C-c [") 'previous-error)
(global-set-key (kbd "C-c ]") 'next-error)
(global-set-key (kbd "C-c o") 'occur)
(global-set-key (kbd "C-c b") 'eval-buffer)
(global-set-key (kbd "C-c e") 'eval-defun)
(global-set-key (kbd "C-c r") 'recentf-open-files)
(global-set-key (kbd "C-c l") 'recentf-open-most-recent-file)
(global-set-key (kbd "C-c L") 'goto-line)
;; flycheck
(global-set-key (kbd "C-c f n") 'flycheck-next-error)
(global-set-key (kbd "C-c f p") 'flycheck-previous-error)
(global-set-key (kbd "C-c f f") 'flycheck-list-errors)
;; flyspell
(global-set-key (kbd "C-c f s") 'flyspell-buffer)
(global-set-key (kbd "C-c f w") 'flyspell-auto-correct-word)
(global-set-key (kbd "C-c f e") 'flyspell-goto-next-error)
;; window management
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
(global-set-key (kbd "C-c D") 'delete-window)
;; avy
(global-set-key (kbd "C-c c") 'avy-goto-char)
(global-set-key (kbd "C-c C") 'avy-goto-char-2)
(global-set-key (kbd "C-c l") 'avy-goto-line)
;; magit/git stuff
(global-set-key (kbd "C-c \\") 'magit-diff-buffer-file)
;; dumb jump
(global-set-key (kbd "C-M-g") 'dumb-jump-go)
(global-set-key (kbd "C-M-p") 'dumb-jump-back)
(global-set-key (kbd "C-M-q") 'dumb-jump-quick-look)
;; drag stuff
(global-set-key (kbd "M-<up>") 'drag-stuff-up)
(global-set-key (kbd "M-<down>") 'drag-stuff-down)
;; counsel/ivy/swiper
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "C-c D") 'counsel-unicode-char)
(global-set-key (kbd "C-c a") 'counsel-ag)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-c m") 'counsel-mark-ring)
(global-set-key (kbd "C-c C-r") 'ivy-resume)

;; ////////////////////////////////////////////////////////////

;; platform specific files
(let ((local-settings (expand-file-name "local-settings.el" user-emacs-directory)))
  (when (file-exists-p local-settings)
    (load-file local-settings)))

;; ////////////////////////////////////////////////////////////

(provide 'init.el)
;;; init.el ends here
