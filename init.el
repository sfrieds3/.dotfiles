;;;; package --- summary
;;; Commentary:
;;;     place 'local-settings.el' file (provide 'local-settings)
;;;     in .emacs.d directory to overwrite settings (loaded at end)

;;; Code:
;;;; GENERAL PACKAGE SETTINGS
  (require 'package)
  (setq package-enable-at-startup nil)
  (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
  ;;(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
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

;; set default preferred fonts
(defvar platform-default-font)
(setq platform-default-font
      (cond ((eq system-type 'windows-nt) "Hack 11")
            ((eq system-type 'gnu/linux) "DejaVu Sans Mono 11")
            (t nil)))

(when platform-default-font
  (set-frame-font platform-default-font nil t))

;; Set theme here
(use-package base16-theme)
(load-theme 'base16-gruvbox-dark-hard t)

;; ////////////////////////////////////////////////////////////

;;;; STARTUP SETTINGS

;; always start emacsclient
(server-start)

;; 0 line buffer before/after cursor
(setq scroll-margin 0)
(setq smooth-scroll-margin 3)

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

;; show matching parens
(show-paren-mode 1)

;; turn on recent file mode
(recentf-mode t)

;; filename in titlebar
(setq frame-title-format '((:eval (if (buffer-file-name)
(abbreviate-file-name (buffer-file-name)) "%b"))))

;; ////////////////////////////////////////////////////////////

;; PERSONAL FUNCTIONS

;; ////////////////////////////////////////////////////////////

;; grep in current directory
(defun dgrep ()
  "Run grep recursively from the directory of the current buffer or the default directory."
  (interactive)
  (let ((dir (file-name-directory (or load-file-name buffer-file-name default-directory))))
    (let ((command (read-from-minibuffer "Run grep (like this): "
                                         (cons (concat "grep -nH -ir  " dir) 14))))
      (grep command))))

;; grep in current file
(defun fgrep ()
  "Run grep in the curRent file."
  (interactive)
  (let ((fname (buffer-file-name)))
    (let ((command (read-from-minibuffer "Run grep (like this): "
                                         (cons (concat "grep -nH -ir  " fname) 14))))
      (grep command))))

;; ////////////////////////////////////////////////////////////

;;;; PACKAGES

;; projectile
(use-package projectile
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; evil mode
(use-package evil
  :config
  (evil-mode 1))

;; Make movement keys work like they should
(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

; Make horizontal movement cross lines
(setq-default evil-cross-lines t)

;; evil nerd commenter
(use-package evil-nerd-commenter)

;; modeline
(use-package mood-line
  :config
  (mood-line-mode 1))
(use-package minions
  :config
  (minions-mode 1))

;; git-gutter
(use-package git-gutter
  :config
  (global-git-gutter-mode t))

;; magit- for git
(use-package magit)

;; ws-butler: trim trailing whitespace
(use-package ws-butler
  :config
  (ws-butler-global-mode t))

;; smooth-scrolling
(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode 1))

;; highlight-symbol
(use-package highlight-symbol)

;; flycheck for syntax checking
(use-package flycheck
  :config
  (global-flycheck-mode t))

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
        (t . ivy--regex-fuzzy)))
  (setq ivy-initial-inputs-alist nil))

(use-package counsel
  :config
  (counsel-mode 1))

;; flx - fuzzy match sorting
(use-package flx)

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

;; matching parenthesis
(use-package smartparens
  :config
  (smartparens-global-mode t))


;; ////////////////////////////////////////////////////////////

;;;; COMPANY MODE

(use-package company
  :commands company-mode
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  (global-company-mode '(not markdown-mode)))

(with-eval-after-load 'company
  (add-hook 'company-mode-hook (lambda ()
                                 (add-to-list 'company-backends 'company-capf)))
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "C-SPC") #'company-complete-selection))

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

(define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
(define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)

;; ////////////////////////////////////////////////////////////

;;;; LANGUAGE SETTINGS AND PACKAGES

;; markdown mode for md files
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; clang
(use-package irony
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package company-irony)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

(use-package flycheck-irony)

(defun my-c++-mode-hook ()
  (setq c-basic-offset 4)
  (c-set-offset 'substatement-open 0))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

;; GOLANG
(use-package go-mode)
(use-package company-go)
(add-hook 'go-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-go))
                          (company-mode)))

;; python
(use-package company-jedi)
(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))
(add-hook 'python-mode-hook 'my/python-mode-hook)
(setq jedi:complete-on-dot t)

;; ////////////////////////////////////////////////////////////

;;;; EVIL MODE SETTINGS

(add-hook 'neotree-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look)
            (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
            (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

;; ////////////////////////////////////////////////////////////

;;;; GENERAL KEY REMAPPINGS

;; move between windows with shift-arrow keys
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
(global-set-key (kbd "M-o") 'ace-window)

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
(global-set-key (kbd "C-c D") 'delete-window)
(global-set-key (kbd "C-c t") 'neotree-toggle)
(global-set-key (kbd "C-c g") 'magit-diff)
(global-set-key (kbd "C-c c") 'avy-goto-char)
(global-set-key (kbd "C-c C") 'avy-goto-char-2)
(global-set-key (kbd "C-c l") 'avy-goto-line)
(global-set-key (kbd "C-c \\") 'diff-buffer-with-file)
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
(global-set-key (kbd "C-c D") 'counsel-unicode-char)
(global-set-key (kbd "C-c a") 'counsel-ag)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-c m") 'counsel-mark-ring)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "C-c L") 'goto-line)
(global-set-key (kbd "C-c SPC") 'evilnc-comment-or-uncomment-lines)
(global-set-key (kbd "S-C-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>")  'shrink-window)
(global-set-key (kbd "S-C-<up>")    'enlarge-window)

;; ////////////////////////////////////////////////////////////

;; platform specific files
(let ((local-settings (expand-file-name "local-settings.el" user-emacs-directory)))
  (when (file-exists-p local-settings)
    (load-file local-settings)))

;; ////////////////////////////////////////////////////////////

(provide 'init.el)
;;; init.el ends here
