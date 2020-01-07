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
        ("melpa"        . "https://melpa.org/packages/")))

;; set package archive priorities if version 26+
(cond ((>= 26 emacs-major-version)
       (setq package-archive-priorities
             '(("melpa-stable" . 2)
               ("gnu-elp"      . 1)
               ("melpa"        . 0)))))

(package-initialize)

;; bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;;; USE PACKAGE
(eval-when-compile
  (add-to-list 'load-path "~/.emacs.d/elpa")
  (add-to-list 'load-path "~/.emacs.d/elisp")
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

(require 'base16-theme)
(require 'base16-gruvbox-dark-hard-theme)
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
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)

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

;; keep isearch results always nightlighted
;;(setq lazy-highlight-cleanup nil)

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

;; evil mode
(use-package evil
  :init
  (evil-mode)
  (progn
    (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
    (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
    (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
    (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
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
                (kbd "C-w C-w") 'other-window)))
  :config
  ;; make evil understand word correctly (i.e. include '-' and '_')
  (with-eval-after-load 'evil
    (defalias #'forward-evil-word #'forward-evil-symbol)
    ;; make evil-search-word look for symbol rather than word boundaries
    (setq-default evil-symbol-word-search t)))

;; evil leader
(use-package evil-leader
  :init
  (evil-leader-mode)
  (global-evil-leader-mode)
  (progn
    (evil-leader/set-leader ",")
    (evil-leader/set-key "," 'other-window)
    (evil-leader/set-key "W" 'delete-trailing-whitespace)
    (evil-leader/set-key "RET" 'lazy-highlight-cleanup)
    (evil-leader/set-key "h" 'dired-jump)))

;; ////////////////////////////////////////////////////////////

;; SMEX/IDO STUFF

;; ////////////////////////////////////////////////////////////

(require 'smex)

;;(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-use-filename-at-point 'guess)
(setq ido-cannot-complete-command 'ido-next-match)
(ido-mode 1)

;; fix keymap for ido completion
(defun ido-my-keys ()
  "Add my keybindings for ido."
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
  (define-key ido-completion-map (kbd "<backtab") 'ido-prev-match)
  (define-key ido-completion-map (kbd "<tab>") 'ido-exit-minibuffer)
  (define-key ido-completion-map (kbd "C-e") 'ido-exit-minibuffer))

(add-hook 'ido-setup-hook #'ido-my-keys)

;; find recent files and show in ido
(defun ido-open-recentf ()
  "Use ido to select a recently visited file from the `recentf-list'."
  (interactive)
  (find-file (ido-completing-read "Open file: " recentf-list nil t)))

;; ////////////////////////////////////////////////////////////

;; PACKAGES

;; ////////////////////////////////////////////////////////////

;; modeline
(require 'mood-line)
(mood-line-mode 1)

;; git-gutter
(require 'git-gutter)
(global-git-gutter-mode t)

(use-package magit)

(use-package flycheck
  :init
  (global-flycheck-mode))

;; dumb jump- attempts to search for source like IDE
(use-package dumb-jump)

;; indent-guide
(require 'indent-guide)
(indent-guide-global-mode)

;; which-key
(require 'which-key)
(which-key-mode t)
(which-key-setup-side-window-bottom)

;; avy - go to characters
(require 'avy)

;; ace window
(require 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

;; drag stuff mode (M-<arrow> to move lines of text)
(require 'drag-stuff)
(drag-stuff-global-mode t)

;; ////////////////////////////////////////////////////////////

;;;; LANGUAGE SETTINGS AND PACKAGES

;; ////////////////////////////////////////////////////////////

;; c++
(defun my-c++-mode-hook ()
  "C++ mode stuff."
  (defvar c-basic-offset)
  (setq c-basic-offset 4)
  (c-set-offset 'substatement-open 0))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

;; ////////////////////////////////////////////////////////////

;;;; GENERAL KEY REMAPPINGS

;; ////////////////////////////////////////////////////////////

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
;; easily find recent files
(global-set-key (kbd "C-x f") 'ido-open-recentf)
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
;;smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
;; hippie expand
(global-set-key (kbd "C-.") 'hippie-expand)

;; ////////////////////////////////////////////////////////////

;; platform specific files

;; ////////////////////////////////////////////////////////////

(let ((local-settings (expand-file-name "local-settings.el" user-emacs-directory)))
  (when (file-exists-p local-settings)
    (load-file local-settings)))

(let ((home-settings (expand-file-name "home.el" user-emacs-directory)))
  (when (file-exists-p home-settings)
    (load-file home-settings)))

;; ////////////////////////////////////////////////////////////

(provide 'init.el)
;;; init.el ends here
