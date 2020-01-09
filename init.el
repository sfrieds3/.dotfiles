;;;; package --- summary
;;; Commentary:
;;;     place 'local-settings.el' file (provide 'local-settings)
;;;     in .emacs.d directory to overwrite settings (loaded at end)

;;; Code:
;;;; GENERAL PACKAGE SETTINGS

(eval-when-compile
  (add-to-list 'load-path "~/.emacs.d/elpa")
  (add-to-list 'load-path "~/.emacs.d/elisp"))

;; backup settings
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; temp file for custom settings
(setq custom-file (make-temp-file "emacs-custom"))

(let ((home-settings (expand-file-name "home.el" user-emacs-directory)))
  (when (file-exists-p home-settings)
    (load-file home-settings)))

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

(load-theme 'wombat t)

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
(setq recentf-max-saved-items 50)

;; filename in titlebar
(setq frame-title-format '((:eval (if (buffer-file-name)
                                      (abbreviate-file-name (buffer-file-name)) "%b"))))

;; C-x w h [REGEX] <RET> <RET> to highlight all occurances of [REGEX], and C-x w r [REGEX] <RET> to unhighlight them again.
(global-hi-lock-mode 1)

;; ////////////////////////////////////////////////////////////

;; PERSONAL FUNCTIONS

;; ////////////////////////////////////////////////////////////

;; grep in current directory
(defun my-dir-grep ()
  "Run grep recursively from the directory of the current buffer or the default directory."
  (interactive)
  (let ((dir (file-name-directory (or load-file-name buffer-file-name default-directory))))
    (let ((command (read-from-minibuffer "Run grep (like this): "
                                         (cons (concat "grep --color --null -nH -ir -e  " dir) 32))))
      (grep command))))

;; grep in current file
(defun my-file-grep ()
  "Run grep in the current file."
  (interactive)
  (let ((fname (buffer-file-name)))
    (let ((command (read-from-minibuffer "Run grep (like this): "
                                         (cons (concat "grep --color --null -nH -ir -e  " fname) 32))))
      (grep command))))

;; revert file without prompt
(defun my-revert-buffer-noconfirm ()
  "Call `revert-buffer' with the NOCONFIRM argument set."
  (interactive)
  (revert-buffer nil t))

(defun my-ido-open-recentf ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

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

;; ////////////////////////////////////////////////////////////

;; PACKAGES

;; ////////////////////////////////////////////////////////////

;; dumb jump- attempts to search for source like IDE
;;(require 'dumb-jump)

;; indent-guide
;;(require 'indent-guide)
;;(indent-guide-global-mode)

;; which-key
(require 'which-key)
(which-key-mode t)
(which-key-setup-side-window-bottom)

;; avy - go to characters
(require 'avy)

;; drag stuff mode (M-<arrow> to move lines of text)
;;(require 'drag-stuff)
;;(drag-stuff-global-mode t)

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
(global-set-key (kbd "C-c g d") 'my-dir-grep)
(global-set-key (kbd "C-c g s") 'my-file-grep)
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
(global-set-key (kbd "C-x f") 'my-ido-open-recentf)
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
;; string insert region
(global-set-key (kbd "C-c I") 'string-insert-rectangle)
;; remove whitespace
(global-set-key (kbd "C-c W") 'delete-trailing-whitespace))
;; read only mode
(global-set-key (kbd "C-c SPC") 'read-only-mode)

;; ////////////////////////////////////////////////////////////

;; platform specific files

;; ////////////////////////////////////////////////////////////

(let ((local-settings (expand-file-name "local-settings.el" user-emacs-directory)))
  (when (file-exists-p local-settings)
    (load-file local-settings)))


;; ////////////////////////////////////////////////////////////

(provide 'init.el)
;;; init.el ends here
