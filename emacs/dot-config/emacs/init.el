;;; init.el --- scwfri init.el -*- lexical-binding: t -*-
;;; Commentary:
;;     place 'local-settings.el' file (provide 'local-settings)
;;     in .emacs.d directory to overwrite settings (loaded at end)

;;; Code:

;;; bootstrap elpaca
(defvar elpaca-installer-version 0.8)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))
(elpaca-wait)

;;; https://adamsimpson.net/writing/on-diminishing-modes-in-emacs
(use-package diminish)
(elpaca-wait)

;;; do not add -hook suffix automatically in use-package :hook
(setq use-package-hook-name-suffix nil)
(setq use-package-verbose nil)
(setq use-package-compute-statistics nil)  ; `use-package-statistics` to see load times

;;; add everything in lisp/ dir to load path
(let ((default-directory  (expand-file-name "lisp" user-emacs-directory)))
  (when (file-exists-p default-directory)
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path)))

;;; add everything in site-lisp/ dir to load path
(let ((default-directory  (expand-file-name "site-lisp" user-emacs-directory)))
  (when (file-exists-p default-directory)
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path)))

;;; home.el
(let ((home-settings (expand-file-name "home.el" user-emacs-directory)))
  (when (file-exists-p home-settings)
    (load-file home-settings)))

;;; shared config not in init.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;;; set font
(set-frame-font "Berkeley Mono 14" nil t)

;;; make scrolling more logical
(setq scroll-conservatively 25)
(setq auto-window-vscroll nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)

;; cursor blinks n times
(setq blink-cursor-blinks 0)

;;; inhibit visual bells
(setq visible-bell nil)
(setq ring-bell-function #'ignore)

;;; transient-mark-mode
(use-package transient)
(setq transient-mark-mode t)

;;; spaces by default instead of tabs!
(setq-default indent-tabs-mode nil)

;;; ask about adding a final newline
(setq require-final-newline 'ask)

;;; allow disabled emacs commands (mainly for narrowing)
(setq disabled-command-function nil)

;;; do not ask about opening large files
(setq large-file-warning-threshold nil)

;;; show garbage collection messages in minbuffer
(setq garbage-collection-messages nil)

;;; disable insert keys automatically for read-only buffers
(setq view-read-only t)

;;; filename in titlebar
(setq frame-title-format
      (concat user-login-name "@" system-name ":%f"))

;;; https://irreal.org/blog/?p=10824
(defvar dictionary-server "dict.org")

;;; sayonara
(global-so-long-mode t)

;;; pretty symbols
(prettify-symbols-mode nil)

(use-package my-defun
  :ensure nil
  :demand t
  :config
  ;; remap some commands to use transient-mark-mode
  (defvar +sf/remap-commands '(mark-word
                            mark-sexp
                            mark-paragraph
                            mark-defun
                            mark-page
                            mark-whole-buffer
                            rectangle-mark-mode))
  (mapc (lambda (command) (+sf/remap-mark-command command))
        +sf/remap-commands)
  (defun +sf/activate-mark ()
    (interactive)
    (activate-mark))
  (with-eval-after-load 'org
    (+sf/remap-mark-command 'org-mark-element org-mode-map)
    (+sf/remap-mark-command 'org-mark-subtree org-mode-map))
  :bind
  (([(control h) (y)] . #'describe-personal-keybindings)
   ([(meta i)] . #'+sf/activate-mark)
   ([(meta shift i)] . #'tab-to-tab-stop)
   (:map isearch-mode-map
         ([(control q)] . #'+sf/isearch-highlight-phrase))))

(use-package my-config
  :ensure nil)

(use-package modeline
  :ensure nil)

;;; Editor settings
(use-package editor
  :ensure nil
  :demand t)

(use-package editor-format
  :ensure nil
  :demand)

(use-package material-theme)
(use-package ef-themes)
(use-package standard-themes
  :config
  (load-theme 'standard-dark t))
(use-package catppuccin-theme
  :config
  (custom-set-faces
   `(diff-hl-change ((t (:background unspecified :foreground ,(catppuccin-get-color 'blue))))))

  (custom-set-faces
   `(diff-hl-delete ((t (:background unspecified :foreground ,(catppuccin-get-color 'red))))))

  (custom-set-faces
   `(diff-hl-insert ((t (:background unspecified :foreground ,(catppuccin-get-color 'green)))))))

(use-package all-the-icons)

;;; match env to shell
(use-package exec-path-from-shell
  :config
  (dolist (var '("XDG_CONFIG_HOME" "XDG_DATA_HOME" "XDG_CACHE_HOME" "JAVA_HOME"))
    (add-to-list 'exec-path-from-shell-variables var))
  (add-to-list 'exec-path (substitute-in-file-name "$XDG_DATA_HOME/nvim/mason/bin"))
  (exec-path-from-shell-initialize)
  (setenv "PATH" (concat (getenv "PATH") ":/home/user/.local/share/mise/shims"))
  (setq exec-path (append exec-path '("/home/user/.local/share/mise/shims"))))

;;; auto-revert everything
(use-package autorevert
  :ensure nil
  :commands (global-auto-revert-mode)
  :init
  (global-auto-revert-mode 1)
  :custom
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)
  :hook
  (auto-save-hook . +sf/diff-hl--update))

(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-alh")
  (dired-dwim-target t))

(use-package dired-x
  :ensure nil
  :config
  (setq dired-omit-files "\\.DS_Store$\\|__pycache__$\\|.pytest_cache$\\|\\.mypy_cache$\\|\\.egg-info$"))

(use-package dired-git-info
  :defer t
  :after dired
  :commands (dired-git-info-mode dired-git-info-auto-enable)
  :bind (:map dired-mode-map
         (")" . #'dired-git-info-mode)))

;;; undo stuff
(use-package undo-fu
  :custom
  (undo-limit 6710886400)
  (undo-strong-limit 100663296)
  (undo-outer-limit 1006632960))

(use-package undo-fu-session
  :custom
  (undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  :init
  (undo-fu-session-global-mode))
(use-package vundo
  :commands vundo)

(use-package breadcrumb
  :config
  (breadcrumb-mode))

;; ;;; eglot
;; (use-package eglot
;;   :straight (:type built-in)
;;   :disabled t
;;   :commands
;;   eglot
;;   :config
;;   (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
;;   (defun $eglot-current-server ()
;;     (interactive)
;;     (print (process-command (jsonrpc--process (eglot-current-server)))))
;;   :bind (([f12] . eglot)
;;          :map eglot-mode-map
;;          ("C-c l a" . #'eglot-code-actions)
;;          ("C-c l r" . #'eglot-rename)
;;          ("C-c l f" . #'eglot-format)
;;          ("C-c l d" . #'eldoc))
;;   :custom
;;   (read-process-output-max (* 1024 1024))
;;   (eglot-events-buffer-size 0)
;;   (completion-category-overrides '((eglot (styles orderless))))
;;   :hook
;;   (python-mode-hook . eglot-ensure))

(use-package spinner)
(use-package lv)

;;; treesitter
(use-package treesit
  :ensure nil)

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package posframe-plus
  :ensure (:host github :type git :repo "zbelial/posframe-plus" ))

(use-package treesitter-context
  :after (treesit posframe-plus)
  :ensure (:type git :host github :repo "zbelial/treesitter-context.el")
  :commands (treesitter-context-toggle-show)
  :config
  (setq treesitter-context-idle-time 0.5)
  (setq treesitter-context-show-context-always t)
  (setq treesitter-context-frame-autohide-timeout 15)

  (require 'treesitter-context-utils) ;; for `treesitter-context-toggle-show'
  :init
  (global-set-key (kbd "M-r") #'treesitter-context-toggle-show))

;;; avy
(use-package avy
  :bind (("C-:" . #'avy-goto-char)
         ("C-'" . #'avy-goto-char-2)
         ("M-g w" . #'avy-goto-word-1)))

;;; hideshow
(use-package hideshow
  :ensure nil
  :bind ("C-c h" . #'hs-toggle-hiding)
  :commands hs-toggle-hiding
  :config
  (diminish 'hs-minor-mode)
  :hook
  (prog-mode-hook . hs-minor-mode))

;;; org-defun
(use-package org-defun
  :ensure nil
  :bind (("s-SPC" . #'$org-table--mark-field)))

;;; org-mode
(use-package org
  :ensure nil

  :commands (org-mode
             org-capture)
  :defer t
  :bind (("C-c N" . #'org-store-link)
         ("C-c a" . #'org-agenda)
         ("C-c c" . #'org-capture)
         ("C-c I" . #'org-id-copy))
  :config
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
  :custom
  (org-hide-leading-stars t)
  (org-src-preserve-indentation nil)
  (org-edit-src-content-indentation 0)
  (org-log-done t)
  (org-startup-folded t)
  (org-agenda-files '("~/wiki/org"))
  (org-agenda-text-search-extra-files (directory-files-recursively "~/wiki" "*.md|*.org"))
  (org-todo-keywords
   '((sequence "TODO(t)" "STRT(s!)" "WAIT(w@/!)" "|" "DONE(d!)" "CNCL(c@)")
     (sequence "NEW(n)" "WORK(k!)" "PUSH-DEV(p!)" "REOPENED(r@/!)" "|" "STAGED(S!)" "RELEASED(R!)" "WONTFIX(w@)")))
  :hook
  (org-mode-hook . org-indent-mode))

;;; org-table
(use-package org-table
  :ensure nil
  :after org)

(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :commands (dired-sidebar-toggle-sidebar +sidebar--toggle)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (defun +sidebar--toggle ()
    "Toggle both `dired-sidebar' and `ibuffer-sidebar'."
    (interactive)
    (ibuffer-sidebar-toggle-sidebar)
    (dired-sidebar-toggle-sidebar))
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
  (setq dired-sidebar-subtree-line-prefix "|")
  (setq dired-sidebar-theme 'nerd)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))

(use-package ibuffer-sidebar
  :commands (ibuffer-sidebar-toggle-sidebar))

(use-package rg
  :commands (rg)
  :init
  (rg-enable-default-bindings)
  (rg-enable-menu))

;;; project
(use-package project
  :ensure nil
  :bind (("C-c f" . #'project-find-file)))

;;; marginalia
(use-package marginalia
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind
  ((:map minibuffer-local-map
         ("C-M-a" . #'marginalia-cycle))
   (:map completion-list-mode-map
         ("C-M-a" . #'marginalia-cycle)))
  :init
  (marginalia-mode))

(use-package minibuffer
  :ensure nil
  :config
  ;;; Add prompt indicator to `completing-read-multiple'.
  ;; (defun $crm-indicator (args)
  ;;   "Add indicator when in CRM prompt and reading ARGS."
  ;;   (cons (concat "[CRM] " (car args) crm-separator) (cdr args)))
  (defun $crm-indicator (args)
    "Add indicator when in CRM prompt and reading ARGS."
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'$crm-indicator)

  ;;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  :init
  (minibuffer-depth-indicate-mode)
  (minibuffer-electric-default-mode)
  :custom
  (max-mini-window-height 0.33)
  (crm-separator ",")
  (resize-mini-windows 'grow-only)
  (enable-recursive-minibuffers t)
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t))

(use-package vertico
  :custom
  (vertico-cycle t)
  (vertico-resize nil)
  (vertico-count 20)
  :init
  (vertico-mode)
  (vertico-multiform-mode)
  :bind (("C-c -" . #'vertico-suspend)
         :map vertico-map
         ("M-q" . #'vertico-multiform-vertical)
         ("M-g" . #'vertico-multiform-grid)
         ("M-f" . #'vertico-multiform-flat)
         ("M-r" . #'vertico-multiform-reverse)
         ("M-u" . #'vertico-multiform-unobtrusive)
         ("<S-backspace>" . #'vertico-directory-up)
         ("M-n" . #'vertico-next-group)
         ("M-p" . #'vertico-previous-group))
  :config
  ;; prefix cutrent candidate with "» "
  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index _start)
                (setq cand (funcall orig cand prefix suffix index _start))
                (concat
                 (if (= vertico--index index)
                     (propertize "» " 'face 'vertico-current)
                   "  ")
                 cand)))
  ;; custom colors for files and dir
  (defvar +vertico-transform-functions nil)

  (defun +vertico-transform (args)
    (dolist (fun (ensure-list +vertico-transform-functions) args)
      (setcar args (funcall fun (car args)))))

  (advice-add #'vertico--format-candidate :filter-args #'+vertico-transform)

  (defun +vertico-highlight-directory (file)
    "Highlight FILE if it ends with a slash."
    (if (string-suffix-p "/" file)
        (propertize file 'face 'marginalia-file-priv-dir)
      file))

  (defun sort-directories-first (files)
    "Sort directories first for a list of FILES."
    (setq files (vertico-sort-history-length-alpha files))
    (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
           (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))

  ;; Configure the display per command.
  ;; Use a buffer with indices for imenu
  ;; and a flat (Ido-like) menu for M-x.
  (setq vertico-multiform-commands
        '(("find-file" list
           (vertico-sort-function . sort-directories-first)
           (+vertico-transform-functions . +vertico-highlight-directory))
          (consult-imenu buffer indexed)
          (execute-extended-command list)
          (project-switch-to-buffer unobtrusive)
          (consult-buffer flat)
          (consult-outline buffer ,(lambda (_) (text-scale-set -1)))))

  ;; Configure the display per completion category.
  ;; Use the grid display for files and a buffer
  ;; for the consult-grep commands.
  (setq vertico-multiform-categories
        '((file grid)
          (consult-grep buffer)))

  (defun down-from-outside ()
    "Move to next candidate in minibuffer, even when minibuffer isn't selected."
    (interactive)
    (with-selected-window (active-minibuffer-window)
      (execute-kbd-macro [down])))

  (defun up-from-outside ()
    "Move to previous candidate in minibuffer, even when minibuffer isn't selected."
    (interactive)
    (with-selected-window (active-minibuffer-window)
      (execute-kbd-macro [up])))

  (defun to-and-fro-minibuffer ()
    "Go back and forth between minibuffer and other window."
    (interactive)
    (if (window-minibuffer-p (selected-window))
        (select-window (minibuffer-selected-window))
      (select-window (active-minibuffer-window)))))

(use-package savehist
  :ensure nil
  :custom
  (savehist-save-minibuffer-history t)
  (history-length 20000)
  :config
  (mapc (lambda (ring)
          (add-to-list 'savehist-additional-variables
                       ring))
        '(kill-ring
          search-ring
          regexp-search-ring
          last-kbd-macro
          shell-command-history
          log-edit-comment-ring))
  (savehist-mode t))

(use-package orderless-defun
  :ensure nil)

(use-package orderless
  :after (orderless-defun)
  :custom
  (completion-styles '(orderless basic))
  (orderless-component-separator #'orderless-escapable-split-on-space) ;; allow escaping space with backslash
  (orderless-matching-styles '(orderless-prefixes
                               orderless-flex
                               orderless-regexp))
  (orderless-style-dispatchers '($dispatch:literal
                                 $dispatch:initialism
                                 $dispatch:regexp
                                 $dispatch:flex
                                 $dispatch:without-literal
                                 $dispatch:flex
                                 $dispatch:prefixes))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles . (basic partial-completion orderless)))
     (project-file (styles . (basic substring partial-completion orderless)))
     (imenu (styles . (basic substring orderless)))
     (kill-ring (styles . (basic substring orderless)))
     (consult-location (styles . (basic substring orderless)))))
  :config
  (define-key minibuffer-local-map (kbd "C-l") #'$match-components-literally))

(use-package corfu
  :ensure (:files (:defaults "extensions/*"))
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-quit-at-boundary 'separator)
  :init
  (global-corfu-mode)
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (let ((completion-extra-properties corfu--extra)
          completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input)
                (eq (current-local-map) read-passwd-map))
      ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)
  :bind
  (:map corfu-map
        ([(meta m)] . #'corfu-move-to-minibuffer)
        ([(meta space)] . #'corfu-insert-separator)
        ([(shift return)] . #'corfu-insert)
        ("M-q" . #'corfu-quick-complete)
        ("C-q" . #'corfu-quick-insert)
        ("RET". nil))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode t)
  (corfu-history-mode t))

(use-package nerd-icons)
(use-package nerd-icons-corfu
  :after (corfu nerd-icons)
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))
(use-package nerd-icons-dired
  :hook
  (dired-mode-hook . nerd-icons-dired-mode))

(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("M-+ p" . #'completion-at-point) ;; capf
         ("M-+ t" . #'complete-tag)        ;; etags
         ("M-+ d" . #'cape-dabbrev)        ;; or dabbrev-completion
         ("M-+ h" . #'cape-history)
         ("M-+ f" . #'cape-file)
         ("M-+ k" . #'cape-keyword)
         ("M-+ s" . #'cape-symbol)
         ("M-+ a" . #'cape-abbrev)
         ("M-+ i" . #'cape-ispell)
         ("M-+ l" . #'cape-line)
         ("M-+ w" . #'cape-dict)
         ("M-+ \\" . #'cape-tex)
         ("M-+ _" . #'cape-tex)
         ("M-+ ^" . #'cape-tex)
         ("M-+ &" . #'cape-sgml)
         ("M-+ r" . #'cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )

;;; fzf.. in emacs!
(use-package fzf
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        fzf/grep-command "rg --no-heading -nH"
        ;; If nil, the fzf buffer will appear at the top of the window
        fzf/position-bottom t
        fzf/window-height 33))

(use-package consult
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c H" . #'consult-history)
         ("C-c RET" . #'consult-mode-command)
         ("C-c k" . #'consult-kmacro)
         ("C-c m" . #'consult-man)
         ("C-c i" . #'consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . #'consult-complex-command)     ;; orig. repeat-complex-command
         ;; ("C-x b" . #'consult-buffer)                ;; orig. switch-to-buffer
         ("C-x f" . #'consult-recent-file)
         ("C-x 4 b" . #'consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . #'consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . #'consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . #'consult-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . #'consult-register-load)
         ("M-'" . #'consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . #'M-register)
         ;; Other custom bindings
         ("M-y" . #'consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings (goto-map)
         ("M-g e" . #'consult-compile-error)
         ("M-g f" . #'consult-flycheck)               ;; Alternative: consult-flycheck
         ("M-g g" . #'consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . #'consult-goto-line)           ;; orig. goto-line
         ("M-g o" . #'consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . #'consult-mark)
         ("M-g k" . #'consult-global-mark)
         ("M-g i" . #'consult-imenu)
         ("M-g I" . #'consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . #'consult-find)
         ("M-s D" . #'consult-locate)
         ("M-s g" . #'consult-grep)
         ("M-s G" . #'consult-git-grep)
         ("M-s r" . #'consult-ripgrep)
         ("M-s l" . #'consult-line)
         ("M-s L" . #'consult-line-multi)
         ("M-s k" . #'consult-keep-lines)
         ("M-s u" . #'consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . #'consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . #'consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . #'consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . #'consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . #'consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . #'consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . #'consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode-hook . consult-preview-at-point-mode)

  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :custom
  (consult-async-min-input 1)
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.5 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key "M-.")

  (setq consult-narrow-key "<")
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help))

(use-package consult-flycheck
  :after (consult flycheck)
  :bind
  (:map flycheck-command-map
        ("!" . #'consult-flycheck)))
(use-package consult-dir
  :bind
  (("C-x C-d" . #'consult-dir)
   (:map minibuffer-local-completion-map
   ("C-x C-d" . #'consult-dir)
   ("C-x C-j" . #'consult-dir-jump-file))))

(use-package deadgrep
  :bind ("<f6>" . #'deadgrep))

(use-package embark-defun
  :ensure nil)

(use-package embark
  :after (embark-defun)
  :bind
  (("C-," . #'embark-act)
   ("M-," . #'$embark-act-noquit)
   ("C-M-," . #'embark-dwim))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :custom
  (embark-indicators '($embark-which-key-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  (embark-action-indicator (lambda (map)
                             (which-key--show-keymap "Embark" map nil nil 'no-paging)
                             #'which-key--hide-popup-ignore-command)
                           embark-become-indicator embark-action-indicator)
  :config
  (advice-add #'embark-completing-read-prompter
              :around #'$embark-hide-which-key-indicator)
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-this-buffer
  :ensure nil
  :after (embark-consult))

(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode-hook . consult-preview-at-point-mode))

(use-package vterm
  :demand t
  :ensure (:pre-build (setq vterm-always-compile-module t))
  :config
  (add-to-list 'vterm-eval-cmds '("update-pwd" (lambda (path) (setq default-directory path))))
  :custom
  (vterm-shell "zsh")
  (vterm-max-scrollback 100000)
  (vterm-buffer-name-string "*vterm* %s")
  :hook
  (vterm-mode-hook . $vterm-mode-hook)
  :bind (:map vterm-mode-map
              ([(control q)] . #'vterm-send-next-key)))

(use-package multi-vterm
  :after vterm
  :custom
  (multi-vterm-buffer-name vterm-buffer-name-string)
  :commands (multi-vterm
             multi-vterm-project
             multi-vterm-dedicated-open
             multi-vterm-dedicated-select
             mult-vterm-dedicated-toggle))
(use-package vterm-toggle
  :after vterm
  :bind (([f2] . #'vterm-toggle)
         ([control f2] . #'vterm-toggle-cd)
         :map vterm-mode-map
         ([f2] . #'vterm-toggle)
         ([(control return)] . #'vterm-toggle-insert-cd)))

(use-package eat)

;;; ansi-term
(use-package term
  :ensure nil
  :config
  (defun $ansi-term-dwim (arg)
    "Launch or switch to ansi-term.
Switch to ansi-term buffer if already open,
else start ansi-term using /bin/bash binary.
If already in ansi-term buffer, switch to previous buffer.
When called with prefix arg, create a new ansi-term
no matter what."
    (interactive "P")
    (let ((abuf "*ansi-term*"))
      (cond
       (arg
        (ansi-term "zsh"))
       ((string= abuf (buffer-name))
        (previous-buffer))
       ((try-completion abuf (mapcar #'buffer-name (buffer-list)))
        (switch-to-buffer abuf))
       (t (ansi-term "zsh"))))))

(use-package shell
  :ensure nil
  :custom
  (explicit-shell-file-name "/opt/homebrew/bin/zsh"))

;; magit
(use-package magit
  :commands (magit
             magit-status
             magit-diff-dwim
             magit-blame
             magit-diff-range)
  :bind
  (("C-x g" . #'magit-status)
   ("C-c g d" . #'magit-diff-range)
   ("C-c g b" . #'magit-blame))
  :config
  (add-to-list 'display-buffer-alist
               '("magit.*"
                 (display-buffer-at-bottom)
                 (window-height . 0.4))))

(use-package git-messenger
  :commands (git-messenger:popup-message
             git-messenger:popup-diff
             git-messenger:popup-show
             git-messenger:popup-close
             git-messenger:show-parent
             git-messenger:copy-message
             git-messenger:copy-commit-id
             git-messenger:popup-show-verbose))

(use-package blamer
  :ensure (:host github :repo "artawower/blamer.el")
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                    :background unspecified
                    :height 140
                    :italic t))))

(use-package diff-hl
  :diminish
  :config
  (global-diff-hl-mode)
  (diff-hl-margin-mode)
  (diff-hl-margin-mode)
  (diff-hl-show-hunk-mouse-mode))

(use-package diff-hl-flydiff
  :ensure nil
  :commands diff-hl-flydiff-mode)

(use-package git-timemachine)

;;; compile
(use-package compile
  :ensure nil
  :config
  (add-to-list 'compilation-error-regexp-alist-alist
               '(pyright "^[[:blank:]]+\\(.+\\):\\([0-9]+\\):\\([0-9]+\\).*$" 1 2 3))
  (add-to-list 'compilation-error-regexp-alist 'pyright)

  (defun $python-compile-hook ()
    (set (make-local-variable 'compile-command)
         (format "pytest %s" (buffer-name))))

  (defun $perl-compile-hook ()
    (set (make-local-variable 'compile-command)
         (format "perl -c %s" (buffer-name))))
  :hook
  ;; (python-mode-hook . $python-compile-hook)
  (perl-mode-hook . $perl-compile-hook)
  (cperl-mode-hook . $perl-compile-hook)
  :bind ("<f5>" . #'recompile))

;;; show matching parens
(use-package paren
  :ensure nil
  :config
  (show-paren-mode 1))

;;; smart parens
(use-package elec-pair
  :ensure nil
  :commands (electric-pair-mode))

;;; highlight current line
(use-package hl-line
  :ensure nil
  :config
  (set-face-attribute hl-line-face nil :underline nil)
  :bind (([f9]. #'hl-line-mode)))

;;; expand-region
(use-package expand-region
  :bind (("C-=" . #'er/expand-region)))

;;; nXml-mode
(use-package nxml-mode
  :ensure nil
  :defer t
  :config
  (defun $pretty-xml ()
    (interactive)
    (save-excursion
      (mark-whole-buffer)
      (shell-command-on-region (point-min) (point-max) "xmllint --format -" (buffer-name) t)
      (nxml-mode)
      (indent-region begin end))))

;;; flycheck
(use-package flycheck
  :commands (flycheck-mode
             global-flycheck-mode)
  :custom
  (flycheck-standard-error-navigation nil)
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-python-pycompile-executable "python3")
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  :init
  (global-flycheck-mode t))

;;; slime
(use-package sly
  :commands (sly)
  :config
  (with-eval-after-load 'sly
    (let ((map sly-editing-mode-map))
      (define-key map [remap display-local-help] #'sly-describe-symbol)
      (define-key map [remap embark-pp-eval-defun] #'sly-compile-defun)
      (define-key map [remap pp-macroexpand-expression] #'sly-expand-1)
      (define-key map [remap pp-eval-expression] #'sly-interactive-eval)
      (define-key map [remap xref-find-definitions] #'sly-edit-definition)))
  :custom
  (inferior-lisp-program "/usr/bin/sbcl"))

;;; eldoc
(use-package eldoc
  :diminish
  :ensure nil)

(use-package eldoc-box
  :diminish((eldoc-box-hover-mode . "")
             (eldoc-box-hover-at-point-mode . ""))
  :commands (eldoc-box-hover-mode
             eldoc-box-helpful-callable
             eldoc-box-helpful-variable
             eldoc-box-helpful-key)
  :custom
  (eldoc-box-cleanup-interval 0.2)
  (eldoc-box-clear-with-C-g t)
  (eldoc-box-doc-fold-threshold 200)
  :config
  (set-face-attribute 'eldoc-box-body nil :family "Hack"))

;;; uniquify
(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'forward)
  (uniquify-separator "/")
  ;; rename after killing unqualified
  (uniquify-after-kill-buffer-p t)
  ;; dont change names of special buffers
  (uniquify-ignore-buffers-re "^\\*"))

(use-package ace-window
  :bind (("M-o" . #'ace-window)))

;;; display-buffer (most/all of this taken from prot)
(use-package window
  :ensure nil
  :custom
  (display-buffer-alist
   '(;; top side window
     ("\\*\\(Flymake\\|Package-Lint\\|vc-git :\\).*"
      (display-buffer-in-side-window)
      (window-height . 0.16)
      (side . top)
      (slot . 0)
      (window-parameters . ((no-other-window . t))))
     ("\\*Messages.*"
      (display-buffer-in-side-window)
      (window-height . 0.16)
      (side . top)
      (slot . 1)
      (window-parameters . ((no-other-window . t))))
     ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.16)
      (side . top)
      (slot . 2)
      (window-parameters . ((no-other-window . t))))
     ;; bottom side window
     ("\\*\\(Embark\\)?.*Completions.*"
      (display-buffer-in-side-window)
      (side . bottom)
      (slot . 0)
      (window-parameters . ((no-other-window . t)
                            (mode-line-format . none))))
     ;; left side window
     ("\\*Help.*"
      (display-buffer-at-bottom)
      (window-width . 0.25)
      (side . left)
      (slot . 0))
     ;; right side window
     ("\\*Faces\\*"
      (display-buffer-in-side-window)
      (window-width . 0.25)
      (side . right)
      (slot . 0)
      (window-parameters
       . ((mode-line-format
           . (" "
              mode-line-buffer-identification)))))
     ("\\*Custom.*"
      (display-buffer-in-side-window)
      (window-width . 0.3)
      (side . right)
      (slot . 1)
      (window-parameters . ((no-other-window . t))))
     ("\\*Embark Actions\\*"
      (display-buffer-reuse-mode-window display-buffer-below-selected)
      (window-height . fit-window-to-buffer)
      (window-parameters . ((no-other-window . t)
                            (mode-line-format . none))))
     ;; bottom buffer (NOT side window)
     ("\\*vc-\\(incoming\\|outgoing\\).*"
      (display-buffer-at-bottom))
     ("\\*\\(Output\\|Register Preview\\).*"
      (display-buffer-at-bottom)
      (window-parameters . ((no-other-window . t))))
     ("\\*.*\\([^E]eshell\\|shell\\|v?term\\|xref\\|compilation\\|Occur\\).*"
      (display-buffer-reuse-mode-window display-buffer-at-bottom)
      (window-height . 0.25))))
  (window-combination-resize t)
  (even-window-sizes 'height-only)
  (window-sides-vertical nil)
  (switch-to-buffer-in-dedicated-window 'pop)
  :hook
  (help-mode-hook . visual-line-mode))

;;; winner-mode
(use-package winner
  ;; TODO: set C-w [ and ] to winner-undo and winner-redo
  :ensure nil
  :hook
  (after-init-hook . winner-mode))

;;; tab-bar (again, most/all of this taken from prot)
(use-package tab-bar
  :ensure nil
  :custom
  (tab-bar-close-button-show nil)
  (tab-bar-close-last-tab-choice 'tab-bar-mode-disable)
  (tab-bar-close-tab-select 'recent)
  (tab-bar-new-tab-choice t)
  (tab-bar-new-tab-to 'right)
  (tab-bar-position nil)
  (tab-bar-show nil)
  (tab-bar-tab-hints t)
  (tab-bar-tab-name-function 'tab-bar-tab-name-current)

  :config
  (tab-bar-mode -1)
  (tab-bar-history-mode 1)
  (defun $tab--tab-bar-tabs ()
    "Return a list of `tab-bar' tabs, minus the current one."
    (mapcar (lambda (tab)
              (alist-get 'name tab))
            (tab-bar--tabs-recent)))

  (defun $tab-select-tab-dwim ()
    "Do-What-I-Mean function for getting to a `tab-bar' tab.
If no other tab exists, create one and switch to it.  If there is
one other tab (so two in total) switch to it without further
questions.  Else use completion to select the tab to switch to."
    (interactive)
    (let ((tabs ($tab--tab-bar-tabs)))
      (cond ((eq tabs nil)
             (tab-new))
            ((eq (length tabs) 1)
             (tab-next))
            (t
             (tab-bar-switch-to-tab
              (completing-read "Select tab: " tabs nil t))))))

  (defun $tab-tab-bar-toggle ()
    "Toggle `tab-bar' presentation."
    (interactive)
    (if (bound-and-true-p tab-bar-mode)
        (progn
          (setq tab-bar-show nil)
          (tab-bar-mode -1))
      (setq tab-bar-show t)
      (tab-bar-mode 1)))

  :bind (("C-x T h" . #'tab-bar-history-forward)
         ("C-x T l" . #'tab-bar-history-back)
         ("C-x T n" . #'tab-next)
         ("C-x T p" . #'tab-previous)
         ("<f8>" . #'$tab-tab-bar-toggle)
         ("C-x T k" . #'tab-close)
         ("C-x T c" . #'tab-new)
         ("C-x T t" . #'$tab-select-tab-dwim)
         ("s-t" . #'$tab-select-tab-dwim)))

;; which-key
(use-package which-key
  :ensure nil
  :diminish
  :config
  (which-key-mode))

(use-package symbol-overlay
  :bind (("C-c j" . #'symbol-overlay-jump-next)
         ("C-c k" . #'symbol-overlay-jump-prev))
  ([f7] . #'symbol-overlay-put)
  ([(control shift f7)] . #'symbol-overlay-remove-all))

;;; hi-lock
(use-package hi-lock
  :ensure nil
  :config
  ;; make hl-lock play nice with idle-highlight-mode
  (defun $enable-idle-highlight-mode ()
    (setq idle-highlight-mode t))
  (defun $disable-idle-highlight-mode ()
    (setq idle-highlight-mode nil))
  ;;(advice-add 'highlight-symbol-at-point :before '$disable-idle-highlight-mode)
  ;;(advice-add 'highlight-symbol-at-point :after '$enable-idle-highlight-mode)
  ;; C-x w h [REGEX] <RET> <RET> to highlight all occurances of [REGEX], and C-x w r [REGEX] <RET> to unhighlight them again.
  (global-hi-lock-mode 1))

;;; hl-todo
(use-package hl-todo
  :hook (prog-mode-hook . hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
        '(("TODO"   . "#FFFF00")
          ("FIXME"  . "#FFFFFF")
          ("NOTE"   . "#F56600")
          ("WORK"   . "#522D80")))
  :bind
  (:map hl-todo-mode-map
        ("C-c t p" . #'hl-todo-previous)
        ("C-c t n" . #'hl-todo-next)
        ("C-c t o" . #'hl-todo-occur)
        ("C-c t i" . #'hl-todo-insert)))

;;; helpful
(use-package helpful
  :bind
  (("C-h f" . #'helpful-callable)
   ("C-h v" . #'helpful-variable)
   ("C-h k" . #'helpful-key)))

(use-package visual-regexp)
(use-package visual-regexp-steroids)

;;; no-littering
(use-package no-littering
  :config
  (recentf-mode t)
  (add-hook 'buffer-list-update-hook 'recentf-track-opened-file) ; add buffer visits to recentf list
  (add-to-list 'recentf-exclude "*/.ido.last")
  (add-to-list 'recentf-exclude "*/TAGS")
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  ;; backup settings
  :custom
  (recentf-max-saved-items 1000)
  (delete-old-versions nil)
  (version-control t)
  (auto-save-default nil) ; no autosave files
  (create-lockfiles nil) ; no lockfiles
  (make-backup-files t)
  (vc-make-backup-files t)
  (backup-by-copying t)
  (backup-directory-alist `(("." . ,(expand-file-name "backup" user-emacs-directory))))
  (kept-new-versions 1000)
  (kept-old-versions 1000)
  ;; history settings
  (history-length 1000)
  (history-delete-duplicates t)
  (savehist-save-minibuffer-history 1)
  (savehist-additional-variables
   '(kill-ring
     search-ring
     regexp-search-ring)))

;;; deft
(use-package deft
  :bind ("C-c \\" . #'deft)
  :commands (deft)
  :custom
  (deft-directory "~/wiki")
  (deft-extensions '("md" "org")))

;;; ws-butler
(use-package ws-butler
  :diminish
  :hook
  (prog-mode-hook . ws-butler-mode))

;;; yasnippet
(use-package yasnippet
  :config
  (add-to-list 'yas-snippet-dirs (expand-file-name "snippets" user-emacs-directory))
  (add-to-list 'yas-snippet-dirs (expand-file-name ".local-emacs-snippets" (getenv "HOME")))
  (yas-reload-all)
  (yas-global-mode))

(use-package auto-yasnippet
  :bind (("C-c C-y w" . #'aya-create)
         ("C-c C-y TAB" . #'aya-expand)
         ("C-c C-y SPC" . #'aya-expand-from-history)
         ("C-c C-y d" . #'aya-delete-from-history)
         ("C-c C-y c" . #'aya-clear-history)
         ("C-c C-y n" . #'aya-next-in-history)
         ("C-c C-y p" . #'aya-previous-in-history)
         ("C-c C-y s" . #'aya-persist-snippet)
         ("C-c C-y o" . #'aya-open-line)))

(use-package tempel
  :custom
  (tempel-trigger-prefix "<")
  :bind (("M-=" . #'tempel-complete) ;; Alternative tempel-expand
         ("M-*" . #'tempel-insert)
         ("M-)" . #'tempel-next)
         ("M-(" . #'tempel-previous))
  :init
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))
  :config
  (setq tempel-path `(,(expand-file-name "templates" user-emacs-directory)
                      ,(expand-file-name "templates-local" user-emacs-directory)))
  :hook ((conf-mode-hook . tempel-setup-capf)
         (prog-mode-hook . tempel-setup-capf)
         (text-mode-hook . tempel-setup-capf)))

(use-package tempel-collection
  :ensure (:host github :repo "Crandel/tempel-collection"))

;;; load local settings
(let ((local-settings (expand-file-name "local-settings.el" user-emacs-directory)))
  (when (file-exists-p local-settings)
    (load-file local-settings)))

(message "loaded init.el!")

(provide 'init.el)
;;; init.el ends here
