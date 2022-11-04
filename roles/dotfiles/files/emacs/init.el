;;; init.el --- scwfri init.el -*- lexical-binding: t -*-
;;; Commentary:
;;     place 'local-settings.el' file (provide 'local-settings)
;;     in .emacs.d directory to overwrite settings (loaded at end)

;;; Code:

;;; built-in libraries
(require 'cl-lib)
(require 'map)
(require 'subr-x)

;; https://www.reddit.com/r/emacs/comments/mtb05k/emacs_init_time_decreased_65_after_i_realized_the/
(defvar straight-check-for-modifications '(check-on-save find-when-checking))

;;; bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; use straight.el with use-package
(straight-use-package 'use-package)
(use-package straight
  :config
  (defun $straight-update-all ()
    (interactive)
    (straight-pull-all)
    (straight-check-all)
    (straight-prune-build))
  :custom (straight-use-package-by-default t))

;;; add everything in lisp/ dir to load path
(let ((default-directory  (expand-file-name "lisp" user-emacs-directory)))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;;; add everything in site-lisp/ dir to load path
(let ((default-directory  (expand-file-name "site-lisp" user-emacs-directory)))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;;; use-package to manage packages
(eval-when-compile
  (require 'use-package)
  ;; do not add -hook suffix automatically in use-package :hook
  (setq use-package-hook-name-suffix nil))

;;; home.el
(let ((home-settings (expand-file-name "home.el" user-emacs-directory)))
  (when (file-exists-p home-settings)
    (load-file home-settings)))

;;; shared config not in init.el
(setq custom-file (expand-file-name "custom.el" temporary-file-directory))

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

;;; debug on error -- off for now
(setq debug-on-error nil)

;;; filename in titlebar
(setq frame-title-format
      (concat user-login-name "@" system-name ":%f"))

;;; https://irreal.org/blog/?p=10824
(defvar dictionary-server "dict.org")

;;; sayonara
(global-so-long-mode t)

;;; pretty symbols
(prettify-symbols-mode nil)

;;; scwfri-defun
(use-package scwfri-defun
  :straight nil
  :demand
  :config
  ;; remap some commands to use transient-mark-mode
  (defvar $remap-commands '(mark-word
                            mark-sexp
                            mark-paragraph
                            mark-defun
                            mark-page
                            mark-whole-buffer
                            rectangle-mark-mode))
  (mapc (lambda (command) ($remap-mark-command command))
        $remap-commands)
  (defun $activate-mark ()
    (interactive)
    (activate-mark))
  (with-eval-after-load 'org
    ($remap-mark-command 'org-mark-element org-mode-map)
    ($remap-mark-command 'org-mark-subtree org-mode-map))
  :bind (([(control h) (y)] . #'describe-personal-keybindings)
         ([(meta i)] . #'$activate-mark)
         ([(meta shift i)] . #'tab-to-tab-stop)
         :map isearch-mode-map
         ([(control q)] . #'$isearch-highlight-phrase)))

;;; theme config
(use-package theme-config
  :straight nil
  :demand
  :config
  ($set-path)
  :custom
  (custom-safe-themes t)
  (fringe-mode 0))

(use-package ef-themes)
(use-package zerodark
  :straight (:host github :repo "NicolasPetton/zerodark-theme")
  :no-require t)
(use-package all-the-icons)
(use-package doom-themes
  :custom
  (doom-themes-treemacs-theme "doom-atom")
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  (doom-modeline-indent-info t)
  (size-indication-mode t)
  (column-number-mode t)
  :config
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(use-package toggle-commands
  :straight nil
  :after (theme-config ef-themes doom-themes)
  :config
  ($cycle-font 0)
  ($cycle-theme 0))

(use-package scwfri-config
  :straight nil)
(use-package modeline
  :straight nil)
;; (use-package keybindings
;;   :straight nil)
(use-package tramp-config
  :straight nil)
(use-package s)

;;; match env to shell
(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

;;; auto-revert everything
(use-package autorevert
  :commands (global-auto-revert-mode)
  :init
  (global-auto-revert-mode 1)
  :custom
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil))

(use-package dired
  :straight (:type built-in)
  :custom
  (dired-listing-switches "-alh")
  (dired-dwim-target t))

(use-package blackout
  :config
  (blackout 'abbrev-mode))

;;; general
(use-package general
  :config
  (general-create-definer $localleader :prefix "_")
  ($localleader
    :keymaps 'normal
    "Ag" #'affe-grep
    "Af" #'affe-find
    "D" #'magit-diff-dwim
    "F" #'format-all-buffer
    "G" #'magit
    "cf" #'$cycle-font
    "ct" #'$cycle-theme
    "e" #'eval-buffer
    "gg" #'global-git-gutter-mode
    "r" #'eglot-rename
    "x" #'eval-last-sexp)
  (general-create-definer $leader :prefix "SPC")
  ($leader
    :keymaps 'normal
    "RET" #'execute-extended-command
    "b" #'consult-buffer
    "d" #'consult-flycheck
    "f" #'projectile-find-file
    "g" #'deadgrep
    "G" #'rg
    "l" #'consult-line
    "P" #'consult-projectile
    "r" #'consult-recent-file
    "t" #'consult-eglot-symbols)
  (general-create-definer $next :prefix "]")
  ($next
    :keymaps 'normal
    "b" #'next-buffer
    "c" #'git-gutter:next-hunk
    "d" #'flycheck-next-error
    "t" #'tab-next)
  (general-create-definer $previous :prefix "[")
  ($previous
    :keymaps 'normal
    "b" #'previous-buffer
    "c" #'git-gutter:previous-hunk
    "d" #'flycheck-previous-error
    "t" #'tab-previous))

;;; evil
(use-package evil-config
  :after (evil)
  :straight nil)

(use-package evil
  :demand t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-disable-insert-state-bindings t)
  :config
  (evil-mode 1)
  (defun $evil-nohl ()
    (progn
      (redraw-frame)
      (evil-ex-nohighlight)))
  :custom
  (evil-undo-system 'undo-fu)
  (evil-want-integration t)
  (evil-want-C-u-scroll t)
  (evil-want-C-i-jump t)
  (evil-want-Y-yank-to-eol t)
  (evil-split-window-below t)
  (evil-split-window-right t)
  (evil-search-module 'evil-search)
  (scroll-margin 3) ; set scrolloff=3
  :bind ((:map evil-normal-state-map
               ([(control l)] . #'evil-ex-nohighlight)
               ([(control j)] . #'evil-next-visual-line)
               ([(control k)] . #'evil-previous-visual-line)
               ([(control return)] . #'eval-last-sexp)
               ("j" . #'evil-next-visual-line)
               ("k" . #'evil-previous-visual-line)
               ("gj" . #'evil-next-line)
               ("gk" . #'evil-previous-line)
               ([(control shift v)] . #'evil-paste-after)
               ([(meta h)] . #'evil-window-left)
               ([(meta j)] . #'evil-window-down)
               ([(meta k)] . #'evil-window-up)
               ([(meta l)] . #'evil-window-right))
         (:map evil-insert-state-map
               ([(control shift c)] . #'evil-yank)
               ([(control shift v)] . #'yank))
         (:map evil-visual-state-map
               ([(control shift c)] . #'evil-yank)
               ([(control shift v)] . #'evil-visual-paste))))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init)
  (blackout 'evil-collection-unimpaired-mode))

(use-package evil-commentary
  :after evil
  :blackout
  :init
  (evil-commentary-mode))

(use-package evil-lion
  :ensure t
  :config
  (evil-lion-mode))

(use-package evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode t))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode t))

(use-package evil-visualstar
  :after evil
  :config
  (global-evil-visualstar-mode)
  :custom
  (evil-visualstar/persist t))

(use-package format-all
  :commands (format-all-buffer)
  :config
  (evil-define-operator evil-format (beg end)
    "Format selected text."
    :move-point nil
    :type line
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (format-all-buffer))))
  (evil-global-set-key 'normal "gq" #'evil-format))

;;; goto-chg
(use-package goto-chg)

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
  (global-undo-fu-session-mode))
(use-package vundo
  :commands vundo)

;;; eglot
(use-package eglot
  :straight (:type built-in)
  :commands
  eglot
  :config
  (defun $eglot-current-server ()
    (interactive)
    (print (process-command (jsonrpc--process (eglot-current-server)))))
  :bind (([f12] . eglot)
         :map eglot-mode-map
         ("C-c l a" . #'eglot-code-actions)
         ("C-c l r" . #'eglot-rename)
         ("C-c l f" . #'eglot-format)
         ("C-c l d" . #'eldoc))
  :custom
  (read-process-output-max (* 1024 1024))
  (eglot-events-buffer-size 0)
  (completion-category-overrides '((eglot (styles orderless)))))

(use-package consult-eglot
  :after eglot)

;;; treesitter
(use-package tree-sitter
  :init
  (global-tree-sitter-mode)
  :config
  (blackout 'tree-sitter-mode)
  :hook
  (tree-sitter-after-on-hook . tree-sitter-hl-mode))
(use-package tree-sitter-langs)

;;; avy
(use-package avy
  :bind
  (:map evil-normal-state-map
        ("s" . #'avy-goto-word-1)))

;;; hideshow
(use-package hideshow
  :bind ("C-c h" . #'hs-toggle-hiding)
  :commands hs-toggle-hiding
  :config
  (blackout 'hs-minor-mode)
  :hook
  (prog-mode-hook . hs-minor-mode))

;;; org-defun
(use-package org-defun
  :straight nil
  :bind (("s-SPC" . #'$org-table--mark-field)))

;;; org-mode
(use-package org
  :straight (:type built-in)
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
  (org-agenda-files '("~/code/org"))
  (org-agenda-text-search-extra-files (directory-files-recursively "~/code" "*.md|*.org"))
  (org-todo-keywords
   '((sequence "TODO(t)" "STRT(s!)" "WAIT(w@/!)" "|" "DONE(d!)" "CNCL(c@)")
     (sequence "NEW(n)" "WORK(k!)" "PUSH-DEV(p!)" "REOPENED(r@/!)" "|" "STAGED(S!)" "RELEASED(R!)" "WONTFIX(w@)")))
  :hook
  (org-mode-hook . org-indent-mode))

(use-package evil-org-mode
  :after org
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (defun $org-mode-hook ()
    (evil-org-mode))
  :hook (org-mode-hook . $org-mode-hook))

;;; org-table
(use-package org-table
  :after org
  :straight nil)

(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . #'treemacs-select-window)
        ("C-x t 1"   . #'treemacs-delete-other-windows)
        ("C-x t t"   . #'treemacs)
        ("C-x t d"   . #'treemacs-select-directory)
        ("C-x t B"   . #'treemacs-bookmark)
        ("C-x t C-t" . #'treemacs-find-file)
        ("C-x t M-t" . #'treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :config (treemacs-set-scope-type 'Tabs))

(use-package rg
  :commands (rg)
  :init
  (rg-enable-default-bindings)
  (rg-enable-menu))

(use-package projectile
  :commands (projectile-mode)
  :init
  (projectile-mode)
  :custom
  (projectile-project-search-path `("~/.dotfiles/" ("~/dev" . 2) ,(expand-file-name "straight/repos" user-emacs-directory)))
  :config
  (setq projectile-tags-command (s-replace-regexp "^ctags" "/usr/bin/ctags" projectile-tags-command))
  :bind (("C-c f" . #'projectile-find-file)
         ("C-c b" . #'projectile-switch-to-buffer)
         :map projectile-mode-map
         ("C-c p" . #'projectile-command-map)))

;;; project
(use-package project
  :config
  (defun $project-override (dir)
    (let ((override (locate-dominating-file dir ".project.el")))
      (if override
          (cons 'vc override)
        nil)))
  :hook
  (project-find-functions . $project-override))

;;; marginalia
(use-package marginalia
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (:map minibuffer-local-map
              ("C-M-a" . #'marginalia-cycle))
  :init
  (marginalia-mode))

(use-package minibuffer
  :straight (:type built-in)
  :config
  ;;; Add prompt indicator to `completing-read-multiple'.
  (defun $crm-indicator (args)
    "Add indicator when in CRM prompt and reading ARGS."
    (cons (concat "[CRM] " (car args) crm-separator) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'$crm-indicator)

  ;;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  :init
  (minibuffer-depth-indicate-mode)
  (minibuffer-electric-default-mode)
  :custom
  (crm-separator ",")
  (resize-mini-windows t)
  (enable-recursive-minibuffers t)
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t))

(use-package vertico
  :custom
  (vertico-cycle t)
  (vertioc-resize t)
  :init
  (vertico-mode)
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

  (setq vertico-multiform-commands
        '(("find-file" flat
           (vertico-sort-function . sort-directories-first)
           (+vertico-transform-functions . +vertico-highlight-directory))))

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
  :init
  (savehist-mode))

(use-package orderless-defun
  :straight nil)

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
   '((eglot (styles . (orderless)))
     (file (styles . (basic partial-completion orderless)))
     (project-file (styles . (basic substring partial-completion orderless)))
     (imenu (styles . (basic substring orderless)))
     (kill-ring (styles . (basic substring orderless)))
     (consult-location (styles . (basic substring orderless)))))
  :config
  (define-key minibuffer-local-map (kbd "C-l") #'$match-components-literally))

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-quit-at-boundary 'separator)
  :config
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (let ((completion-extra-properties corfu--extra)
          completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))
  :bind (:map corfu-map
              ([(meta m)] . #'corfu-move-to-minibuffer)
              ([(meta space)] . #'corfu-insert-separator)
              ([(shift return)] . #'corfu-insert)
              ("RET". nil))
  :init
  (global-corfu-mode))

(use-package corfu-doc
  :after corfu
  :hook
  (corfu-mode-hook . corfu-doc-mode))

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

(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . #'consult-history)
         ("C-c m" . #'consult-mode-command)
         ("C-c k" . #'consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . #'consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . #'consult-buffer)                ;; orig. switch-to-buffer
         ("C-x f" . #'consult-recent-file)
         ("C-x 4 b" . #'consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . #'consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . #'consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . #'consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . #'consult-register-load)
         ("M-'" . #'consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . #'consult-register)
         ;; Other custom bindings
         ("M-y" . #'consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . #'consult-apropos)            ;; orig. apropos-command
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
         ("M-s m" . #'consult-multi-occur)
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
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :commands (consult-register-window
             consult-multi-occur
             consult-register-format)

  :init
  ;; Replace `multi-occur' with `consult-multi-occur', which is a drop-in replacement.
  (fset 'multi-occur #'consult-multi-occur)

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

  :config
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any))
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file consult-line
   :preview-key (kbd "M-."))

  (setq consult-narrow-key (kbd "C-+"))
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; use projectile
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root))))

(use-package consult-yasnippet
  :after (consult yasnippet))
(use-package consult-projectile
  :after (projectile consult)
  :bind ("C-c P" . #'consult-projectile))
(use-package consult-flycheck
  :after (consult flycheck)
  :bind (:map flycheck-command-map
              ("!" . #'consult-flycheck)))
(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . #'consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . #'consult-dir)
         ("C-x C-j" . #'consult-dir-jump-file)))

(use-package affe
  :defer t
  :config
  ;; Manual preview key for `affe-grep'
  (consult-customize affe-grep :preview-key (kbd "M-."))
  (defun affe-orderless-regexp-compiler (input _type _ignorecase)
    (setq input (orderless-pattern-compiler input))
    (cons input (lambda (str) (orderless--highlight input str))))
  :custom
  (affe-regexp-compiler #'affe-orderless-regexp-compiler))

(use-package deadgrep
  :bind ("<f6>" . #'deadgrep))

(use-package embark-defun
  :straight nil)

(use-package embark
  :after (embark-defun)
  :bind (("C-," . #'embark-act)
         ("M-," . #'$embark-act-noquit)
         ("C-M-," . #'embark-dwim)
         :map embark-general-map
         ([(control y)] . #'symbol-overlay-put)
         ([(control g)] . #'projectile-ripgrep)
         ([(control G)] . #'rg))
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
  :straight nil
  :after embark)

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;; ctags
(use-package ctags
  :bind (("s-." . #'ctags-find)))

(use-package vterm
  :demand t
  :config
  (defun $vterm-mode-hook ()
    (setq-local evil-insert-state-cursor 'box)
    (evil-insert-state))
  (add-to-list 'vterm-eval-cmds '("update-pwd" (lambda (path) (setq default-directory path))))
  :custom
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

;;; ansi-term
(use-package term
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
        (ansi-term "/usr/bin/zsh"))
       ((string= abuf (buffer-name))
        (previous-buffer))
       ((try-completion abuf (mapcar #'buffer-name (buffer-list)))
        (switch-to-buffer abuf))
       (t (ansi-term "/bin/bash")))))
  :bind (("C-`" . #'$ansi-term-dwim)))

;; magit
(use-package magit
  :commands (magit
             magit-status
             magit-diff-dwim
             magit-blame
             magit=diff-range)
  :init
  :bind (("C-x g" . #'magit-status)
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
(use-package git-gutter
  :blackout
  :init
  (global-git-gutter-mode t))

;;; compile
(use-package compile
  :config
  (defun $python-compile-hook ()
    (set (make-local-variable 'compile-command)
         (format "pep8 --ignore=E501,E261,E262,E265,E266 --format=pylint %s" (buffer-name))))

  (defun $perl-compile-hook ()
    (set (make-local-variable 'compile-command)
         (format "perl -c %s" (buffer-name))))
  :hook
  (python-mode-hook . $python-compile-hook)
  (perl-mode-hook . $perl-compile-hook)
  (cperl-mode-hook . $perl-compile-hook)
  :bind ("<f5>" . #'recompile))

;;; show matching parens
(use-package paren
  :config
  (show-paren-mode 1))

;;; smart parens
(use-package elec-pair
  :commands (electric-pair-mode)
  :init
  (electric-pair-mode t)
  :config
  (defun $inhibit-electric-pair-mode (char)
    "Do not use smart parens in mini-buffers.  Params: CHAR."
    (minibufferp))
  (setq electric-pair-inhibit-predicate #'$inhibit-electric-pair-mode))

;;; highlight current line
(use-package hl-line
  :config
  (set-face-attribute hl-line-face nil :underline nil)
  :custom
  (global-hl-line-mode nil)
  :bind (([f9]. #'hl-line-mode)))

;;; expand-region
(use-package expand-region
  :bind (("C-=" . #'er/expand-region)))

;;; nXml-mode
(use-package nxml-mode
  :straight (:type built-in)
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
  :init
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
  :blackout
  :straight (:type built-in))

(use-package eldoc-box
  :blackout ((eldoc-box-hover-mode . "")
             (eldoc-box-hover-at-point-mode . ""))
  :commands (eldoc-box-hover-mode
             eldoc-box-eglot-help-at-point
             eldoc-box-helpful-callable
             eldoc-box-helpful-variable
             eldoc-box-helpful-key)
  :init
  (add-hook 'eglot--managed-mode-hook #'eldoc-box-hover-at-point-mode t)
  :custom
  (eldoc-box-cleanup-interval 0.2)
  (eldoc-box-clear-with-C-g t)
  (eldoc-box-doc-fold-threshold 200)
  :config
  (set-face-attribute 'eldoc-box-body nil :family "Hack"))

(use-package elisp-mode
  :straight (:type built-in)
  :blackout ((lisp-interaction-mode . "Lisp-Interaction")
             (emacs-lisp-mode . `("ELisp"
                                  (lexical-binding
                                   ""
                                   (:propertize
                                    "/d" face warning))))))

;;; yaml-mode
(use-package yaml-mode
  :commands (yaml-mode)
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

;;; markdown-mode
(use-package markdown-mode
  :commands (markdown-mode
             gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown"))

;;; python-mode
(use-package python-mode
  :commands (python-mode)
  :custom
  (python-shell-interpreter "python3"))

(use-package pyvenv
  :init
  (setenv "WORKON_HOME" "~/.venv")
  (pyvenv-mode t))

;;; golang
(use-package go-mode)

;; rust
(use-package rust-mode
  :config
  (defun $rust-mode-hook ()
    (setq indent-tabs-mode nil))
  :custom
  (rust-format-on-save t)
  :hook
  (rust-mode-hook . prettify-symbols-mode)
  (rust-mode-hook . $rust-mode-hook))

;; zig
(use-package zig-mode)

;;; cperl-mode
(use-package cperl-mode
  :commands (cperl-mode)
  :init
  (mapc
   (lambda (pair)
     (if (eq (cdr pair) 'perl-mode)
         (setcdr pair 'cperl-mode)))
   (append auto-mode-alist interpreter-mode-alist))
  :custom
  (cperl-invalid-face nil)
  (cperl-highlight-variables-indiscriminately t)
  (cperl-indent-level 4)
  (cperl-close-paren-offset (- cperl-indent-level))
  (cperl-indent-parens-as-block t)
  :config
  (modify-syntax-entry ?: "-" cperl-mode-syntax-table)
  :bind ((:map cperl-mode-map
               ("<tab>" . #'indent-for-tab-command))))

;;; javacsript
(use-package js
  :straight (:type built-in)
  :custom
  (js-indent-level 2))

;;; sh-mode
(use-package sh-script
  :straight (:type built-in)
  :config
  (add-to-list 'auto-mode-alist '("/zsh/" . sh-mode)))

;; typescript
(use-package tide
  :config
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (tide-hl-identifier-mode t))
  :hook
  (before-save-hook . tide-format-before-save)
  (typescript-mode-hook . setup-tide-mode)
  )

;;; ruby-mode
(use-package ruby-mode
  :custom
  (ruby-deep-indent-paren nil))

;;; web-mode
(use-package web-mode
  :defer t
  :config
  (defun $tide-web-mode-hook ()
      (when (string-equal "tsx" (file-name-extension buffer-file-name))
        (setup-tide-mode)))
  :hook
  (web-mode-hook . $tide-web-mode-hook)
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  :mode ("\\.phtml\\'"
         "\\.tpl\\.php\\'"
         "\\.[agj]sp\\'"
         "\\.as[cp]x\\'"
         "\\.erb\\'"
         "\\.tsx\\'"
         "\\.mustache\\'"
         "\\.djhtml\\'"))

;;; c++-mode
(use-package c++-mode
  :straight (:type built-in)
  :commands (c++-mode)
  :custom
  (c-basic-offset 2)
  :config
  (c-set-offset 'substatement-open 0)
  :bind ((:map c-mode-base-map
               ("<tab>" . #'indent-for-tab-command))))

;;; projectile-rails
(use-package projectile-rails
  :mode "\\.rb\\'"
  :interpreter "ruby"
  :commands (projectile-rails-mode
             projectile-rails-command-map)
  :bind (("C-c R" . #'projectile-rails-command-map))
  :config
  (projectile-rails-global-mode)
  :hook
  (ruby-mode-hook . projectile-rails-mode))

;;; uniquify
(use-package uniquify
  :straight (:type built-in)
  :custom
  (uniquify-buffer-name-style 'forward)
  (uniquify-separator "/")
  ;; rename after killing unqualified
  (uniquify-after-kill-buffer-p t)
  ;; dont change names of special buffers
  (uniquify-ignore-buffers-re "^\\*"))

;;; display-buffer (most/all of this taken from prot)
(use-package window
  :straight (:type built-in)
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
     ;; bottom buffer (NOT side window)
     ("\\*\\vc-\\(incoming\\|outgoing\\).*"
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
  (help-mode-hook . visual-line-mode)
  :bind (("M-o" . #'other-window)))

;;; winner-mode
(use-package winner
  :hook
  (after-init-hook . winner-mode)
  :bind(("s-<" . #'winner-undo)
        ("s->" . #'winner-redo)))

;;; tab-bar (again, most/all of this taken from prot)
(use-package tab-bar
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
  :blackout
  :config
  (which-key-mode))

;;; idle-highlight-mode
(use-package idle-highlight-mode
  :custom
  (idle-highlight-idle-time 1.0)
  :bind (([(control f7)] . idle-highlight-mode)))

(use-package symbol-overlay
  :bind (("M-n" . #'symbol-overlay-jump-next)
         ("M-p" . #'symbol-overlay-jump-prev))
  ([f7] . #'symbol-overlay-put)
  ([(control shift f7)] . #'symbol-overlay-remove-all))

;;; hi-lock
(use-package hi-lock
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
  :bind (:map hl-todo-mode-map
              ("C-c t p" . #'hl-todo-previous)
              ("C-c t n" . #'hl-todo-next)
              ("C-c t o" . #'hl-todo-occur)
              ("C-c t i" . #'hl-todo-insert)))

;;; helpful
(use-package helpful
  :bind (("C-h f" . #'helpful-callable)
         ("C-h v" . #'helpful-variable)
         ("C-h k" . #'helpful-key)))

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

;;; tags
(use-package etags
  :custom
  (tags-revert-without-query 1))

;;; deft
(use-package deft
  :bind ("C-c \\" . #'deft)
  :commands (deft)
  :custom
  (deft-directory "~/wiki")
  (deft-extensions '("md" "org")))

;;; ws-butler
(use-package ws-butler
  :blackout
  :hook
  (prog-mode-hook . ws-butler-mode))

;;; yasnippet
(use-package yasnippet
  :disabled
  :commands (yas-global-mode)
  :init
  (yas-global-mode)
  :config
  (add-to-list 'yas-snippet-dirs (expand-file-name "snippets" user-emacs-directory))
  (yas-reload-all))

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


;;; TODO investigate tempel
(use-package tempel
  :custom
  (tempel-trigger-prefix "<")

  :bind (("M-=" . #'tempel-complete) ;; Alternative tempel-expand
         ("M-*" . #'tempel-insert))

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

  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
  )

;;; load local settings
(let ((local-settings (expand-file-name "local-settings.el" user-emacs-directory)))
  (when (file-exists-p local-settings)
    (load-file local-settings)))

(provide 'init.el)
;;; init.el ends here
