;;; init.el --- scwfri init.el -*- lexical-binding: t -*-
;;; Commentary:
;;     place 'local-settings.el' file (provide 'local-settings)
;;     in .emacs.d directory to overwrite settings (loaded at end)

;;; Code:

;;; bootstrap elpaca
(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
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
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
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
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;;; add everything in site-lisp/ dir to load path
(let ((default-directory  (expand-file-name "site-lisp" user-emacs-directory)))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;;; home.el
(let ((home-settings (expand-file-name "home.el" user-emacs-directory)))
  (when (file-exists-p home-settings)
    (load-file home-settings)))

;;; shared config not in init.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

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
  :elpaca nil
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

(use-package toggle-commands
  :elpaca nil
  :after (theme-config ef-themes doom-themes)
  :config
  (+sf/cycle-font 0)
  (+sf/cycle-theme 0))

(use-package my-config
  :elpaca nil)

(use-package modeline
  :after evil
  :elpaca nil)

(use-package tramp-config
  :elpaca nil)

;;; language specific settings
(use-package lang-config
  :elpaca nil
  :demand t)

;;; editor settings
(use-package editor
  :elpaca nil
  :demand t)

(use-package treesit-config
  :elpaca nil
  :demand t)

(use-package editor-format
  :elpaca nil
  :demand)

;;; theme config
(use-package theme-config
  :elpaca nil
  :demand
  :config
  (+sf/set-path)
  :custom
  (custom-safe-themes t)
  (fringe-mode 0))

(use-package material-theme)
(use-package ef-themes)
(use-package standard-themes)
(use-package zerodark
  :elpaca (:host github :repo "NicolasPetton/zerodark-theme")
  :no-require t)
;; run all-the-icons-install-fonts
(use-package all-the-icons)
(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  (doom-modeline-indent-info t)
  (size-indication-mode t)
  (column-number-mode t)
  :config
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;;; match env to shell
(use-package exec-path-from-shell
  :custom
  (exec-path-from-shell-shell-name "/opt/homebrew/bin/fish")
  :init
  :config
  (dolist (var '("XDG_CONFIG_HOME" "XDG_DATA_HOME" "XDG_CACHE_HOME"))
    (add-to-list 'exec-path-from-shell-variables var))
  (add-to-list 'exec-path (substitute-in-file-name "$XDG_DATA_HOME/nvim/mason/bin"))
  (exec-path-from-shell-initialize))

;;; auto-revert everything
(use-package autorevert
  :elpaca nil
  :commands (global-auto-revert-mode)
  :init
  (global-auto-revert-mode 1)
  :custom
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil))

(use-package dired
  :elpaca nil
  :custom
  (dired-listing-switches "-alh")
  (dired-dwim-target t))

(use-package dired-x
  :elpaca nil
  :config
  (setq dired-omit-files "\\.DS_Store$\\|__pycache__$\\|.pytest_cache$\\|\\.mypy_cache$\\|\\.egg-info$"))

(use-package dired-git-info
  :defer t
  :after dired
  :commands (dired-git-info-mode dired-git-info-auto-enable)
  :bind (:map dired-mode-map
         (")" . #'dired-git-info-mode)))

;;; general
(use-package general
  :config
  (general-evil-setup)
  (general-create-definer +sf/bslocalleader :prefix "\\")
  (+sf/bslocalleader
   :keymaps 'normal
   "c" #'evil-delete-buffer
   "\\" #'evil-execute-in-emacs-state)
  (general-create-definer +sf/localleader :prefix "_")
  (+sf/localleader
    :keymaps 'normal
    "D" #'magit-diff-dwim
    "G" #'magit
    "cf" #'+sf/cycle-font
    "ct" #'+sf/cycle-theme
    "e" #'eval-buffer
    "x" #'eval-last-sexp)
  (general-create-definer +sf/leader :prefix "SPC")
  (+sf/leader
    :keymaps 'normal
    "RET" #'execute-extended-command
    "SPC" #'consult-buffer
    "b" #'consult-project-buffer
    "d" #'consult-flycheck
    "f" #'project-find-file
    "G" #'rg
    "gb" #'blamer-show-commit-info
    "gd" #'lsp-ui-peek-find-definitions
    "gg" #'consult-ripgrep
    "gr" #'lsp-ui-peek-find-references
    "hr" #'diff-hl-revert-hunk
    "hv" #'diff-hl-show-hunk
    "l" #'consult-line
    "R" #'lsp-rename
    "rg" #'deadgrep
    "vd" #'consult-lsp-diagnostics
    "vt" #'hl-todo-occur
    "/"  #'consult-line
    "\\" #'neotree
    ":" #'consult-lsp-symbols
    ";" #'consult-lsp-file-symbols)
  (+sf/leader
    :keymaps 'visual
    "RET" #'execute-extended-command)
  (general-create-definer +sf/search-leader :prefix "SPC s")
  (+sf/search-leader
    :keymaps 'normal
    "f" #'project-find-file
    "r" #'consult-recent-file
    "t" #'consult-lsp-symbols)
  (general-create-definer +sf/ctrl-c-leader :prefix "C-c")
  (+sf/ctrl-c-leader
    :keymaps 'normal
    "j" #'diff-hl-next-hunk
    "k" #'diff-hl-previous-hunk)
  (general-create-definer +sf/next :prefix "]")
  (+sf/next
    :keymaps 'normal
    "b" #'next-buffer
    "d" #'flycheck-next-error
    "t" #'tab-next)
  (general-create-definer +sf/previous :prefix "[")
  (+sf/previous
    :keymaps 'normal
    "b" #'previous-buffer
    "d" #'flycheck-previous-error
    "t" #'tab-previous)
  (general-def 'normal
    "M-n" #'tab-next
    "M-p" #'tab-previous))
;;; we may want to use general keyword later
(elpaca-wait)

;;; evil
(use-package evil-config
  :elpaca nil
  :after (evil))

(use-package evil
  :demand t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-disable-insert-state-bindings t)
  :config
  (evil-mode t)
  (defalias #'forward-evil-word #'forward-evil-symbol)
  (defun +sf/evil--nohl ()
    (progn
      (redraw-frame)
      (evil-ex-nohighlight)))
  (defun +sf/evil--eval-visual-region ()
    (interactive)
    (call-interactively 'eval-region)
    (evil-force-normal-state))
  ;; replaced be evil-goggles
  (defun +sf/evil--yank-advice (orig-fn beg end &rest args)
    "Advice to be added to `evil-yank' to highlight yanked region.
Pass ORIG-FN, BEG, END, TYPE, ARGS."
    (pulse-momentary-highlight-region beg end 'region)
    (apply orig-fn beg end args))
  ;; (advice-add 'evil-yank :around '+sf/evil--yank-advice)
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
  :bind
  ((:map evil-normal-state-map
         ("/" . #'ctrlf-forward-fuzzy-regexp)
         ("?" . #'ctrlf-backward-fuzzy-regexp)
         ([(control l)] . #'evil-ex-nohighlight)
         ([(control j)] . #'evil-next-line)
         ([(control k)] . #'evil-previous-line)
         ([(control return)] . #'eval-last-sexp)
         ([(control |)] . #'+sf/revert-buffer-noconfirm-and-update-diff-hl)
         ("j" . #'evil-next-visual-line)
         ("k" . #'evil-previous-visual-line)
         ("gj" . #'evil-next-line)
         ("gk" . #'evil-previous-line)
         ("gq" . #'+format/region-or-buffer)
         ([(control shift v)] . #'evil-paste-after)
         ([(meta h)] . #'evil-window-left)
         ([(meta j)] . #'evil-window-down)
         ([(meta k)] . #'evil-window-up)
         ([(meta l)] . #'evil-window-right)
         ([(shift up)] . #'evil-window-increase-height)
         ([(shift right)] . #'evil-window-increase-width)
         ([(shift down)] . #'evil-window-decrease-height)
         ([(shift left)] . #'evil-window-decrease-width))
   (:map evil-insert-state-map
         ([(control shift c)] . #'evil-yank)
         ([(control shift v)] . #'yank))
   (:map evil-visual-state-map
         ([(control shift c)] . #'evil-yank)
         ([(control shift v)] . #'evil-visual-paste)
         (";" . #'+sf/evil--eval-visual-region)
         ("SPC RET" . #'execute-extended-command))))
;;; we may want to use evil keyword later
(elpaca-wait)

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init)
  (diminish 'evil-collection-unimpaired-mode))

(use-package evil-goggles
  :after evil
  :diminish
  :config
  (evil-goggles-mode))

(use-package evil-commentary
  :after evil
  :diminish
  :init
  (evil-commentary-mode))

(use-package evil-quickscope
  :after evil
  :config
  (global-evil-quickscope-mode t))

(use-package evil-lion
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

(use-package evil-textobj-line)
(use-package evil-textobj-syntax)
(use-package evil-indent-plus
  :config
  (define-key evil-inner-text-objects-map "i" 'evil-indent-plus-i-indent)
  (define-key evil-outer-text-objects-map "i" 'evil-indent-plus-a-indent)
  (define-key evil-inner-text-objects-map "I" 'evil-indent-plus-i-indent-up)
  (define-key evil-outer-text-objects-map "I" 'evil-indent-plus-a-indent-up)
  (define-key evil-inner-text-objects-map "J" 'evil-indent-plus-i-indent-up-down)
  (define-key evil-outer-text-objects-map "J" 'evil-indent-plus-a-indent-up-down))

(use-package evil-numbers
  :after (evil)
  :commands (evil-numbers/inc-at-pt-incremental evil-numbers/dec-at-pt-incremental)
  :init
  (define-key evil-normal-state-map (kbd "C-; C-a") 'evil-numbers/inc-at-pt-incremental)
  (define-key evil-normal-state-map (kbd "C-; C-x") 'evil-numbers/dec-at-pt-incremental))

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

(use-package breadcrumb)

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

;;; lsp-mode
(use-package lsp-mode
  :demand t
  :custom
  (lsp-completion-provider :none)
  (lsp-file-watch-threshold 50000)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-headerline-breadcrumb-segments '(project symbols))
  :config
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\venv\\'")
  (defun +sf/python-mode-hook()
    (require 'lsp-pyright)
    (lsp-deferred))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((json-ts-mode-hook . lsp-deferred)
         (js-json-mode-hook . lsp-deferred)
         (yaml-mode-hook . lsp-deferred)
         (yaml-ts-mode-hook . lsp-deferred)
         (rust-mode-hook . lsp-deferred)
         (rust-ts-mode-hook . lsp-deferred)
         ;; (tsx-ts-mode-hook . lsp-deferred)
         ;; (typescript-ts-mode-hook . lsp-deferred)
         (lsp-mode-hook . lsp-enable-which-key-integration)
         (python-ts-mode-hook . +sf/python-mode-hook)
         (python-mode-hook . +sf/python-mode-hook)))

(use-package lsp-ui
  :after lsp
  :commands lsp-ui-mode)

(use-package lsp-pyright
  :after lsp)

(use-package consult-lsp
  :after (consult lsp))

;; debugging
(use-package realgud
  :commands (realgud-pdb)
  :bind (:map python-ts-mode-map
              ([f5] . #'realgud:pdb)
              :map python-mode-map
              ([f5] . #'realgud:pdb)))

;;; treesitter
(use-package treesit
  :elpaca nil)

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (global-treesit-auto-mode))

(use-package evil-textobj-tree-sitter
  :after (evil treesit)
  :config
  (defmacro $inlambda (functionname &rest args)
    "Create an interactive lambda of existing function `FUNCTIONNAME' with `ARGS'."
    (let ((funsymbol (concat "ilambda/" (symbol-name functionname))))
      `(cons ,funsymbol (lambda () (interactive) (apply #',functionname ',args)))))
  (defmacro $ilambda (functionname &rest args)
    "Create an interactive lambda of existing function `FUNCTIONNAME' with `ARGS'."
    `(lambda () (interactive) (apply #',functionname ',args)))

  (define-key evil-outer-text-objects-map "m" (evil-textobj-tree-sitter-get-textobj "import"
                                                '((python-mode . [(import_statement) @import])
                                                  (go-mode . [(import_spec) @import])
                                                  (rust-mode . [(use_declaration) @import]))))
  (define-key evil-outer-text-objects-map "f" (cons "evil-outer-function" (evil-textobj-tree-sitter-get-textobj "function.outer")))
  (define-key evil-inner-text-objects-map "f" (cons "evil-inner-function" (evil-textobj-tree-sitter-get-textobj "function.inner")))
  (define-key evil-outer-text-objects-map "c" (cons "evil-outer-class" (evil-textobj-tree-sitter-get-textobj "class.outer")))
  (define-key evil-inner-text-objects-map "c" (cons "evil-inner-class" (evil-textobj-tree-sitter-get-textobj "class.inner")))
  (define-key evil-outer-text-objects-map "n" (cons "evil-outer-comment" (evil-textobj-tree-sitter-get-textobj "comment.outer")))
  (define-key evil-inner-text-objects-map "n" (cons "evil-outer-comment" (evil-textobj-tree-sitter-get-textobj "comment.outer")))
  (define-key evil-outer-text-objects-map "v" (cons "evil-outer-conditional-loop" (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer"))))
  (define-key evil-inner-text-objects-map "v" (cons "evil-inner-conditional-loop" (evil-textobj-tree-sitter-get-textobj ("conditional.inner" "loop.inner"))))
  (define-key evil-inner-text-objects-map "a" (cons "evil-inner-parameter" (evil-textobj-tree-sitter-get-textobj "parameter.inner")))
  (define-key evil-outer-text-objects-map "a" (cons "evil-outer-parameter" (evil-textobj-tree-sitter-get-textobj "parameter.outer")))

  (define-key evil-normal-state-map (kbd "]a") (cons "goto-parameter-start" ($ilambda evil-textobj-tree-sitter-goto-textobj "parameter.inner")))
  (define-key evil-normal-state-map (kbd "[a") (cons "goto-parameter-start" ($ilambda evil-textobj-tree-sitter-goto-textobj "parameter.inner" t)))
  (define-key evil-normal-state-map (kbd "]A") (cons "goto-parameter-end" ($ilambda evil-textobj-tree-sitter-goto-textobj "parameter.inner" nil t)))
  (define-key evil-normal-state-map (kbd "[A") (cons "goto-parameter-end" ($ilambda evil-textobj-tree-sitter-goto-textobj "parameter.inner" t t)))
  (define-key evil-normal-state-map (kbd "]v") (cons "goto-conditional-start" ($ilambda evil-textobj-tree-sitter-goto-textobj ("conditional.outer" "loop.outer"))))
  (define-key evil-normal-state-map (kbd "[v") (cons "goto-conditional-start" ($ilambda evil-textobj-tree-sitter-goto-textobj ("conditional.outer" "loop.outer") t)))
  (define-key evil-normal-state-map (kbd "]V") (cons "goto-conditional-end" ($ilambda evil-textobj-tree-sitter-goto-textobj ("conditional.outer" "loop.outer") nil t)))
  (define-key evil-normal-state-map (kbd "[V") (cons "goto-conditional-end" ($ilambda evil-textobj-tree-sitter-goto-textobj ("conditional.outer" "loop.outer") t t)))
  (define-key evil-normal-state-map (kbd "]c") (cons "goto-class-start" ($ilambda evil-textobj-tree-sitter-goto-textobj "class.outer")))
  (define-key evil-normal-state-map (kbd "[c") (cons "goto-class-start" ($ilambda evil-textobj-tree-sitter-goto-textobj "class.outer" t)))
  (define-key evil-normal-state-map (kbd "]C") (cons "goto-class-end" ($ilambda evil-textobj-tree-sitter-goto-textobj "class.outer" nil t)))
  (define-key evil-normal-state-map (kbd "[C") (cons "goto-class-end" ($ilambda evil-textobj-tree-sitter-goto-textobj "class.outer" t t)))
  (define-key evil-normal-state-map (kbd "]n") (cons "goto-comment-start" ($ilambda evil-textobj-tree-sitter-goto-textobj "comment.outer")))
  (define-key evil-normal-state-map (kbd "[n") (cons "goto-comment-start" ($ilambda evil-textobj-tree-sitter-goto-textobj "comment.outer" t)))
  (define-key evil-normal-state-map (kbd "]N") (cons "goto-comment-end" ($ilambda evil-textobj-tree-sitter-goto-textobj "comment.outer" nil t)))
  (define-key evil-normal-state-map (kbd "[N") (cons "goto-comment-end" ($ilambda evil-textobj-tree-sitter-goto-textobj "comment.outer" t t)))
  (define-key evil-normal-state-map (kbd "]f") (cons "goto-function-start" (lambda () (interactive) (progn (evil-textobj-tree-sitter-goto-textobj "function.outer") (reposition-window)))))
  (define-key evil-normal-state-map (kbd "[f") (cons "goto-function-start" (lambda () (interactive) (progn (evil-textobj-tree-sitter-goto-textobj "function.outer" t) (reposition-window)))))
  (define-key evil-normal-state-map (kbd "]F") (cons "goto-function-end" (lambda () (interactive) (progn (evil-textobj-tree-sitter-goto-textobj "function.outer" nil t) (reposition-window)))))
  (define-key evil-normal-state-map (kbd "[F") (cons "goto-function-end" (lambda () (interactive) (progn (evil-textobj-tree-sitter-goto-textobj "function.outer" t t) (reposition-window))))))

;;; context using treesitter
(use-package posframe-plus
  :elpaca (:host github :type git :repo "zbelial/posframe-plus" ))
(use-package treesitter-context
  :after (treesit posframe-plus)
  :elpaca (:type git :host github :repo "zbelial/treesitter-context.el")
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
  :bind
  (:map evil-normal-state-map
        ("s" . #'avy-goto-word-1)))

;;; hideshow
(use-package hideshow
  :elpaca nil
  :bind ("C-c h" . #'hs-toggle-hiding)
  :commands hs-toggle-hiding
  :config
  (diminish 'hs-minor-mode)
  :hook
  (prog-mode-hook . hs-minor-mode))

;;; org-defun
(use-package org-defun
  :elpaca nil
  :bind (("s-SPC" . #'$org-table--mark-field)))

;;; org-mode
(use-package org
  :elpaca nil
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

(use-package evil-org-mode
  :elpaca (:host github :repo "Somelauw/evil-org-mode")
  :after org
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (defun $org-mode-hook ()
    (evil-org-mode))
  :hook (org-mode-hook . $org-mode-hook))

;;; org-table
(use-package org-table
  :elpaca nil
  :after org)

(use-package neotree
  :custom
  (neo-smart-open t)
  :config
  (defun $neotree--project-dir ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (project-root (project-current)))
          (file-name (buffer-file-name)))
      (neotree-toggle)
      (if project-dir
          (if (neo-global--window-exists-p)
              (progn
                (neotree-dir project-dir)
                (neotree-find file-name)))
        (message "Could not find git project root.")))))

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
    (dired-sidebar-toggle-sidebar)
    (ibuffer-sidebar-toggle-sidebar))
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
  :elpaca nil
  :config
  (defun +sf/project-override (dir)
    (let ((override (locate-dominating-file dir ".project.el")))
      (if override
          (cons 'vc override)
        nil)))
  (defun +sf/project--default-directory ()
    (if (project-current)
        (setq default-directory (cdr (project-current)))
      (if (buffer-file-name)
          (setq default-directory (file-name-parent-directory (buffer-file-name)))
        (setq default-directory (getenv "HOME")))))
  :hook
  ;; (find-file-hook . #'+sf/project--default-directory)
  ;; note: this is *not* `PROJECT-FIND-FUNCTIONS-HOOK`
  (project-find-functions . +sf/project-override))

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
  :elpaca nil
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
  :elpaca nil
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
  :elpaca nil)

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
   '((lsp-mode (styles . (orderless)))
     (file (styles . (basic partial-completion orderless)))
     (project-file (styles . (basic substring partial-completion orderless)))
     (imenu (styles . (basic substring orderless)))
     (kill-ring (styles . (basic substring orderless)))
     (consult-location (styles . (basic substring orderless)))))
  :config
  (define-key minibuffer-local-map (kbd "C-l") #'$match-components-literally))

(use-package corfu
  :elpaca (:files (:defaults "extensions/*"))
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
  (defun corfu-lsp-setup ()
    (setq-local completion-styles '(orderless)
                completion-category-defaults nil))
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
  :hook (lsp-mode-hook . corfu-lsp-setup)
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

(use-package ctrlf
  :bind
  (:map ctrlf-mode-map
        ("C-n" . #'ctrlf-forward-fuzzy-regexp)
        ("C-p" . #'ctrlf-backward-fuzzy-regexp)
        ("C-M-n" . #'ctrlf-forward-symbol-at-point)
        ("C-M-p" . #'ctrlf-backward-symbol-at-point)
        ("<escape>" . #'ctrlf-cancel)))

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
   consult-lsp-symbols consult-lsp-file-symbols consult-lsp-diagnostics
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
  :elpaca nil)

(use-package embark
  :after (embark-defun)
  :bind
  (("C-," . #'embark-act)
   ("M-," . #'$embark-act-noquit)
   ("C-M-," . #'embark-dwim) :map embark-general-map
   ([(control y)] . #'symbol-overlay-put)
   ([(control g)] . #'consult-ripgrep)
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
  :elpaca nil
  :after (embark-consult))

(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode-hook . consult-preview-at-point-mode))

(use-package vterm
  :demand t
  :elpaca (:pre-build (setq vterm-always-compile-module t))
  :config
  (defun $vterm-mode-hook ()
    (setq-local evil-insert-state-cursor 'box)
    (evil-insert-state))
  (add-to-list 'vterm-eval-cmds '("update-pwd" (lambda (path) (setq default-directory path))))
  :custom
  (vterm-shell "fish")
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
  :elpaca nil
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
        (ansi-term "fish"))
       ((string= abuf (buffer-name))
        (previous-buffer))
       ((try-completion abuf (mapcar #'buffer-name (buffer-list)))
        (switch-to-buffer abuf))
       (t (ansi-term "fish"))))))

;;; use fish shell
(use-package shell
  :elpaca nil
  :custom
  (explicit-shell-file-name "fish"))

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
  :elpaca (:host github :repo "artawower/blamer.el")
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
  :elpaca nil
  :commands diff-hl-flydiff-mode)

(use-package git-timemachine)

;;; compile
(use-package compile
  :elpaca nil
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
  :elpaca nil
  :config
  (show-paren-mode 1))

;;; smart parens
(use-package elec-pair
  :elpaca nil
  :commands (electric-pair-mode))

;;; highlight current line
(use-package hl-line
  :elpaca nil
  :config
  (set-face-attribute hl-line-face nil :underline nil)
  :bind (([f9]. #'hl-line-mode)))

;;; expand-region
(use-package expand-region
  :bind (("C-=" . #'er/expand-region)))

;;; nXml-mode
(use-package nxml-mode
  :elpaca nil
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
  (defun $flycheck--configure-python-checkers ()
    (flycheck-add-next-checker 'lsp 'python-pylint))
  (defun $flycheck--configure-json-checkers ()
    (flycheck-add-next-checker 'lsp 'json-jsonlint))
  (defun $flycheck--configure-ts-checkers ()
    (flycheck-add-next-checker 'lsp 'javascript-eslint))
  ;; :hook ((python-mode-hook . $flycheck--configure-python-checkers)
  ;;        (python-ts-mode-hook . $flycheck--configure-python-checkers)
  ;;        (json-js-mode-hook . $flycheck--configure-json-checkers)
  ;;        (json-ts-mode-hook . $flycheck--configure-json-checkers)
  ;;        (typescript-ts-mode-hook . $flycheck--configure-ts-checkers))
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
  :elpaca nil)

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
  :elpaca nil
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
  :elpaca nil
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
  :elpaca nil
  :hook
  (after-init-hook . winner-mode))

;;; tab-bar (again, most/all of this taken from prot)
(use-package tab-bar
  :elpaca nil
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
  :diminish
  :config
  (which-key-mode))

(use-package dimmer
  :custom
  (dimmer-fraction 0.10)
  (dimmer-watch-frame-focus-events nil)
  :config
  (defun +dimmer--advise-dimmer-config-change-handler ()
    "Advise to only force process if no predicate is truthy."
    (let ((ignore (cl-some (lambda (f) (and (fboundp f) (funcall f)))
                           dimmer-prevent-dimming-predicates)))
      (unless ignore
        (when (fboundp 'dimmer-process-all)
          (dimmer-process-all t)))))

  (defun +sf/corfu--frame-p ()
    "Check if the buffer is a corfu frame buffer."
    (string-match-p "\\` \\*corfu" (buffer-name)))

  (defun +dimmer--configure-corfu ()
    "Convenience settings for corfu users."
    (add-to-list
     'dimmer-prevent-dimming-predicates
     #'+sf/corfu--frame-p))

  (advice-add
   'dimmer-config-change-handler
   :override '+dimmer--advise-dimmer-config-change-handler)
  (+dimmer--configure-corfu)

  (defun dimmer--config-change-handler-patch (orig-fun &rest args)
    (run-at-time 0.2 nil #'apply orig-fun args))
  (advice-add 'dimmer-config-change-handler :around #'dimmer--config-change-handler-patch)

  (dimmer-mode t)
  (dimmer-configure-which-key)
  (dimmer-configure-magit)
  (dimmer-configure-posframe))

;;; idle-highlight-mode
(use-package idle-highlight-mode
  :custom
  (idle-highlight-idle-time 1.0)
  :bind (([(control f7)] . idle-highlight-mode)))

(use-package symbol-overlay
  :bind (("C-c j" . #'symbol-overlay-jump-next)
         ("C-c k" . #'symbol-overlay-jump-prev))
  ([f7] . #'symbol-overlay-put)
  ([(control shift f7)] . #'symbol-overlay-remove-all))

;;; hi-lock
(use-package hi-lock
  :elpaca nil
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
  :elpaca (:host github :repo "Crandel/tempel-collection"))

;;; load local settings
(let ((local-settings (expand-file-name "local-settings.el" user-emacs-directory)))
  (when (file-exists-p local-settings)
    (load-file local-settings)))

(provide 'init.el)
;;; init.el ends here
