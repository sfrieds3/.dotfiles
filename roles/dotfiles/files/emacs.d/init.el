;;; init.el --- scwfri init.el ;;; Commentary:
;;     place 'local-settings.el' file (provide 'local-settings)
;;     in .emacs.d directory to overwrite settings (loaded at end)

;;; Code:

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
(setf transient-mark-mode t)

;;; spaces by default instead of tabs!
(setq-default indent-tabs-mode nil)

;;; ask about adding a final newline
(setq require-final-newline 'ask)

;;; allow recursive minibuffers
(setq enable-recursive-minibuffers t)

;;; overwrite highlighted text
(add-hook 'after-init-hook 'delete-selection-mode)

;;; allow disabled emacs commands (mainly for narrowing)
(setq disabled-command-function nil)

;;; do not ask about opening large files
(setf large-file-warning-threshold nil)

;;; show garbage collection messages in minbuffer
(setq garbage-collection-messages t)

;;; disable insert keys automatically for read-only buffers
(setq view-read-only t)

;;; debug on error -- off for now
(setq debug-on-error nil)

;;; filename in titlebar
(setq frame-title-format
      (concat user-login-name "@" system-name ":%f [%m]"))

;;; set completing-read-multiple separator to '#'
(setf crm-separator "#")

;;; Add prompt indicator to `completing-read-multiple'.
(defun $crm-indicator (args)
  "Add indicator when in CRM prompt and reading ARGS."
  (cons (concat "[CRM] " (car args) crm-separator) (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'$crm-indicator)

;;; Grow and shrink minibuffer
(setq resize-mini-windows t)

;;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

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
  (with-eval-after-load 'org
    ($remap-mark-command 'org-mark-element org-mode-map)
    ($remap-mark-command 'org-mark-subtree org-mode-map))
  :bind (("M-i" . (lambda () (interactive) (activate-mark)))
         ("M-S-i" . tab-to-tab-stop)
         :map isearch-mode-map
         ("C-q" . $isearch-highlight-phrase)))

;;; theme config
(use-package theme-config
  :straight nil
  :demand
  :config
  ($set-path)
  ($set-preferred-font)
  :custom
  (custom-safe-themes t)
  (fringe-mode 0)
  :hook
  (after-init-hook . $set-preferred-theme))

(use-package scwfri-config
  :straight nil)
(use-package modeline
  :straight nil)
;;;(use-package keybindings
;;;  :straight nil)
(use-package tramp-config
  :straight nil)
(use-package s)

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

;;; themes
(use-package color-theme-sanityinc-tomorrow)
(use-package material-theme
  :straight (material-theme :type git
                            :host github
                            :repo "cpaulik/emacs-material-theme"))

;;; ir-black theme
(use-package ir-black-theme
  :config
  (with-eval-after-load "ir-black-theme"
    (custom-theme-set-faces
     'ir-black
     '(cursor ((t (:background "white"))))
     '(highlight ((t (:background "grey"))))
     '(italic ((t (:slant italic))))
     '(cperl-array-face ((t (:inherit font-lock-keyword-face))))
     '(cperl-hash-face ((t (:inherit font-lock-variable-name-face)))))))

;;; evil
(use-package evil
  :init
  (evil-mode 1)
  (setq evil-undo-system 'undo-fu))

;;; goto-chg
(use-package goto-chg)

;;; undo stuff
(use-package undo-fu
  :init
  (setq undo-limit 6710886400)
  (setq undo-strong-limit 100663296)
  (setq undo-outer-limit 1006632960))
(use-package undo-fu-session
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  :init
  (global-undo-fu-session-mode))

;;; lsp
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (
         (python . lsp)
         (go . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)
(use-package lsp-ui :commands lsp-ui-mode)

;;; treesitter
(use-package tree-sitter
  :init
  (global-tree-sitter-mode)
  :hook
  (tree-sitter-after-on-hook . tree-sitter-hl-mode))
(use-package tree-sitter-langs)

;;; pyright
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

;;; golang
(use-package go-mode)

;;; avy
(use-package avy
  :bind (("C-;" . avy-goto-word-1)
         ("s-;" . avy-goto-char-timer)))

;;; hideshow
(use-package hideshow
  :bind ("C-c h" . hs-toggle-hiding)
  :commands hs-toggle-hiding
  :hook
  (prog-mode-hook . hs-minor-mode))

;;; org-defun
(use-package org-defun
  :straight nil
  :bind (("s-SPC" . $org-table--mark-field)))

;;; org-mode
(use-package org
  :straight (:type built-in)
  :commands (org-mode
             org-capture)
  :defer t
  :bind (("C-c N" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c I" . org-id-copy))
  :config
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
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

;;; org-table
(use-package org-table
  :straight nil)

;;; projectile
(use-package projectile
  :commands (projectile-mode)
  :init
  (projectile-mode)
  :custom
  (projectile-use-git-grep t)
  :config
  (setf projectile-tags-command (s-replace-regexp "^ctags" "/usr/bin/ctags" projectile-tags-command))
  :bind (("C-c f" . projectile-find-file)
         ("C-c b" . projectile-switch-to-buffer)
         :map projectile-mode-map
         ("C-c p" . projectile-command-map)))

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
              ("C-M-a" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package vertico
  :init
  (vertico-mode)
  (setq vertico-cycle t)
  (setq vertioc-resize t))

(use-package savehist
  :init
  (savehist-mode))

(use-package orderless-defun
  :straight nil)

(use-package orderless
  :after (orderless-defun)
  :custom
  (completion-styles '(orderless))
  (orderless-matching-styles '(orderless-flex))
  (orderless-style-dispatchers '($orderless-literal
                                 $orderless-strict-leading-initialism
                                 $orderless-initialism
                                 $orderless-regexp
                                 $orderless-flex
                                 $orderless-without-literal))
  :config
  (define-key minibuffer-local-map (kbd "C-l")
    #'$match-components-literally))

(use-package corfu
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  :init
  (global-corfu-mode))

(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x f" . consult-recent-file)
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flycheck)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

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
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key (kbd "M-."))

  (setq consult-narrow-key (kbd "C-+"))
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; use projectile
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root))))

;;; consult-flycheck
(use-package consult-flycheck
  :bind (:map flycheck-command-map
              ("!" . consult-flycheck)))

;;; ctags
(use-package ctags
  :bind (("s-." . ctags-find)))

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
        (ansi-term "/bin/bash"))
       ((string= abuf (buffer-name))
        (previous-buffer))
       ((try-completion abuf (mapcar #'buffer-name (buffer-list)))
        (switch-to-buffer abuf))
       (t (ansi-term "/bin/bash")))))
  :bind (("C-x T" . $ansi-term-dwim)
         ("C-`" . $ansi-term-dwim)))

;; magit
(use-package magit
  :defer t
  :commands (magit-status
             magit-diff-dwim
             magit-blame
             magit=diff-range)
  :init
  :bind (("C-x g" . magit-status)
         ("C-c g d" . magit-diff-range)
         ("C-c g b" . magit-blame))
  :config
  (add-to-list 'display-buffer-alist
               '("magit.*"
                 (display-buffer-at-bottom)
                 (window-height . 0.4))))

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
  :bind ("<f5>" . recompile))

;;; show matching parens
(use-package paren
  :config
  (show-paren-mode 1))

;;; smart parens
(use-package elec-pair
  :commands (electric-pair-mode)
  :init
  (electric-pair-mode -1)
  :config
  (defun $inhibit-electric-pair-mode (char)
    "Do not use smart parens in mini-buffers.  Params: CHAR."
    (minibufferp))
  (setq electric-pair-inhibit-predicate #'$inhibit-electric-pair-mode))

;;; highlight current line
(use-package hl-line
  :config
  (global-hl-line-mode nil)
  (set-face-attribute hl-line-face nil :underline nil)
  :bind (("<f9> l". hl-line-mode)))

;;; expand-region
(use-package expand-region
  :bind (("C-=" . er/expand-region)))

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
  :defer 5
  :commands (flycheck-mode
             global-flycheck-mode)
  :custom
  (flycheck-standard-error-navigation nil)
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-python-pycompile-executable "python3")
  :config
  (global-flycheck-mode))

;;; slime
(use-package slime
  :commands (slime)
  :init
  (setq inferior-lisp-program "/usr/bin/sbcl")
  :config
  (defun $slime-keybindings ()
    "keybindings for use in slime"
    (local-set-key (kbd "C-c e") 'slime-eval-last-expression)
    (local-set-key (kbd "C-c b") 'slime-eval-buffer))
  (add-hook 'slime-mode-hook #'$slime-keybindings)
  (add-hook 'slime-repl-mode-hook #'$slime-keybindings)
  (setq slime-contribs '(slime-fancy))
  :hook
  (slime-mode-hook . (lambda ()
                       (load (expand-file-name "~/quicklisp/slime-helper.el"))
                       (add-to-list 'slime-contribs 'slime-fancy)
                       (add-to-list 'slime-contribs 'inferior-slime))))

;;; eldoc mode
(use-package eldoc-mode
  :straight (:type built-in)
  :commands (eldoc-mode)
  :hook
  (emacs-lisp-mode-hook . eldoc-mode)
  (lisp-interaction-mode-hook . eldoc-mode)
  (ielm-mode-hook . eldoc-mode))

;;; yaml-mode
(use-package yaml-mode
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

;;; web-mode
(use-package web-mode
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  :mode ("\\.phtml\\'"
         "\\.tpl\\.php\\'"
         "\\.[agj]sp\\'"
         "\\.as[cp]x\\'"
         "\\.erb\\'"
         "\\.mustache\\'"
         "\\.djhtml\\'"))

;;; python-mode
(use-package python-mode
  :commands (python-mode)
  :custom
  (python-shell-interpreter "python3")
  :init
  (eldoc-mode 1))

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
               ("<tab>" . indent-for-tab-command))))

;;; ruby-mode
(use-package ruby-mode
  :custom
  (ruby-deep-indent-paren nil))

;;; c++-mode
(use-package c++-mode
  :straight (:type built-in)
  :commands (c++-mode)
  :custom
  (c-basic-offset 2)
  :config
  (c-set-offset 'substatement-open 0)
  :bind ((:map c-mode-base-map
               ("<tab>" . indent-for-tab-command))))

;;; projectile-rails
(use-package projectile-rails
  :commands (projectile-rails-mode
             projectile-rails-command-map)
  :bind (("C-c R" . projectile-rails-command-map))
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
  :bind (("M-o" . other-window)))

;;; winner-mode
(use-package winner
  :hook
  (after-init-hook . winner-mode)
  :bind(("s-<" . winner-undo)
        ("s->" . winner-redo)))

;;; windmove
(use-package windmove
  :custom
  (windmove-create-window nil)
  :bind (("C-c <up>" . windmove-up)
         ("C-c <down>" . windmove-down)
         ("C-c <left>" . windmove-left)
         ("C-c <right>" . windmove-right)))

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

  :bind (("C-x t h" . tab-bar-history-forward)
         ("C-x t l" . tab-bar-history-back)
         ("C-x t n" . tab-next)
         ("C-x t p" . tab-previous)
         ("<s-tab>" . tab-next)
         ("<S-s-iso-lefttab>" . tab-previous)
         ("<f8>" . $tab-tab-bar-toggle)
         ("C-x t k" . tab-close)
         ("C-x t c" . tab-new)
         ("C-x t t" . $tab-select-tab-dwim)
         ("s-t" . $tab-select-tab-dwim)))

;;; which-key
(use-package which-key
  :config
  (which-key-mode))

;;; idle-highlight-mode
(use-package idle-highlight-mode)

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
              ("C-c t p" . hl-todo-previous)
              ("C-c t n" . hl-todo-next)
              ("C-c t o" . hl-todo-occur)
              ("C-c t i" . hl-todo-insert)))

;;; helpful
(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)))

;;; no-littering
(use-package no-littering
  :config
  (recentf-mode t)
  (add-to-list 'recentf-exclude "*/.ido.last")
  (add-to-list 'recentf-exclude "*/TAGS")
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  ;; backup settings
  :custom
  (recentf-max-saved-items 1000)
  (delete-old-versions -1)
  (version-control t)
  (vc-make-backup-files t)

  ;; history settings
  (history-length t)
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

;;; visual-regexp
(use-package visual-regexp
  :bind (("C-c r" . vr/replace)
         ("C-c q" . vr/query-replace)))

;;; rectangle-mark
(use-package rect
  :straight (:type built-in)
  :bind (:map rectangle-mark-mode-map
              ("C-x r I" . string-insert-rectangle)
              ("C-x r R" . replace-rectangle)))

;;; deft
(use-package deft
  :bind ("C-c \\" . deft)
  :commands (deft)
  :custom
  (deft-directory "~/ref")
  (deft-extensions '("md" "org")))

;;; ws-butler
(use-package ws-butler
  :hook
  (prog-mode-hook . ws-butler-mode))

;;; yasnippet
(use-package yasnippet
  :commands (yas-global-mode)
  :init
  (yas-global-mode)
  :config
  (add-to-list 'yas-snippet-dirs (expand-file-name "snippets" user-emacs-directory))
  (yas-reload-all))

;;; load local settings
(let ((local-settings (expand-file-name "local-settings.el" user-emacs-directory)))
  (when (file-exists-p local-settings)
    (load-file local-settings)))

(provide 'init.el)
;;; init.el ends here
