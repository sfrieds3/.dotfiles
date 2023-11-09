;;; lang-config.el --- lang configs -*- lexical-binding: t -*-
;;; Commentary:
;;     language-specific configurations

;;; Code:
(use-package elisp-mode
  :elpaca nil
  :hook
  ;; diminish lisp-interaction and emacs-lisp modes
  ((lisp-interaction-mode-hook . (lambda () (setq mode-name "λ")))
   (emacs-lisp-mode-hook . (lambda () (setq mode-name '("λ"
                                  (lexical-binding
                                   ""
                                   (:propertize
                                    "/d" face warning))))))))

;;; yaml-mode
(use-package yaml-mode
  :commands (yaml-mode)
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode)))

;;; markdown-mode
(use-package markdown-mode
  :commands (markdown-mode
             gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown"))

;;; python
(use-package python-mode
  :commands (python-mode)
  :config
  :custom
  (python-shell-interpreter "python3")
  (python-indent-offset 4))

(use-package pyvenv
  :init
  (setenv "WORKON_HOME" "~/.venv")
  (pyvenv-mode t)
  :hook
  (pyvenv-post-activate-hooks . pyvenv-restart-python))

(use-package conda
  :after python
  :config
  ;; If none of these work for you, `conda-anaconda-home' must be set
  ;; explicitly. Afterwards, run M-x `conda-env-activate' to switch between
  ;; environments
  (or (cl-loop for dir in (list conda-anaconda-home
                                "~/.anaconda"
                                "~/.miniconda"
                                "~/.miniconda3"
                                "~/.miniforge3"
                                "~/anaconda3"
                                "~/miniconda3"
                                "~/miniforge3"
                                "~/opt/miniconda3"
                                "/usr/bin/anaconda3"
                                "/usr/local/anaconda3"
                                "/usr/local/miniconda3"
                                "/usr/local/Caskroom/miniconda/base"
                                "~/.conda")
               if (file-directory-p dir)
               return (setq conda-anaconda-home (expand-file-name dir)
                            conda-env-home-directory (expand-file-name dir)))
      (message "Cannot find Anaconda installation"))

  ;; integration with term/eshell
  (conda-env-initialize-interactive-shells)
  :hook
  (eshell-mode-hook . #'conda-env-initialize-eshell))

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

;; fish
(use-package fish-mode)

;; zig
(use-package zig-mode)

;;; cperl-mode
(use-package cperl-mode
  :elpaca nil
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

(use-package dockerfile-mode)

;;; javacsript
(use-package js
  :elpaca nil
  :custom
  (js-indent-level 2))

;;; sh-mode
(use-package sh-script
  :elpaca nil
  :config
  (add-to-list 'auto-mode-alist '("/zsh/" . sh-mode)))

(use-package tsx-ts-mode
  :elpaca nil
  :mode("\\.tsx\\'"))

(use-package typescript-ts-mode
  :elpaca nil
  :mode("\\.ts\\'"))

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
  (typescript-ts-mode-hook . tide-setup)
  (tsx-ts-mode-hook . tide-setup)
  (typescript-ts-mode . tide-hl-identifier-mode))

;;; ruby-mode
(use-package ruby-mode
  :elpaca nil
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
         "\\.mustache\\'"
         "\\.djhtml\\'"))

;;; c++-mode
(use-package c++-mode
  :elpaca nil
  :commands (c++-mode)
  :custom
  (c-basic-offset 2)
  :config
  (c-set-offset 'substatement-open 0)
  :bind ((:map c-mode-base-map
               ("<tab>" . #'indent-for-tab-command))))

;;; projectile-rails
(use-package projectile-rails
  :disabled
  :mode "\\.rb\\'"
  :interpreter "ruby"
  :commands (projectile-rails-mode
             projectile-rails-command-map)
  :bind (("C-c R" . #'projectile-rails-command-map))
  :config
  (projectile-rails-global-mode)
  :hook
  (ruby-mode-hook . projectile-rails-mode))

(provide 'lang-config)
;;; lang-config.el ends here
