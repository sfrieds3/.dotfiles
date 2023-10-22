;;; lang-config.el --- lang configs -*- lexical-binding: t -*-
;;; Commentary:
;;     language-specific configurations

;;; Code:
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

;;; python-mode
(use-package python-mode
  :commands (python-mode)
  :custom
  (python-shell-interpreter "ipython"))

(use-package pyvenv
  :init
  (setenv "WORKON_HOME" "~/.venv")
  (pyvenv-mode t)
  :hook (pyvenv-post-activate-hooks . pyvenv-restart-python))

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
  (typescript-mode-hook . setup-tide-mode))

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

(provide 'lang-config)
;;; lang-config.el ends here
