;;; editor.el --- editor configs -*- lexical-binding: t -*-
;;; Commentary:
;;     editor configuration

;;; Code:

;;; formatting
(use-package apheleia
  :diminish
  :config
  (setf (alist-get 'isort apheleia-formatters)
        '("isort" "--stdout" "-"))
  (setf (alist-get 'python-mode apheleia-mode-alist)
        '(isort black))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist)
        '(isort black)))

(use-package flymake-ruff
  :hook
  (python-mode-hook . flymake-ruff-load))

(provide 'editor)
;;; editor.el ends here
