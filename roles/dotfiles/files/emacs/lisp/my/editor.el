;;; editor.el --- editor configs -*- lexical-binding: t -*-
;;; Commentary:
;;     editor configuration

;;; Code:

;;; formatting
(use-package apheleia
  :diminish)

(use-package flymake-ruff
  :hook
  (python-mode-hook . flymake-ruff-load))

(provide 'editor)
;;; editor.el ends here
