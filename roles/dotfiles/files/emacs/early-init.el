;;; early-init.el --- Emacs 27+ pre-init -*- lexical-binding: t -*-
;;; Commentary:
;; loaded pre-init file for Emacs 27+..
;; we are not going to do anythin here to maintain
;; backwards compatibility

;;; code:

(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(setq package-enable-at-startup nil)
(setq frame-inhibit-implied-resize t)
(customize-set-variable 'warning-minimum-level :error)

(custom-set-faces
 '(fixed-pitch ((t (:inherit default))))
 '(variable-pitch ((t (:inherit default)))))

;;; make the default frame a bit wider
(add-to-list 'default-frame-alist `(width . 150))

;;; nicer titlebar
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

;;; try not to gc during emacs startup... set to 10MB from 800kb
;; (setq gc-cons-threshold 100000000)
;; (defun $restore-gc-cons-threshold ()
;;   "Restore \"gc-cons-threshold\" to 800kb.  To be added to after-init-hook."
;;   (setq gc-cons-threshold 800000)
;;   (message "gc-cons-threshold restored to %S"
;;            gc-cons-threshold))
;; (add-hook 'after-init-hook #'$restore-gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216 gc-cons-percentage 0.1)))

;;; increase amount of data emacs reads from process
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;;; do not warn cl deprecated
(setq byte-compile-warnings '(cl-functions))

(provide 'early-init)
;;; early-init.el ends here
