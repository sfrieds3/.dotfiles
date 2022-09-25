;;; early-init.el --- Emacs 27+ pre-init
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

;;; try not to gc during emacs startup... set to 10MB from 800kb
(setq gc-cons-threshold 100000000)
(defun $restore-gc-cons-threshold ()
  "Restore \"gc-cons-threshold\" to 800kb.  To be added to after-init-hook."
  (setq gc-cons-threshold 800000)
  (message "gc-cons-threshold restored to %S"
           gc-cons-threshold))
(add-hook 'after-init-hook #'$restore-gc-cons-threshold)

;;; do not warn cl deprecated
(setq byte-compile-warnings '(cl-functions))

(provide 'early-init)
;;; early-init.el ends here
