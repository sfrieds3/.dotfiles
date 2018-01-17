;;; package - Summary
;;; Commentary:
;;; settings for autocomplete

;;; code:

(use-package auto-complete
  :ensure t)
(ac-config-default)
(setq ac-auto-show-menu 0)   ; show menu immediately
(setq ac-ignore-case 'smart) ; use smart case
(global-auto-complete-mode t)

(provide 'autocomplete.el)

;;; autocomplete.el ends here
