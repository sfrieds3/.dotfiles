;;;; package --- summary
;;; Commentary:
;;;   custom packages I want to use at home, but not at work

;;; Code:

;; common lisp
(use-package slime
  :init
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq slime-contribs '(slime-fancy)))

;; clojure
(use-package cider)

(provide 'home)
;;; home ends here
