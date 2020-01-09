;;;; package --- summary
;;; Commentary:
;;;   custom packages I want to use at home, but not at work

;;; Code:

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      '(("gnu-elpa"     . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa"        . "https://melpa.org/packages/")))

;; set package archive priorities if version 26+
(cond ((>= 26 emacs-major-version)
       (setq package-archive-priorities
             '(("melpa-stable" . 2)
               ("gnu-elp"      . 1)
               ("melpa"        . 0)))))

(package-initialize)

;; bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

;; always ensure packages are installed
(setq use-package-always-ensure t)

;; common lisp
(use-package slime
  :init
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq slime-contribs '(slime-fancy)))

;; clojure
(use-package cider)

(provide 'home)
;;; home ends here
