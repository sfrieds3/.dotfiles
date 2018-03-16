;;; package - Summary
;;; Commentary:
;;; packages to load

;;; code:

;; list packages
(setq package-list
      '(cl-lib
        markdown-mode
        evil
        evil-nerd-commenter
        better-defaults
        smooth-scrolling
        flycheck
        ag
        google-this
        indent-guide
        autopair
        minimap
        magit
        ghub
        which-key
        idle-highlight-mode
        ace-jump-mode
        undo-tree
        whitespace
        rainbow-delimiters
        ivy
        counsel
        swiper
        flx
        monochrome-theme))

;; activate all the packages (in particular autoloads)
(package-initialize)

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(provide 'packageload.el)

;;; packageload.el ends here
