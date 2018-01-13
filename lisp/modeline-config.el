;;; modeline-config.el -- solarized modeline for emacs
;; solarized modeline for emacs
;; most code is from https://gitlab.com/mark.feller/emacs.d/blob/master/modules/module-solarized.el
;; reqires: M-x all-the-icons-install-fonts

;;; code:

(use-package solarized-theme
  :ensure t
  :config
  (progn (setq solarized-emphasize-indicators nil
               solarized-high-contrast-mode-line nil
               solarized-scale-org-headlines nil
               solarized-use-less-bold t
               solarized-use-variable-pitch nil
               solarized-distinct-fringe-background nil)))

;; use rich-minority package to get rid of minor modes on statusline
(use-package rich-minority
  :ensure t)
(rich-minority-mode 1)
(setf rm-blacklist "")

(use-package all-the-icons
  :ensure t
  :demand
  :init
  (progn (defun -custom-modeline-github-vc ()
           (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
             (concat
              (propertize (format " %s" (all-the-icons-octicon "git-branch"))
                          'face `(:height 1 :family ,(all-the-icons-octicon-family))
                          'display '(raise 0))
              (propertize (format " %s" branch)))))

         (defun -custom-modeline-svn-vc ()
           (let ((revision (cadr (split-string vc-mode "-"))))
             (concat
              (propertize (format " %s" (all-the-icons-faicon "cloud"))
                          'face `(:height 1)
                          'display '(raise 0))
              (propertize (format " %s" revision) 'face `(:height 0.9)))))

         (defvar mode-line-my-vc
           '(:propertize
             (:eval (when vc-mode
                      (cond
                       ((string-match "Git[:-]" vc-mode) (-custom-modeline-github-vc))
                       ((string-match "SVN-" vc-mode) (-custom-modeline-svn-vc))
                       (t (format "%s" vc-mode)))))
             face mode-line-directory)
           "Formats the current directory.")

         ;; (setcar mode-line-position "")
         )
  :config
  (progn (setq-default mode-line-format
                       (list
                        " "
                        mode-line-mule-info
                        mode-line-modified
                        mode-line-frame-identification
                        mode-line-buffer-identification
                        "  "
                        mode-line-position
                        mode-line-my-vc
                        "   "
                        mode-line-modes))))

(load-theme 'solarized-dark t)

(set-face-attribute 'mode-line nil
                    :background "#808080"
                    :foreground "#1c1c1c"
                    :box '(:line-width 2 :color "#1c1c1c")
                    :overline nil
                    :underline nil)

(set-face-attribute 'mode-line-buffer-id nil
                    :foreground "8a8a8a")

(set-face-attribute 'mode-line-inactive nil
                    :background "#808080"
                    :foreground "#1c1c1c"
                    :box '(:line-width 2 :color "#808080")
                    :overline nil
                    :underline nil)

(provide 'modeline-config.el)

;;; modeline-config.el ends here
