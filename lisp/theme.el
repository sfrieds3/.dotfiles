;;; package --- Summary
;;; Commentary:
;; theme.el -- solarized modeline for Emacs
;; solarized modeline for Emacs
;; most code is from https://gitlab.com/mark.feller/emacs.d/blob/master/modules/module-solarized.el
;; reqires: M-x all-the-icons-install-fonts

;;; code:

;; use rich-minority package to get rid of minor modes on statusline
(require 'rich-minority)
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

         (setcar mode-line-position
                 '(:eval (format "%3d%%" (/ (window-end) 0.01 (point-max)))))
         )
  :config
  ;; add git status to mode line
  (defadvice vc-git-mode-line-string (after plus-minus (file) compile activate)
    (setq ad-return-value
          (concat ad-return-value
                  (let ((plus-minus (vc-git--run-command-string
                                     file "diff" "--numstat" "--")))
                    (and plus-minus
                         (string-match "^\\([0-9]+\\)\t\\([0-9]+\\)\t" plus-minus)
                         (format " +%s-%s" (match-string 1 plus-minus) (match-string 2 plus-minus)))))))

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
                        mode-line-modes
                        " "
                        system-name
                        ))))

(load-theme 'monochrome)

(set-face-attribute 'mode-line t
                    :background "#808080"
                    :foreground "#1c1c1c"
                    :box '(:line-width 2 :color "#1c1c1c")
                    :overline nil
                    :underline nil)

(set-face-attribute 'mode-line-buffer-id t
                    :foreground "8a8a8a")

(set-face-attribute 'mode-line-inactive t
                    :background "#808080"
                    :foreground "#1c1c1c"
                    :box '(:line-width 2 :color "#808080")
                    :overline nil
                    :underline nil)

(provide 'theme.el)

;;; theme.el ends here
