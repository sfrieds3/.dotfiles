;;; evil-config.el --- configs for evil -*- lexical-binding: t -*-

;;; Commentary:
;;; configs for evil

;;; Code:

(defun $evil-ex-mode-map (cmd)
  "Bind CMD for evil ex-mode."
  (let ((binding (car cmd))
        (fn (cdr cmd)))
    (evil-ex-define-cmd binding fn)))
(mapc '$evil-ex-mode-map
      '(("!" . shell-command)
        ("W" . evil-write)
        ("Wq" . evil-save-and-quit)
        ("WQ" . evil-save-and-quit)
        ("E" . evil-edit)
        ("Q" . evil-quit)
        ("QA" . evil-quit-all)
        ("Qa" . evil-quit-all)
        ("CQ" . evil-quit-all-with-error-code)
        ("Cq" . evil-quit-all-with-error-code)
        ("WA" . evil-write-all)
        ("Wa" . evil-write-all)
        ("Git" . magit)
        ("Gdiff" . magit-diff)
        ("Grep" . consult-ripgrep)
        ("Occur" . occur)
        ("Align" . align-regexp)
        ("Glog" . magit-log)
        ("Gstatus" . magit-status)))

(defun $propertize-evil-state-tags ()
  "Set properties for evil states."
  (let ((white "#FFFFFF"))
    (setq evil-normal-state-tag
          (propertize " NORMAL "
                      'face `(:foreground ,white :background "dark blue" :weight bold)))

    (setq evil-insert-state-tag
          (propertize " INSERT "
                      'face `(:foreground ,white :background "dark green" :weight bold)))

    (setq evil-visual-char-tag
          (propertize " VISUAL "
                      'face `(:foreground ,white :background "dark cyan" :weight bold)))

    (setq evil-visual-line-tag
          (propertize " V-LINE "
                      'face `(:foreground ,white :background "dark cyan" :weight bold)))

    (setq evil-visual-screen-line-tag
          (propertize " VISUAL "
                      'face `(:foreground ,white :background "dark cyan" :weight bold)))

    (setq evil-visual-block-tag
          (propertize " V-BLOCK "
                      'face `(:foreground ,white :background "dark cyan" :weight bold)))

    (setq evil-operator-state-tag
          (propertize " O-PENDING "
                      'face `(:foreground ,white :background "dark orange" :weight bold)))

    (setq evil-replace-state-tag
          (propertize " REPLACE "
                      'face `(:foreground ,white :background "dark red" :weight bold)))

    (setq evil-motion-state-tag
          (propertize " MOTION "
                      'face `(:foreground ,white :background "black" :weight bold)))

    (setq evil-emacs-state-tag
          (propertize " EMACS "
                      'face `(:foreground ,white :background "dark magenta" :weight bold)))))

($propertize-evil-state-tags)

(provide 'evil-config)
;;; evil-config.el ends here
