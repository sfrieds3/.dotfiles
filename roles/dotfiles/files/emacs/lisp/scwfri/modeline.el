;;;; package --- summary

;;; Commentary:
;;;     modeline

;;; Code:

(defun $simple-mode-line-render (left right)
  "Return a string of `window-width' length.  Containing LEFT, and RIGHT aligned respectively."
  (let ((available-width
         (- (window-total-width)
            (+ (length (format-mode-line left))
               (length (format-mode-line right))))))
    (append left
            (list (format (format "%%%ds" available-width) ""))
            right)))

(setq-default mode-line-format
              '((:eval ($simple-mode-line-render
                        ;; Left.
                        (quote ("%e "
                                evil-mode-line-format
                                evil-mode-line-tag
                                mode-line-mule-info
                                mode-line-modified
                                " %b"
                                vc-mode))
                        ;; Right.
                        (quote (
                                "[%m] (%l:%c %p) "
                                mode-line-misc-info))))))

;  (defun with-face (str &rest face-plist)
;    (propertize str 'face face-plist))

;  (defun sl/make-header ()
;    ""
;    (let* ((sl/full-header (abbreviate-file-name buffer-file-name))
;           (sl/header (file-name-directory sl/full-header))
;           (sl/drop-str "[...]"))
;      (if (> (length sl/full-header)
;             (window-body-width))
;          (if (> (length sl/header)
;                 (window-body-width))
;              (progn
;                (concat (with-face sl/drop-str
;                                   :background "blue"
;                                   :weight 'bold
;                                   )
;                        (with-face (substring sl/header
;                                              (+ (- (length sl/header)
;                                                    (window-body-width))
;                                                 (length sl/drop-str))
;                                              (length sl/header))
;                                   ;; :background "red"
;                                   :weight 'bold
;                                   )))
;            (concat (with-face sl/header
;                               ;; :background "red"
;                               :foreground "#8fb28f"
;                               :weight 'bold
;                               )))
;        (concat (with-face sl/header
;                           ;; :background "green"
;                           ;; :foreground "black"
;                           :weight 'bold
;                           :foreground "#8fb28f"
;                           )
;                (with-face (file-name-nondirectory buffer-file-name)
;                           :weight 'bold
                           ;; :background "red"
;                           )))))

;  (defun sl/display-header ()
;    (setq header-line-format
;          '("" ;; invocation-name
;            (:eval (if (buffer-file-name)
;                       (sl/make-header)
;                     "%b")))))

;  (add-hook 'buffer-list-update-hook
;            'sl/display-header)

(provide 'modeline)
;;; modeline.el ends here
