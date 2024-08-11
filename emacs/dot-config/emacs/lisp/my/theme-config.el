;;; theme-config --- configure theme-related items -*- lexical-binding: t -*-

;;; Commentary:
;;;     theme configurations

;;; Code:

;;; disable existing themes before loading new themes
(defadvice load-theme
  (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))

;;; default preferred font-sizes
(defvar +sf/default-font-size
  (cond ((string= (system-name) "mixolydian") "10")
        ((string= (system-name) "phrygian") "14")
        (t "15")))

;;;###autoload
(defun +sf/set-current-font (font &optional size)
  "Set current font to FONT and SIZE."
  (let* ((font-size (or size +sf/default-font-size))
        (new-font (string-join `(,font ,font-size) " ")))
       (set-frame-font new-font nil t)))

;;;###autoload
(defun +sf/set-preferred-font (&optional frame)
  "Set preferred font and size for FRAME."
  (interactive)
  (catch 'done
    (with-selected-frame (or frame (selected-frame))
      (dolist (font +sf/preferred-font)
        (when (ignore-errors (x-list-fonts font))
          (+sf/set-current-font font)
          (throw 'done nil))))))

;;;###autoload
 (defun +sf/cycle-preferred-font ()
   "Cycle through preferred fonts."
   ;; TODO: this only cycles between first 2 fonts.. make it cycle between all
   (interactive)
   (catch 'done
     (dolist (font +sf/preferred-font)
       (unless (string= font +sf/current-font)
         (when (ignore-errors (x-list-fonts font))
           (+sf/set-current-font font)
           (throw 'done nil))))))

;;;###autoload
(defun +sf/set-path ()
  "Set path for themes and packages."
  (add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
  (add-to-list 'custom-theme-load-path (expand-file-name "themes"
                                                         (expand-file-name "themes" user-emacs-directory)))
  (add-to-list 'load-path (expand-file-name "themes" user-emacs-directory))

;;; add everything in themes/ dir to load path
  (let ((default-directory  (expand-file-name "themes" user-emacs-directory)))
    (normal-top-level-add-to-load-path '("."))
    (normal-top-level-add-subdirs-to-load-path)))

(provide 'theme-config)
;;; theme-config.el ends here
