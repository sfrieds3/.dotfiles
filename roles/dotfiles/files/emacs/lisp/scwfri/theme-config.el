;;;; theme-config --- configure theme-related items

;;; Commentary:
;;;     theme configurations

;;; Code:

;;(advice-add 'load-theme :before #'$load-theme--disable-current-theme)

;; define theme here
(defun $set-preferred-theme ()
  "Set theme here.. loaded after init."
  (load-theme 'material t))

;;; default preferred font-sizes
(defvar $default-font-size
  (cond ((string= (system-name) "mixolydian") "10")
        ((string= (system-name) "phrygian") "14")
        (t "12")))

;;; list of preferred fonts
(defvar $preferred-font
  '("SauceCodePro Nerd Font Mono"
    "Iosevka Fixed SS14"
    "DejaVu Sans Mono"))

;;; current font name.. set by $set-current-font
(defvar $current-font nil)

;;;###autoload
(defun $set-frame-font--update-current (font &optional keep-size frames)
  "TODO: docstring FONT KEEP-SIZE FRAMES."
  ;; TODO: strip trailing size to *really* set the font
  (setf $current-font font))

;;; make sure we update the current font after set-frame-font called
(advice-add 'set-frame-font :after #'$set-frame-font--update-current)

;;;###autoload
(defun $set-current-font (font &optional size)
  "Set current font to FONT and SIZE."
  (let* ((font-size (or size $default-font-size))
        (new-font (string-join `(,font ,font-size) " ")))
       (set-frame-font new-font nil t)))

;;;###autoload
(defun $set-preferred-font (&optional frame)
  "Set preferred font and size for FRAME."
  (interactive)
  (catch 'done
    (with-selected-frame (or frame (selected-frame))
      (dolist (font $preferred-font)
        (when (ignore-errors (x-list-fonts font))
          ($set-current-font font)
          (throw 'done nil))))))

;;;###autoload
 (defun $cycle-preferred-font ()
   "Cycle through preferred fonts."
   ;; TODO: this only cycles between first 2 fonts.. make it cycle between all
   (interactive)
   (catch 'done
     (dolist (font $preferred-font)
       (unless (string= font $current-font)
         (when (ignore-errors (x-list-fonts font))
           ($set-current-font font)
           (throw 'done nil))))))

;;;###autoload
(defun $set-path ()
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
