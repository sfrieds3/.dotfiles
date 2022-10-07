;;; embark-defun
;;; --- defun used with embark -*- lexical-binding: t -*-

;;; Commentary:
;;;     defun used with embark
;;; Code:

;;;###autoload
(defun $embark-on-last-message (arg)
    "Act on the last message displayed in the echo area."
    (interactive "P")
    (with-current-buffer "*Messages*"
      (goto-char (1- (point-max)))
      (embark-act arg)))

;;;###autoload
(defun $embark-collect-live-shrink-to-fit (&rest _)
  "Fit live updating collect bufferst to fit contents."
  (when (memq (defvar embark-collect--kind) '(:live :completions))
    (fit-window-to-buffer (get-buffer-window)
                          (floor (frame-height) 2) 1)))

;;;###autoload
(defun $embark-act-noquit ()
  "Run action but do not quit minibuffer."
  (interactive)
  (let ((embark-quit-after-action nil))
    (embark-act)))

(defun $embark-which-key-indicator ()
  "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
        (which-key--hide-popup-ignore-command)
      (which-key--show-keymap
       (if (eq (plist-get (car targets) :type) 'embark-become)
           "Become"
         (format "Act on %s '%s'%s"
                 (plist-get (car targets) :type)
                 (embark--truncate-target (plist-get (car targets) :target))
                 (if (cdr targets) "…" "")))
       (if prefix
           (pcase (lookup-key keymap prefix 'accept-default)
             ((and (pred keymapp) km) km)
             (_ (key-binding prefix 'accept-default)))
         keymap)
       nil nil t (lambda (binding)
                   (not (string-suffix-p "-argument" (cdr binding))))))))

(defun $embark-hide-which-key-indicator (fn &rest args)
  "Hide the which-key indicator immediately when using the completing-read prompter."
  (which-key--hide-popup-ignore-command)
  (let ((embark-indicators
         (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

(provide 'embark-defun)
;;; embark-defun.el ends here
