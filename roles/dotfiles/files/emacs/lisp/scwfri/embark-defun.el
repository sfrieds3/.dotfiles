;;;; embark-defun --- defun used with embark -*- lexical-binding: t -*-

;;; Commentary:
;;;     defun used with embark
;;; Code:

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

(provide 'embark-defun)
;;; embark-defun.el ends here
