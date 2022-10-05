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

(defun $current-candidate+category ()
  "TODO: add docstring."
  (when (defvar selectrum-active-p)
    (cons (selectrum--get-meta 'category)
          (selectrum-get-current-candidate))))

;;;###autoload
(defun $current-candidates+category ()
  "TODO: add docstring."
  (when (defvar selectrum-active-p)
    (cons (selectrum--get-meta 'category)
          (selectrum-get-current-candidates
           ;; Pass relative file names for dired.
           minibuffer-completing-file-name))))

;;;###autoload
(defun $embark-shrink-selectrum ()
  "Shrink selectrum to one line when embark-collect-live enabled."
  (when (eq (defvar embark-collect--kind) :live)
    (with-selected-window (active-minibuffer-window)
      (setq-local selectrum-num-candidates-displayed 1)
      (setq-local selectrum-display-style
                  '(horizontal :before-candidates "["
                               :after-candidates "]"
                               :more-candidates ""
                               :candidates-separator "")))))
(provide 'embark-defun)
;;; embark-defun ends here
