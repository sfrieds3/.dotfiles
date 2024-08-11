;;;###autoload
(defun region-active-p ()
  "Return non-nil if selection is active.
Detects evil visual mode as well."
  (declare (side-effect-free t))
  (or (use-region-p)
      (and (bound-and-true-p evil-local-mode)
           (evil-visual-state-p))))

;;;###autoload
(defun $use-custom-src-directory (orig-fn &rest args)
  "Use custom src directory as default directory.
Instead of `default-directory' when calling `ORIG-FN' with `ARGS'."
  (let ((default-directory
         (expand-file-name
          ;; custom-src-directory is supposed to come from .dir-locals.el
          (if (boundp 'custom-src-directory)
              custom-src-directory
            default-directory))))
    (apply orig-fn args))))
