;;;; scwfri-defun --- org mode personal functions
;;; Commentary:
;;;     personal functions for org mode
;;; Code:

;;;###autoload
(defun $org-table--mark-field ()
  "Mark the current table field."
  (interactive)
  ;; Do not try to jump to the beginning of field if the point is already there
  (when (not (looking-back "|\\s-?"))
    (org-table-beginning-of-field 1))
  (set-mark-command nil)
  (org-table-end-of-field 1))

(provide 'org-defun)
;;; org-defun.el ends here
