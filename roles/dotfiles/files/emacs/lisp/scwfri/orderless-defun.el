;;;; orderless-defun --- defun used with orderless

;;; Commentary:
;;;     defun used with orderless
;;; Code:

;;;###autoload
(defun $orderless-flex (pattern _index _total)
  "TODO: add docstring (PATTERN _INDEX _TOTAL)."
  (when (string-suffix-p "~" pattern)
    `(orderless-flex . ,(substring pattern 0 -1))))

;;;###autoload
(defun $orderless-initialism (pattern _index _total)
  "TODO: add docstring (PATTERN _INDEX _TOTAL)."
  (when (string-suffix-p "," pattern)
    `(orderless-initialism . ,(substring pattern 0 -1))))

;;;###autoload
(defun $orderless-literal (pattern index _total)
  "TODO: add docstring (PATTERN _INDEX _TOTAL)."
  (when (string-suffix-p "=" pattern)
    `(orderless-literal . ,(substring pattern 0 -1))))

;;;###autoload
(defun $orderless-regexp (pattern index _total)
  "TODO: add docstring (PATTERN _INDEX _TOTAL)."
  (when (string-suffix-p "/" pattern)
    `(orderless-regexp . ,(substring pattern 0 -1))))

;;;###autoload
(defun $orderless-strict-leading-initialism (pattern index _total)
  "TODO: add docstring (PATTERN _INDEX _TOTAL)."
  (when (string-suffix-p "\\" pattern)
    `(orderless-strict-leading-initialism . ,(substring pattern 0 -1))))

;;;###autoload
(defun $orderless-without-literal (pattern _index _total)
  "TODO: add docstring (PATTERN _INDEX _TOTAL)."
  (when (string-prefix-p "!" pattern)
    `(orderless-without-literal . ,(substring pattern 1))))

;;;###autoload
(defun $match-components-literally ()
  "Components match literally for the rest of the session."
  (interactive)
  (setq-local orderless-matching-styles '(orderless-literal))
  (setq-local orderless-style-dispatchers nil))

(provide 'orderless-defun)
;;; orderless-defun ends here
