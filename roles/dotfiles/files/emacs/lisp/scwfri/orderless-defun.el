;;; orderless-defun --- defun used with orderless -*- lexical-binding: t -*-

;;; Commentary:
;;;     defun used with orderless
;;; Code:

;;; source: https://github.com/oantolin/emacs-config/blob/12a5ca1c379625aa2409af9be21a7ccf70eabc28/init.el
(defmacro $dispatch: (regexp style)
  "Generate 'Dispatch: (REGEXP STYLE) orderless style dispatcher function."
  (cl-flet ((symcat (a b) (intern (concat a (symbol-name b)))))
    `(defun ,(symcat "$dispatch:" style) (pattern _index _total)
       (when (string-match ,regexp pattern)
         (cons ',(symcat "orderless-" style) (match-string 1 pattern))))))

;;; create dispatchers
(cl-flet ((pre/post (str) (format "^%s\\(.*\\)$\\|^\\(?1:.*\\)%s$" str str)))
  ($dispatch: (pre/post "=") literal)
  ($dispatch: (pre/post "/") regexp)
  ($dispatch: (pre/post "!") without-literal)
  ($dispatch: (pre/post (if (or minibuffer-completing-file-name
                                (derived-mode-p 'eshell-mode))
                            "," "[,.]"))
              initialism))
($dispatch: "^{\\(.*\\)}$" flex)
($dispatch: "^\\([^][^\\+*]*[./-][^][\\+*$]*\\)$" prefixes)

;;; DEPRECATED in favor of above
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
;;; orderless-defun.el ends here
