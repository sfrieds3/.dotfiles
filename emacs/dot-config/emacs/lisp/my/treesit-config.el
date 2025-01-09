;;; treesit-config.el --- some helpers for treesit -*- lexical-binding: t -*-

;;; Commentary:
;;;     treesit helpers

;;; Code:

(setq $treesit--class-like '((rust-mode . (impl_item))
                             (python-mode . (class_definition))))
(setq $treesit--function-like '((rust-mode . (function_item))
                                (go-mode . (function_declaration method_declaration))
                                (sh-mode . (function_definition))
                                (python-mode . (function_definition))))

(defun $treesit--thing-name (kind)
  "Get name of treesit KIND thing."
  (when-let ((bound-and-true-p (string-match-p (regexp-quote "-ts-") (symbol-name major-mode)))
             (node-types (pcase kind
                           ('class-like $tree-sitter-class-like)
                           ('function-like $tree-sitter-function-like)))
             (node-at-point (cl-some #'tree-sitter-node-at-point
                                     (alist-get major-mode node-types)))
             (node-name (tsc-get-child-by-field node-at-point :name)))
    (tsc-node-text node-name)))

;; Connect to which-function for magit-log-trace-definition
(setq which-func-functions
      (list
       (lambda () ($treesit--thing-name 'function-like))
       (lambda () ($treesit--thing-name 'class-like))))

(defun $json--path-to-position (pos)
  "Return the JSON path from the document's root to the element at POS.

The path is represented as a list of strings and integers,
corresponding to the object keys and array indices that lead from
the root to the element at POS."
  (named-let loop ((node (treesit-node-at pos)) (acc nil))
    (if-let* ((parent (treesit-parent-until
                      node
                      (lambda (n)
                        (member (treesit-node-type n)
                                '("pair" "array"))))))
        (loop parent
              (cons
               (pcase (treesit-node-type parent)
                 ("pair"
                  (treesit-node-text
                   (treesit-node-child (treesit-node-child parent 0) 1) t))
                 ("array"
                  (named-let check ((i 1))
                    (if (< pos (treesit-node-end (treesit-node-child parent i)))
                        (/ (1- i) 2)
                      (check (+ i 2))))))
               acc))
      acc)))

(defun $json--path-at-point (point &optional kill)
  "Display the JSON path at POINT.  When KILL is non-nil, kill it too.

Interactively, POINT is point and KILL is the prefix argument."
  (interactive "d\nP" json-ts-mode)
  (let ((path (mapconcat (lambda (o) (format "%s" o))
                         ($json--path-to-position point)
                         ".")))
    (if kill
        (progn (kill-new path) (message "Copied: %s" path))
      (message path))
    path))

(provide 'treesit-config)
;;; treesit-config.el ends here
