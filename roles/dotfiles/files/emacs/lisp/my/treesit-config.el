;;; treesit-config.el --- some helpers for treesit -*- lexical-binding: t -*-

;;; Commentary:
;;;     treesit helpers

;;; Code:

;;; treesitter
(use-package treesit
  :straight (:type built-in))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (global-treesit-auto-mode))

(use-package evil-textobj-tree-sitter
  :after (evil tree-sitter)
  :config
  (defmacro $inlambda (functionname &rest args)
    "Create an interactive lambda of existing function `FUNCTIONNAME' with `ARGS'."
    (let ((funsymbol (concat "ilambda/" (symbol-name functionname))))
      `(cons ,funsymbol (lambda () (interactive) (apply #',functionname ',args)))))
  (defmacro $ilambda (functionname &rest args)
    "Create an interactive lambda of existing function `FUNCTIONNAME' with `ARGS'."
    `(lambda () (interactive) (apply #',functionname ',args)))

  (define-key evil-outer-text-objects-map "m" (evil-textobj-tree-sitter-get-textobj "import"
                                                '((python-mode . [(import_statement) @import])
                                                  (go-mode . [(import_spec) @import])
                                                  (rust-mode . [(use_declaration) @import]))))
  (define-key evil-outer-text-objects-map "f" (cons "evil-outer-function" (evil-textobj-tree-sitter-get-textobj "function.outer")))
  (define-key evil-inner-text-objects-map "f" (cons "evil-inner-function" (evil-textobj-tree-sitter-get-textobj "function.inner")))
  (define-key evil-outer-text-objects-map "c" (cons "evil-outer-class" (evil-textobj-tree-sitter-get-textobj "class.outer")))
  (define-key evil-inner-text-objects-map "c" (cons "evil-inner-class" (evil-textobj-tree-sitter-get-textobj "class.inner")))
  (define-key evil-outer-text-objects-map "n" (cons "evil-outer-comment" (evil-textobj-tree-sitter-get-textobj "comment.outer")))
  (define-key evil-inner-text-objects-map "n" (cons "evil-outer-comment" (evil-textobj-tree-sitter-get-textobj "comment.outer")))
  (define-key evil-outer-text-objects-map "v" (cons "evil-outer-conditional-loop" (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer"))))
  (define-key evil-inner-text-objects-map "v" (cons "evil-inner-conditional-loop" (evil-textobj-tree-sitter-get-textobj ("conditional.inner" "loop.inner"))))
  (define-key evil-inner-text-objects-map "a" (cons "evil-inner-parameter" (evil-textobj-tree-sitter-get-textobj "parameter.inner")))
  (define-key evil-outer-text-objects-map "a" (cons "evil-outer-parameter" (evil-textobj-tree-sitter-get-textobj "parameter.outer")))

  (define-key evil-normal-state-map (kbd "]a") (cons "goto-parameter-start" ($ilambda evil-textobj-tree-sitter-goto-textobj "parameter.inner")))
  (define-key evil-normal-state-map (kbd "[a") (cons "goto-parameter-start" ($ilambda evil-textobj-tree-sitter-goto-textobj "parameter.inner" t)))
  (define-key evil-normal-state-map (kbd "]A") (cons "goto-parameter-end" ($ilambda evil-textobj-tree-sitter-goto-textobj "parameter.inner" nil t)))
  (define-key evil-normal-state-map (kbd "[A") (cons "goto-parameter-end" ($ilambda evil-textobj-tree-sitter-goto-textobj "parameter.inner" t t)))
  (define-key evil-normal-state-map (kbd "]v") (cons "goto-conditional-start" ($ilambda evil-textobj-tree-sitter-goto-textobj ("conditional.outer" "loop.outer"))))
  (define-key evil-normal-state-map (kbd "[v") (cons "goto-conditional-start" ($ilambda evil-textobj-tree-sitter-goto-textobj ("conditional.outer" "loop.outer") t)))
  (define-key evil-normal-state-map (kbd "]V") (cons "goto-conditional-end" ($ilambda evil-textobj-tree-sitter-goto-textobj ("conditional.outer" "loop.outer") nil t)))
  (define-key evil-normal-state-map (kbd "[V") (cons "goto-conditional-end" ($ilambda evil-textobj-tree-sitter-goto-textobj ("conditional.outer" "loop.outer") t t)))
  (define-key evil-normal-state-map (kbd "]c") (cons "goto-class-start" ($ilambda evil-textobj-tree-sitter-goto-textobj "class.outer")))
  (define-key evil-normal-state-map (kbd "[c") (cons "goto-class-start" ($ilambda evil-textobj-tree-sitter-goto-textobj "class.outer" t)))
  (define-key evil-normal-state-map (kbd "]C") (cons "goto-class-end" ($ilambda evil-textobj-tree-sitter-goto-textobj "class.outer" nil t)))
  (define-key evil-normal-state-map (kbd "[C") (cons "goto-class-end" ($ilambda evil-textobj-tree-sitter-goto-textobj "class.outer" t t)))
  (define-key evil-normal-state-map (kbd "]n") (cons "goto-comment-start" ($ilambda evil-textobj-tree-sitter-goto-textobj "comment.outer")))
  (define-key evil-normal-state-map (kbd "[n") (cons "goto-comment-start" ($ilambda evil-textobj-tree-sitter-goto-textobj "comment.outer" t)))
  (define-key evil-normal-state-map (kbd "]N") (cons "goto-comment-end" ($ilambda evil-textobj-tree-sitter-goto-textobj "comment.outer" nil t)))
  (define-key evil-normal-state-map (kbd "[N") (cons "goto-comment-end" ($ilambda evil-textobj-tree-sitter-goto-textobj "comment.outer" t t)))
  (define-key evil-normal-state-map (kbd "]f") (cons "goto-function-start" (lambda () (interactive) (progn (evil-textobj-tree-sitter-goto-textobj "function.outer") (reposition-window)))))
  (define-key evil-normal-state-map (kbd "[f") (cons "goto-function-start" (lambda () (interactive) (progn (evil-textobj-tree-sitter-goto-textobj "function.outer" t) (reposition-window)))))
  (define-key evil-normal-state-map (kbd "]F") (cons "goto-function-end" (lambda () (interactive) (progn (evil-textobj-tree-sitter-goto-textobj "function.outer" nil t) (reposition-window)))))
  (define-key evil-normal-state-map (kbd "[F") (cons "goto-function-end" (lambda () (interactive) (progn (evil-textobj-tree-sitter-goto-textobj "function.outer" t t) (reposition-window))))))

;; treesitter fold
(use-package ts-fold
  :blackout
  :straight (ts-fold :type git :host github :repo "emacs-tree-sitter/ts-fold")
  :init
  (global-ts-fold-mode)
  (global-ts-fold-indicators-mode))


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
    (if-let ((parent (treesit-parent-until
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
