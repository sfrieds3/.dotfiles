;;; debug --- some debug tools -*- lexical-binding: t -*-

;;; Commentary:
;;;     debug.el

;;; Code:
(require 'loadhist)

(defun package-requires (f)
  "Return package which requires F."
  (file-dependents (feature-file f)))

;;; debug.el ends here
