;;;; tramp-config --- configure tramp

;;; Commentary:
;;;     configuration for tramp

;;; Code:

;;;###autoload
(defun $add-server-postfix ()
  "Add the name of the connection type and server to the buffer name."
  (if (string-match "^/ssh:.*?:" (buffer-file-name (current-buffer)))
      (rename-buffer (concat (buffer-name (current-buffer)) "<" (match-string 0 (buffer-file-name (current-buffer))) ">")) nil))

(add-hook 'find-file-hook  #'$add-server-postfix)

(provide 'tramp-config)
;;; tramp-config.el ends here
