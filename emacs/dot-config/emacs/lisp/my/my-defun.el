;;; my-defun --- personal functions -*- lexical-binding: t -*-

;;; Commentary:
;;;     personal functions

;;; Code:

;;;###autoload
(defun +sf/insert-zero-width-space ()
  "Insert zero width space."
  (interactive)
  (insert-char ?\u200B))

;;;###autoload
(defun +sf/symbol-at-point ()
  "Return current symbol at point as a string."
  (let ((s (thing-at-point 'symbol)))
    (and (stringp s)
         (if (string-match "\\`[`']?\\(.*?\\)'?\\'" s)
             (match-string 1 s)
           s))))

;;;###autoload
(defun +sf/profile-session ()
  "Easily toggle Emacs profiler."
  (interactive)
  (require 'profiler)
  (if (profiler-running-p)
      (progn (profiler-stop) (profiler-report))
    (profiler-start 'cpu)))

;;;###autoload
(defun +sf/eval-defun-view-results ()
  "Eval defun and view results in a new buffer."
  (interactive)
  (let ((result (pp-to-string (eval-defun nil))))
    (with-current-buffer
        (get-buffer-create "*ELISP RESULT*")
      (delete-region (point-min) (point-max))
      (insert result)
      (switch-to-buffer-other-window (current-buffer)))))

;;;###autoload
(defun +sf/toggle-show-trailing-whitespace ()
  "Toggle 'show-trailing-whitespace'."
  (interactive)
  (if (eq 1 show-trailing-whitespace)
      (progn
        (setq show-trailing-whitespace nil)
        (message "show-trailing-whitespace nil"))
    (progn
      (setq show-trailing-whitespace 1)
      (message "show-trailing-whitespace t"))))

;;;###autoload
(defun +sf/show-full-file-path ()
  "Show full file path in msg."
  (interactive)
  (message "%s" (buffer-file-name)))

;;;###autoload
(defun +sf/what-face (pos)
  "Return face under point POS."
  (interactive "d")
  (let ((face (or (get-char-property (pos) 'read-face-name)
                  (get-char-property (pos) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

;;;###autoload
(defun +sf/dir-grep ()
  "Run grep recursively from the directory of the current buffer or the default directory."
  (interactive)
  (let ((dir (file-name-directory (or load-file-name buffer-file-name default-directory))))
    (let ((command (read-from-minibuffer "Run grep (like this): "
                                         (cons (concat "grep --color --null -nH -ir -e  " dir) 32))))
      (grep command))))

;;;###autoload
(defun +sf/file-grep ()
  "Run grep in the current file."
  (interactive)
  (let ((fname (buffer-file-name)))
    (let ((command (read-from-minibuffer "Run grep (like this): "
                                         (cons (concat "grep --color --null -nH -ir -e  " fname) 32))))
      (grep command))))

;;;###autoload
(defun +sf/revert-buffer-noconfirm ()
  "Call `revert-buffer' with the NOCONFIRM argument set."
  (interactive)
  (revert-buffer nil t))

;;;###autoload
(defun +sf/diff-hl--update ()
  "Update diff-hl"
  (interactive)
  (diff-hl-update))

;;;###autoload
(defun +sf/revert-buffer-noconfirm-and-update-diff-hl ()
  "Call `revert-buffer' with the NOCONFIRM argument set."
  (interactive)
  (+sf/revert-buffer-noconfirm)
  (+sf/diff-hl--update))

;;;###autoload
(defun +sf/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;;;###autoload
(defun +sf/goto-match-paren (arg)
  "Go to the matching parenthesis if ARG on parenthesis, otherwise insert %.
vi style of % jumping to matching brace."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1))
        (t
         (backward-char 1)
         (cond ((looking-at "\\s\)")
                (forward-char 1) (backward-list 1))
               (t
                (while (not (looking-at "\\s("))
                  (backward-char 1)
                  (cond ((looking-at "\\s\)")
                         (message "->> )")
                         (forward-char 1)
                         (backward-list 1)
                         (backward-char 1)))))))))

;;;###autoload
(defun +sf/kill-back-to-indent ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (+sf/smarter-move-beginning-of-line nil)
    (kill-region (point) prev-pos)))

;;;###autoload
(defun +sf/delete-trailing-whitespace ()
  "Delete trailing whitespace, and echo."
  (interactive)
  (delete-trailing-whitespace)
  (message "trailing whitespace deleted..."))

;; source: https://emacs.stackexchange.com/questions/51972/possible-to-use-emacs-undo-redo-without-keyboard-quit-ctrl-g/54142#54142
;;;###autoload
(defun +sf/simple-redo ()
  "Simple redo function."
  (interactive)
  (let
      ((last-command
        (cond
         ;; Break undo chain, avoid having to press Ctrl-G.
         ((string= last-command '+sf/simple-undo) 'ignore)
         ;; Emacs undo uses this to detect successive undo calls.
         ((string= last-command '+sf/simple-redo) 'undo)
         (t last-command))))
    (condition-case err
        (progn
          (undo) t)
      (error
       (message "%s" (error-message-string err)))))
  (setq this-command '+sf/simple-redo))

;;;###autoload
(defun +sf/simple-undo ()
  "Simple undo function."
  (interactive)
  (let
      ((last-command
        (cond
         ;; Emacs undo uses this to detect successive undo calls.
         ((string= last-command '+sf/simple-undo) 'undo)
         ((string= last-command '+sf/simple-redo) 'undo)
         (t last-command))))
    (condition-case err
        (progn
          (undo-only) t)
      (error
       (message "%s" (error-message-string err)))))
  (setq this-command '+sf/simple-undo))

;;;###autoload
(defun +sf/load-theme--disable-current-theme (theme &rest args)
  "Disable the current THEME before loading a new one."
  (mapcar #'disable-theme custom-enabled-themes))

;;;###autoload
(defun +sf/narrow-or-widen-dwim (p)
  "If the buffer is narrowed, it widens.  Otherwise, it narrows intelligently.
Intelligently means: region, subtree, or defun, whichever applies
first.

With prefix P, don't widen, just narrow even if buffer is already
narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode) (org-narrow-to-subtree))
        (t (narrow-to-defun))))

(global-set-key (kbd "C-x n x") '+sf/narrow-or-widen-dwim)

;;;###autoload
(defun +sf/unfill-paragraph ()
  "Convert a multi-line paragraph into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;;;###autoload
(defun +sf/kill-and-delete-window ()
  "Kill and delete current window."
  (interactive)
  (kill-this-buffer)
  (delete-window))

;;;###autoload
(defun +sf/dont-kill-scratch ()
  "Never kill scratch buffer."
  (if (not (equal (buffer-name) "*scratch*"))
      t
    (message "Not allowed to kill %s, burying instead" (buffer-name))
    (bury-buffer)
    nil))

(add-hook 'kill-buffer-query-functions  #'+sf/dont-kill-scratch)

;;;###autoload
(defun +sf/dont-kill-messages ()
  "Never kill messages bufffer."
  (if (not (equal (buffer-name) "*Messages*"))
      t
    (message "Not allowed to kill %s, burying instead" (buffer-name))
    (bury-buffer)
    nil))

(add-hook 'kill-buffer-query-functions  #'+sf/dont-kill-messages)

;;;###autoload
(defun +sf/scroll-down-in-place (n)
  "Scroll down N lines, keeping cursor postion."
  (interactive "p")
  (forward-line (- n))
  (scroll-down n))

;;;###autoload
(defun +sf/scroll-up-in-place (n)
  "Scroll up N lines, keeping cursor position."
  (interactive "p")
  (forward-line n)
  (scroll-up n))

;;;###autoload
(defun +sf/scroll-down (n)
  "Scroll down N lines."
  (interactive "p")
  (scroll-down n))

;;;###autoload
(defun +sf/scroll-up (n)
  "Scroll up N lines."
  (interactive "p")
  (scroll-up n))

;;;###autoload
(defun +sf/scroll-up-multiline ()
  "Scroll up multiple lines."
  (interactive)
  (scroll-up (/ (window-body-height) 3)))

;;;###autoload
(defun +sf/scroll-down-multiline ()
  "Scroll up multiple lines."
  (interactive)
  (scroll-down (/ (window-body-height) 3)))

;;;###autoload
(defun +sf/toggle-var (var)
  "Toggle variable VAR."
  (interactive
   (let* ((def  (variable-at-point))
          (def  (and def
                     (not (numberp def))
                     (memq (symbol-value def) '(nil t))
                     (symbol-name def))))
     (list
      (completing-read
       "Toggle value of variable: "
       obarray (lambda (c)
                 (unless (symbolp c) (setq c  (intern c)))
                 (and (boundp c)  (memq (symbol-value c) '(nil t))))
       'must-confirm nil 'variable-name-history def))))
  (let ((sym  (intern var)))
    (set sym (not (symbol-value sym)))
    (message "`%s' is now `%s'" var (symbol-value sym))))

;;;###autoload
(defun +sf/swap-windows ()
  "If you have 2 windows, it swaps them.
from: https://sites.google.com/site/steveyegge2/my-dot-emacs-file."
  (interactive)
  (cond ((not (= (count-windows) 2))
         (message "You need exactly 2 windows to do this."))
        (t
         (let* ((w1 (first (window-list)))
	        (w2 (second (window-list)))
	        (b1 (window-buffer w1))
	        (b2 (window-buffer w2))
	        (s1 (window-start w1))
	        (s2 (window-start w2)))
           (set-window-buffer w1 b2)
           (set-window-buffer w2 b1)
           (set-window-start w1 s2)
           (set-window-start w2 s1)))))

;;;###autoload
(defun +sf/rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME." (interactive "sNew name: ")
  (let ((name (buffer-name))
	(filename (buffer-file-name)))
    (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
	  (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

;;;###autoload
(defun +sf/move-buffer-file (dir)
  "Move both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
	 (filename (buffer-file-name))
	 (dir
	  (if (string-match dir "\\(?:/\\|\\\\)$")
	      (substring dir 0 -1) dir))
	 (newname (concat dir "/" name)))

    (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
      (progn 	(copy-file filename newname 1) 	(delete-file filename) 	(set-visited-file-name newname) 	(set-buffer-modified-p nil) 	t))))

;;;###autoload
(defun +sf/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

;;;###autoload
(defun +sf/unpop-to-mark-command ()
  "Unpop off mark ring.  Does nothing if mark ring is empty."
  (interactive)
  (when mark-ring
    (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
    (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
    (when (null (mark t)) (ding))
    (setq mark-ring (nbutlast mark-ring))
    (goto-char (marker-position (car (last mark-ring))))))

;;;###autoload
(defun +sf/indirect-region (beg end name)
  "Open new named indirect buffer NAME, narrowed to region [BEG, END]."
  (interactive "r\nsname of narrowed buffer: ")
  (let ((new-buff
         (make-indirect-buffer (current-buffer)
                               (generate-new-buffer-name name)
                               t)))
    (switch-to-buffer new-buff nil t)
    (narrow-to-region beg end)))

;;;###autoload
(defun +sf/isearch-highlight-phrase ()
  "Invoke `highligh-phrase' from within isearch."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (highlight-phrase (if isearch-regexp
                          isearch-string
                        (regexp-quote isearch-string)))))

(defun +sf/remap-mark-command (command &optional map)
  "Remap a mark-* COMMAND to temporarily activate Transient Mark mode.
If a MAP is passed, update for that map."
  (let* ((cmd (symbol-name command))
         (fun (intern (concat "$" cmd)))
         (doc (concat "Call `"
                      cmd
                      "' and temporarily activate Transient Mark mode.")))
    (fset fun `(lambda ()
                 ,doc
                 (interactive)
                 (call-interactively #',command)
                 (activate-mark)))
    (if map
        (define-key map (vector 'remap command) fun)
      (global-set-key (vector 'remap command) fun))))

(defun +sf/kill-region-or-backward-word ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))

(defun +sf/kill-to-beginning-of-line ()
  (interactive)
  (kill-region (save-excursion (beginning-of-line) (point))
               (point)))

(defun +sf/copy-to-end-of-line ()
  (interactive)
  (kill-ring-save (point)
                  (line-end-position))
  (message "Copied to end of line"))

(defun $copy-whole-lines (arg)
  "Copy lines (as many as prefix ARG) in the kill ring."
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

(defun $copy-line (arg)
  "Copy to end of line, or as many lines as prefix ARG."
  (interactive "P")
  (if (null arg)
      (copy-to-end-of-line)
    (copy-whole-lines (prefix-numeric-value arg))))

(defun +sf/save-region-or-current-line (arg)
  "Copy current line or region (if ARG)."
  (interactive "P")
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (copy-line arg)))

(defun +sf/kill-and-retry-line ()
  "Kill the entire current line and reposition point at indentation."
  (interactive)
  (back-to-indentation)
  (kill-line))

(defun +sf/open-line-below ()
  "Add new line below current line."
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun +sf/open-line-above ()
  "Add new line above current line."
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(defun +sf/join-next-line ()
  "Join current line with next line."
  (interactive)
  (join-line -1))

(defun +sf/toggle-window-split ()
  "Toggle between horizontal and vertial layout of two windows."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun +sf/rotate-windows ()
  "Rotate windows."
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

(defun +sf/move-line-down ()
  "Move current line down."
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun +sf/move-line-up ()
  "Move current line up."
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (forward-line -2)
    (move-to-column col)))

(defun +sf/pulse-line (&rest _)
  "Pulse the current line."
  (interactive)
  (pulse-momentary-highlight-one-line (point)))

(defun +sf/native-comp-available ()
  "Return message if compiled with --with-native-compilation."
  (interactive)
  (if (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
      (message "Native compilation is available")
    (message "Native complation is *not* available")))

(defun +sf/json-available ()
  "Return message if compiled with --with-json."
  (interactive)
  (if (functionp 'json-serialize)
      (message "Native JSON is available")
    (message "Native JSON is *not* available")))


(provide 'my-defun)
;;; my-defun.el ends here
