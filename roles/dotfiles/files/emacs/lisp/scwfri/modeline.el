;;; modeline.el --- modeline config -*- lexical-binding: t -*-

;;; Commentary:
;;;     modeline.. inspiration:
;;;     https://github.com/peterwu/dotfiles/blob/9ac6c212f6a57650e39703ad79c5d45592deabe3/emacs/mine/my-mode-line.el

;;; Code:

;;; mode-line
(defun $ellipsize-file-name (file-name max-length)
  "Elipsize FILE-NAME if over MAX-LENGTH."
  (let* ((ellipsis (if (char-displayable-p ?…) "…" "..."))
         (left (/ max-length 2))
         (center (length ellipsis))
         (right (- max-length left center)))
    (if (> (length file-name) max-length)
        (concat
         (substring file-name 0 (1- left))
         " "
         ellipsis
         " "
         (substring file-name (- (length file-name) (1- right))))
      file-name)))

(defun $left-right-mode-line-render (left right)
  "Return a string of `window-width' length.
Containing LEFT, and RIGHT aligned respectively."
  (let ((available-width
         (- (window-total-width)
            (+ (length (format-mode-line left))
               (length (format-mode-line right))))))
    (append left
            (list (format (format "%%%ds" available-width) ""))
            right)))

(defun $mode-line-render (left center right)
  "Return a string of `window-total-width' length.
Containing LEFT, CENTER and RIGHT aligned respectively."
  (let* ((left-width (string-width (format-mode-line left)))
         (center-width (string-width (format-mode-line center)))
         (right-width (string-width (format-mode-line right)))
         (available-width-left
          (- (/
              (- (window-total-width) center-width)
              2)
             left-width))
         (available-width-right
          (- (window-total-width)
             left-width
             available-width-left
             center-width
             right-width)))
    (cond ((> (+ left-width right-width) (window-total-width))
           left)
          ((> (+ left-width center-width right-width) (window-total-width))
           ($left-right-mode-line-render left right))
          (t (append left
                     (list (format (format "%%%ds" available-width-left) ""))
                     center
                     (list (format (format "%%%ds" available-width-right) ""))
                     right)))))

(defvar-local $mode-line-tab-bar-indicator
  '(:eval
    (let ((tab-name (alist-get 'name (assq 'current-tab (funcall tab-bar-tabs-function)))))
      (propertize
       tab-name
       'help-echo "Tab name"
       'face '(:inherit mode-line-buffer-id)
       'mouse-face 'mode-line-highlight))))
(put '$mode-line-tab-bar-indicator 'risky-local-variable t)

(defvar-local $mode-line-buffer-identification
  '(:eval (cond ((buffer-file-name)
              (let* (($buffer-name (cond ((projectile-project-root)
                                        (file-relative-name buffer-file-name (projectile-project-root)))
                                       (t ($ellipsize-file-name
                                           (abbreviate-file-name (buffer-file-name))
                                           36)))))
                                       (propertize $buffer-name
                                                   'help-echo (abbreviate-file-name (buffer-file-name))
                                                   'face '(:inherit mode-line-emphasis)
                                                   'mouse-face 'mode-line-highlight)))
            (t (propertize (buffer-name)
                        'help-echo "Buffer name"
                        'face '(:inherit mode-line-buffer-id)
                        'mouse-face 'mode-line-highlight)))))
(put '$mode-line-buffer-identification 'risky-local-variable t)

(defvar $mode-line--vc-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] 'vc-diff)
    (define-key map [mode-line down-mouse-3] 'vc-root-diff)
    map))

(defun $mode-line--vc-help-echo (file)
  "Return `help-echo' for FILE under vc."
  (format "\nRevision: %s\nmouse-1: `vc-diff'\nmouse-3: `vc-root-diff'"
          (vc-working-revision file)))

(defvar-local $mode-line-git-status
  ;; Format: (defun vc-default-mode-line-string (backend file) in vc-hooks.el
  ;;   \"BACKEND-REV\"        if the file is up-to-date
  ;;   \"BACKEND:REV\"        if the file is edited (or locked by the calling user)
  ;;   \"BACKEND:LOCKER:REV\" if the file is locked by somebody else
  ;;   \"BACKEND@REV\"        if the file was locally added
  ;;   \"BACKEND!REV\"        if the file contains conflicts or was removed
  ;;   \"BACKEND?REV\"        if the file is under VC, but is missing

  '(:eval (when vc-mode
            (let* ((git-status (vc-git-mode-line-string (buffer-file-name)))
                   (status (replace-regexp-in-string "^Git" "" git-status))
                   (class (substring-no-properties status 0 1))
                   (locked? (string-match
                             (rx (and
                                  line-start ":"
                                  (one-or-more alnum) ":"
                                  (group (one-or-more alnum))))
                             status))
                   (branch
                    (if locked?  (match-string 1 status)
                      (substring status 1)))
                   (git-mode-line-status (concat " λ: " branch " ")))
              (cond
               ;; up-to-date
               ((string-equal "-" class)
                (propertize git-mode-line-status
                            'help-echo (concat branch ": up-to-date" ($mode-line--vc-help-echo buffer-file-name))
                            'face '(:foreground "dark cyan" :weight bold)
                            'mouse-face 'mode-line-highlight))
               ;; locked
               (locked?
                (propertize git-mode-line-status
                            'help-echo (concat branch ": locked" ($mode-line--vc-help-echo buffer-file-name))
                            'face '(:foreground "dark orange" :weight bold)
                            'mouse-face 'mode-line-highlight))
               ;; modified
               ((string-equal ":" class)
                (propertize git-mode-line-status
                            'help-echo (concat branch ": modified" ($mode-line--vc-help-echo buffer-file-name))
                            'face '(:foreground "yellow" :weight bold)
                            'mouse-face 'mode-line-highlight
                            'local-map $mode-line--vc-map))
               ;; locally added
               ((string-equal "@" class)
                (propertize git-mode-line-status
                            'help-echo (concat branch ": locally added file" ($mode-line--vc-help-echo buffer-file-name))
                            'face '(:foreground "dark blue" :weight bold)
                            'mouse-face 'mode-line-highlight
                            'local-map $mode-line--vc-map))
               ;; removed or conflicting
               ((string-equal "!" class)
                (propertize git-mode-line-status
                            'help-echo (concat branch ": removed file or conflicts" ($mode-line--vc-help-echo buffer-file-name))
                            'face '(:background "yellow" :foreground "dark red" :weight bold)
                            'mouse-face 'mode-line-highlight
                            'local-map $mode-line--vc-map))
               ;; missing
               ((string-equal "?" class)
                (propertize git-mode-line-status
                            'help-echo (concat branch ": missing" ($mode-line--vc-help-echo buffer-file-name))
                            'face '(:foreground "dark red" :weight bold)
                            'mouse-face 'mode-line-highlight
                            'local-map $mode-line--vc-map))
               ((t git-mode-line-status)))))))
(put '$mode-line-git-status 'risky-local-variable t)

(defvar-local $mode-line-center-placeholder "")
(put '$mode-line-center-placeholder 'risky-local-variable t)

(defvar-local $mode-line-position
  '(:propertize "(%l,%C)"
                help-echo "(Line,Column)"
                mouse-face mode-line-highlight))
(put '$mode-line-position 'risky-local-variable t)

(defvar-local $mode-line-buffer-size
  '(:propertize "%I"
                help-echo "Size"
                mouse-face mode-line-highlight))
(put '$mode-line-buffer-size 'risky-local-variable t)

(defvar-local $mode-line-percent-position
  '(:eval (let ((p (format-mode-line "%p")))
            (cond
             ((string-equal p "All")
              (propertize "All" 'help-echo p 'mouse-face 'mode-line-highlight))
             ((string-equal p "Top")
              (propertize "Top" 'help-echo p 'mouse-face 'mode-line-highlight))
             ((string-equal p "Bottom")
              (propertize "Bot" 'help-echo p 'mouse-face 'mode-line-highlight))
             (t
              (propertize (concat p "%%")
                          'help-echo "Position"
                          'mouse-face 'mode-line-highlight))))))
(put '$mode-line-percent-position 'risky-local-variable t)

(setq-default mode-line-format
              '(:eval
                (cond ((mode-line-window-selected-p)
                       ($mode-line-render
                        ;; left hand side
                        '("%e"
                          evil-mode-line-tag
                          " "
                          mode-line-mule-info
                          mode-line-modified
                          " "
                          $mode-line-buffer-identification)

                        ;; center
                        '(mode-line-modes
                          mode-line-misc-info)

                        ;; right hand side
                        '($mode-line-git-status
                          $mode-line-position
                          " "
                          $mode-line-buffer-size
                          " "
                          $mode-line-percent-position
                          " ")))
                      (t ($mode-line-render
                            nil
                            '($mode-line-buffer-identification
                              " "
                              $mode-line-percent-position)
                            nil)))))

(provide 'modeline)
;;; modeline.el ends here
