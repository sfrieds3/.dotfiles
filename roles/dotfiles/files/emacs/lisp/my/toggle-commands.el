;;; toggle-commands --- toggle certain commands -*- lexical-binding: t -*-
;;; Commentary:
;;; Commands used to toggle certain commands
;;; Code:

(require 'theme-config)

;;; testing out random unnecessary macro
(defmacro $load-var-theme (theme)
  "Load THEME with given string name."
  `(load-theme ',(car (read-from-string theme)) t))

(defvar $themes)
(put '$themes :themes ["standard-dark" "doom-monokai-pro" "doom-material-dark" "doom-miramare" "zerodark" "doom-nord" "doom-1337" "doom-xcode" "doom-gruvbox" "modus-vivendi" "ef-duo-dark" "ef-trio-dark"])

(defvar $fonts)
(put '$fonts :fonts ["Hack Nerd Font" "JetBrainsMono Nerd Font" "SauceCodePro Nerd Font" "FuraMono Nerd Font" "VictorMono Nerd Font"])

;;; TODO finish this
;; (defmacro $cycle: (cycle-var load-fun)
;;   "Generate $cycle: (CYCLE-VAR LOAD-FUN) function.
;; Cycle through CYCLE-VAR and use LOAD-FUN to load var."
;;   (cl-flet ((symcat (a b) (intern (concat a (symbol-name b)))))
;;     `(defun ,(symcat "$cycle:" cycle-var) (arg)
;;        (interactive "p")
;;        (let* ((cycle-list (get ,(quote cycle-var) ,(concat ":" ,(symbol-name cycle-var))))
;;               (idx (if ,(get ,(quote cycle-var :state))
;;                        ,(get (quote cycle-var) :state)
;;                      0))
;;               (next-idx (% (+ idx arg (length cycle-list)) (length cycle-list)))
;;               (new-var (aref cycle-list next-idx)))
;;          (put (quote cycle-var :state next-idx))
;;          (load-fun (car (read-from-string new-var)))
;;          (message "Loaded %s: %s" (symbol-name cycle-var) new-var)))))

;;; idea: http://xahlee.info/emacs/emacs/elisp_toggle_command.html
;;;###autoload
(defun $cycle-theme (arg)
  "Cycle colorschemes through preset list.  ARG is prefix argument, default 1."
  (interactive "p")
  (let* ((themes (get '$themes :themes))
         (idx (if (get '$themes :state)
                  (get '$themes :state)
                0))
         (next-idx (% (+ idx arg (length themes)) (length themes)))
         (new-theme (aref themes next-idx)))
    (put '$themes :state next-idx)
    (load-theme (car (read-from-string new-theme)) t)
    (message "Loaded colorscheme: %s" new-theme)))

;;;###autoload
(defun $describe-current-theme ()
  "Echo current colorscheme."
  (interactive)
  (let* ((themes (get '$themes :themes))
         (idx (if (get '$themes :state)
                  (get '$themes :state)
                0)))
    (message "Theme: %s" (aref themes idx))))

;;;###autoload
(defun $cycle-font (arg)
  "Cycle through fonts.  ARG is prefix arg, default 1."
  (interactive "p")
  (let* ((fonts (get '$fonts :fonts))
         (idx (if (get '$fonts :state)
                  (get '$fonts :state)
                0))
         (next-idx (% (+ idx arg (length fonts)) (length fonts)))
         (next-font (aref fonts next-idx)))
    (put '$fonts :state next-idx)
    ($set-current-font next-font)
    (message "Font: %s" next-font)))

;;;###autoload
(defun $describe-current-font ()
  "Echo current font."
  (interactive)
  (let* ((fonts (get '$fonts :fonts))
         (idx (if (get '$fonts :state)
                  (get '$fonts :state)
                0)))
    (message "Font: %s" (aref fonts idx))))

(provide 'toggle-commands)
;;; toggle-commands.el ends here
