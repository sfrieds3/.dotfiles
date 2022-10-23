;;; toggle-commands --- toggle certain commands -*- lexical-binding: t -*-
;;; Commentary:
;;; Commands used to toggle certain commands
;;; Code:

(require 'theme-config)

;;; testing out random unnecessary macro
(defmacro load-var-theme (theme)
  "Load THEME with given string name."
  `(load-theme ',(car (read-from-string theme)) t))

;;; idea: http://xahlee.info/emacs/emacs/elisp_toggle_command.html
;;;###autoload
(defun $cycle-theme (arg)
  "Cycle colorschemes through preset list.  ARG is prefix argument, default 1."
  (interactive "p")
  (let*
      ((colorschemes ["doom-material-dark" "zerodark" "doom-nord" "doom-1337" "doom-xcode" "doom-gruvbox" "modus-vivendi" "ef-duo-dark" "ef-trio-dark"])
       (idx (if (get '$cycle-colorscheme 'state)
                (get '$cycle-colorscheme 'state)
              0))
       (next-idx (% (+ idx arg (length colorschemes)) (length colorschemes)))
       (current-colorscheme (aref colorschemes idx))
       (new-colorscheme (aref colorschemes next-idx)))
    (put '$cycle-colorscheme 'state next-idx)
    ;;(disable-theme (car (read-from-string current-colorscheme))) ; we advised load-theme to disable other themes first
    (load-theme (car (read-from-string new-colorscheme)) t)
    (message "Loaded colorscheme: %s" new-colorscheme)))

;;;###Autoload
(defun $cycle-font (arg)
  "Cycle through fonts.  ARG is prefix arg, default 1."
  (interactive "p")
  (let* ((fonts ["VictorMono Nerd Font Mono" "SauceCodePro Nerd Font Mono" "Hack Nerd Font Mono" "JetBrainsMono Nerd Font Mono"])
         (idx (if (get '$cycle-font 'state)
                  (get '$cycle-font 'state)
                0))
         (next-idx (% (+ idx arg (length fonts)) (length fonts)))
         (next-font (aref fonts next-idx)))
    (put '$cycle-font 'state next-idx)
    ($set-current-font next-font)
    (message "Font: %s" next-font)))

(provide 'toggle-commands)
;;; toggle-commands.el ends here
