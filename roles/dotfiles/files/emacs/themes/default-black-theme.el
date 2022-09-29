;;; defaut-black-theme.el --- default black theme
;;; Commentary:
;; source: https://github.com/magnars/.emacs.d with some slight modifications

;;; Code:

(deftheme default-black)

(custom-theme-set-faces
 'default-black
 '(default ((t (:inherit nil :stipple nil :background "Black" :foreground "White" :inverse-video nil :box nil :strike-t*hrough nil :overline nil :underline nil :slant normal :weight normal :width normal :height 105))))
 '(italic ((t (:slant italic))))
 '(highlight ((((class color) (min-colors 88) (background dark)) (:background "#464740"))))
 '(region ((nil (:background "#464740"))))
 '(hl-line ((nil (:background "#222222"))))
 '(yas-field-highlight-face ((nil (:background "#333399"))))
 '(font-lock-warning-face ((nil (:foreground "#ff6666"))))
 '(show-paren-mismatch ((((class color)) (:background "red"))))
 '(show-paren-match ((nil (:background "#333399"))))

 ;;; tab bar
 '(tab-bar ((nil (:background "#000000"))))
 '(tab-bar-tab ((nil (:background "#464740"))))
 '(tab-bar-tab-inactive ((nil (:background "#000000"))))

 ;; term
 '(term-color-red ((nil (:foreground "#ff8059"))))
 '(term-color-green ((nil (:foreground "#44bc44"))))
 '(term-color-yellow ((nil (:foreground "#eecc00"))))
 '(term-color-blue ((nil (:foreground "#2fafff"))))
 '(term-color-magenta ((nil (:foreground "#feacd0"))))
 '(term-color-cyan ((nil (:foreground "#00d3d0"))))
 '(term-color-white ((nil (:foreground "#bfbfbf"))))

  ;; cperl
 '(cperl-array-face ((t (:inherit font-lock-keyword-face))))
 '(cperl-hash-face ((t (:inherit font-lock-variable-name-face)))))

(provide-theme 'default-black)
;;; default-black-theme ends here
