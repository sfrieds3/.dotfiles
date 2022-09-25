;;;; scwfri-config --- summary

;;; Commentary:
;;;     scwfri-settings

;;; Code:

;;; display-time-world command
(setq display-time-world-list
  '(("America/Los_Angeles" "Los Angeles")
    ("America/New_York" "New York")
    ("Europe/Athens" "Athens")
    ("Europe/Berlin" "Berlin")
    ("Asia/Tokyo" "Tokyo")
    ("Europe/London" "London")
    ("Pacific/Auckland" "Auckland")
    ("Asia/Shanghai" "Shanghai")))
(setq display-time-world-time-format "%a, %d %b %I:%M %p %Z")

(provide 'scwfri-config)
;;; scwfri-settings.el ends here
