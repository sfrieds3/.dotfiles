;;; my-config --- other config stuff -*- lexical-binding: t -*-

;;; Commentary:
;;;     my various settings

;;; Code:

;;; display-time-world command
(setq display-time-world-list
  '(("America/Los_Angeles" "Los Angeles")
    ("America/New_York" "New York")
    ("Europe/Amsterdam" "Amsterdam")
    ("Europe/Berlin" "Berlin")
    ("Asia/Tokyo" "Tokyo")
    ("Europe/London" "London")
    ("Pacific/Auckland" "Auckland")
    ("Pacific/Sydney" "Sydney")
    ("Asia/Shanghai" "Shanghai")))
(setq display-time-world-time-format "%a, %d %b %I:%M %p %Z")

(provide 'my-config)
;;; my-config.el ends here
