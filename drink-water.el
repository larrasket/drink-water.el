;;; drink-water.el --- Hydration reminder -*- lexical-binding: t; -*-

;; Author: Salih Muhammed <lr0@gmx.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: health, convenience
;; URL: https://github.com/larrasket/drink-water.el

;;; Commentary:
;; This package reminds you to drink water at smart intervals based on your
;; needs. It calculates your daily water requirement, tracks your intake, and
;; motivates you with hydration quotes.

;;; Code:

(require 'cl-lib)
(require 'alert)

;; Quotes are now loaded from the `drink-water-quotes' variable in
;; `drink-water-quotes.el'.

;; Require the quotes file
(require 'drink-water-quotes)

(defgroup drink-water nil
  "Hydration reminder and tracker."
  :group 'convenience)

(defcustom drink-water-weight-kg 70
  "User's weight in kilograms. Used to estimate daily water needs."
  :type 'number)

(defcustom drink-water-cup-size-ml 250
  "Default cup size in milliliters."
  :type 'number)

(defcustom drink-water-wake-hour 8
  "Hour of the day when you start your day (24h format)."
  :type 'integer)

(defcustom drink-water-sleep-hour 22
  "Hour of the day when you end your day (24h format)."
  :type 'integer)

(defcustom drink-water-notification-title "💧 Drink Water!"
  "Title for the hydration notification."
  :type 'string)

(defcustom drink-water-minimum-interval-minutes 60
  "Minimum number of minutes between hydration notifications.
No notification will be shown more frequently than this interval."
  :type 'integer)

(defvar drink-water--timer nil)
(defvar drink-water--cups-today 0)
(defvar drink-water--last-reset (current-time))
(defvar drink-water--last-notification-time nil)

(defun drink-water--daily-need-ml ()
  "Calculate daily water need in milliliters."
  (round (* drink-water-weight-kg 35)))

(defun drink-water--cups-per-day ()
  "Calculate how many cups per day are needed."
  (max 1 (ceiling (/ (drink-water--daily-need-ml) (float drink-water-cup-size-ml)))))

(defun drink-water--interval-minutes ()
  "Calculate interval in minutes between reminders, respecting the minimum interval."
  (let* ((hours (- drink-water-sleep-hour drink-water-wake-hour))
         (total-minutes (* hours 60))
         (cups (drink-water--cups-per-day))
         (suggested (floor (/ total-minutes cups))))
    (max drink-water-minimum-interval-minutes suggested)))

(defun drink-water--random-quote ()
  "Return a random hydration quote from the `drink-water-quotes' variable."
  (when (and (boundp 'drink-water-quotes) drink-water-quotes)
    (nth (random (length drink-water-quotes)) drink-water-quotes)))

(defun drink-water--notify ()
  "Show a hydration notification using the alert package."
  (let ((quote (drink-water--random-quote)))
    (alert (or quote "Time to hydrate!")
           :title drink-water-notification-title
           :category 'drink-water)))

(defun drink-water--reset-if-needed ()
  "Reset daily counter if a new day has started."
  (let ((now (current-time)))
    (unless (equal (format-time-string "%Y-%m-%d" now)
                   (format-time-string "%Y-%m-%d" drink-water--last-reset))
      (setq drink-water--cups-today 0)
      (setq drink-water--last-reset now))))

(defun drink-water--reminder ()
  "Remind the user to drink water, and update state."
  (drink-water--reset-if-needed)
  (when (< drink-water--cups-today (drink-water--cups-per-day))
    (let ((now (float-time (current-time))))
      (when (or (not drink-water--last-notification-time)
                (>= (- now drink-water--last-notification-time)
                    (* 60 drink-water-minimum-interval-minutes)))
        (drink-water--notify)
        (setq drink-water--last-notification-time now)))))

;;;###autoload
(define-minor-mode drink-water-mode
  "Global minor mode to remind you to drink water at smart intervals.

When enabled, a timer will periodically remind you to drink water, but never more
frequently than `drink-water-minimum-interval-minutes`. The interval is calculated
based on your daily water needs and day length, but is always at least the minimum."
  :global t
  :group 'drink-water
  (if drink-water-mode
      (progn
        (drink-water--maybe-cancel-timer)
        (setq drink-water--last-notification-time nil)
        (setq drink-water--timer
              (run-at-time 0 (drink-water--interval-minutes) #'drink-water--reminder)))
    (drink-water--maybe-cancel-timer)))

(defun drink-water--maybe-cancel-timer ()
  "Cancel the drink-water timer if it exists."
  (when (timerp drink-water--timer)
    (cancel-timer drink-water--timer)
    (setq drink-water--timer nil)))

;;;###autoload
(defun drink-water-drank ()
  "Record that you drank a cup of water."
  (interactive)
  (drink-water--reset-if-needed)
  (cl-incf drink-water--cups-today)
  (message "Great! You've had %d/%d cups today."
           drink-water--cups-today (drink-water--cups-per-day)))

(provide 'drink-water)
;;; drink-water.el ends here
