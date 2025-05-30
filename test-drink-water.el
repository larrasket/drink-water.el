;;; test-drink-water.el --- Tests for drink-water.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'drink-water)

(ert-deftest drink-water--daily-need-ml-test ()
  (let ((drink-water-weight-kg 60))
    (should (= (drink-water--daily-need-ml) 2100)))
  (let ((drink-water-weight-kg 80))
    (should (= (drink-water--daily-need-ml) 2800))))

(ert-deftest drink-water--cups-per-day-test ()
  (let ((drink-water-weight-kg 70)
        (drink-water-cup-size-ml 200))
    (should (= (drink-water--cups-per-day) 13)))
  (let ((drink-water-weight-kg 50)
        (drink-water-cup-size-ml 500))
    (should (= (drink-water--cups-per-day) 4))))

(ert-deftest drink-water--interval-minutes-test ()
  (let ((drink-water-wake-hour 8)
        (drink-water-sleep-hour 20)
        (drink-water-weight-kg 60)
        (drink-water-cup-size-ml 300))
    (should (>= (drink-water--interval-minutes) 30))
    (should (= (drink-water--interval-minutes)
               (max 30 (floor (/ (* 12 60) (drink-water--cups-per-day))))))))

(ert-deftest drink-water--random-quote-test ()
  (let ((drink-water-quote-file (expand-file-name "drink-water-quotes.el" default-directory)))
    (should (stringp (drink-water--random-quote)))))

(provide 'test-drink-water)
;;; test-drink-water.el ends here 