;;; leap.el --- Leap exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

(defun leap-year-p (year)
;;; Code:
  (and (= 0 (% year 4))
       (or (= 0 (% year 400))
        (not (= 0 (% year 100))))))

(provide 'leap-year-p)
;;; leap.el ends here
