;;; difference-of-squares.el --- Difference of Squares (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(defun square-of-sum (n)
  (let ((sum (/ (* n (+ n 1)) 2)))
    (* sum sum)))

(defun sum-of-squares (n)
  (let ((sum (/ (* (* n (+ n 1)) (+ (* n 2) 1)) 6)))
    sum))

(defun difference (n)
  (- (square-of-sum n) (sum-of-squares n)))

(provide 'difference-of-squares)
;;; difference-of-squares.el ends here
