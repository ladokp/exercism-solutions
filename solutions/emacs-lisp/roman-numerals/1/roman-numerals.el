;;; roman-numerals.el --- roman-numerals Exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

(defconst roman-map
  '((1000 "M") (900 "CM") (500 "D") (400 "CD") (100 "C") (90 "XC")
    (50 "L") (40 "XL") (10 "X") (9 "IX") (5 "V") (4 "IV") (1 "I")))

(defun to-roman (n)
  (if (zerop n) ""
    (let ((pair (seq-find (lambda (pair) (>= n (car pair))) roman-map)))
      (concat (cadr pair) (to-roman (- n (car pair)))))))

(provide 'roman-numerals)
;;; roman-numerals.el ends here
