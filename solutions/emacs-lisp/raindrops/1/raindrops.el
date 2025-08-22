;;; raindrops.el --- Raindrops (exercism)

;;; Commentary:

;;; Code:

(defun convert (number)
  (let* ((result "")
		 (result (if (zerop (% number 3)) (concat result "Pling") result))
		 (result (if (zerop (% number 5)) (concat result "Plang") result))
		 (result (if (zerop (% number 7)) (concat result "Plong") result)))
	(if (string= result "") (format "%d" number)
        result)))

(provide 'raindrops)
;;; raindrops.el ends here
