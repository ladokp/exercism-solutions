;;; bob.el --- Bob exercise (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

(defun response-for (input)
  (setq input (replace-regexp-in-string "\n" "" input))
  (let ((len (length input)) yell question)
    (when (and (string-match-p "[[:upper:]]" input)
               (equal input (upcase input)))
      (setq yell t))
    (when (string-match-p "?[\s\t]*$" input)
      (setq question t))
    (cond
     ((and yell question)
      "Calm down, I know what I'm doing!")
     (yell "Whoa, chill out!")
     (question "Sure.")
     ((or (string-match-p "[[:alpha:]]" input)
          (string-match-p "[[:digit:]]" input))
      "Whatever.")
     (t "Fine. Be that way!")
     )))

(provide 'bob)
;;; bob.el ends here
