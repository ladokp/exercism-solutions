;;; anagram.el --- Anagram (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

(require 'cl-lib)

(defun anagrams-for (w ws)
  (seq-filter
   (lambda (ww)
     (and
      (string= (seq-sort #'< (downcase w)) (seq-sort #'< (downcase ww)))
      (not (string= (downcase w) (downcase ww)))))
   ws))

(provide 'anagram)
;;; anagram.el ends here
