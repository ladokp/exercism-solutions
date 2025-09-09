(defmodule anagram
  (export (find 2)))

(defun find (word candidates)
  (let ((normed-word (normalize word)))
    (lists:filter
      (lambda (candidate) (anagram? normed-word (normalize candidate)))
      candidates)))

(defun anagram? (normed-word normed-candidate)
  (let ((`#(,word-lowered ,word-sorted) normed-word)
        (`#(,candidate-lowered ,candidate-sorted) normed-candidate))
    (andalso (=/= word-lowered candidate-lowered)
             (=:= word-sorted candidate-sorted))))

(defun normalize (str)
  (let ((lowered (string:to_lower str)))
    `#(,lowered ,(lists:sort lowered))))
