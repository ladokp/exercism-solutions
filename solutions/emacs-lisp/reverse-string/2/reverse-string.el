;;; reverse-string.el --- Reverse String (exercism)  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ucs-normalize)

(defun combining-mark-p (char)
  (let ((category (get-char-code-property char 'general-category)))
    (or (string= category "Mn")    ; Nonspacing_Mark
        (string= category "Mc")    ; Spacing_Mark
        (string= category "Me")))) ; Enclosing_Mark

(defun split-grapheme-clusters (string)
  (let ((grapheme-clusters '())
        (index 0)
        (string-length (length string)))
    (while (< index string-length)
      (let ((cluster-start index)
            (current-char (aref string index)))
        (setq index (1+ index))
        (while (and (< index string-length)
                    (combining-mark-p (aref string index)))
          (setq index (1+ index)))
        (push (substring string cluster-start index) grapheme-clusters)))
    (nreverse grapheme-clusters)))

(defun reverse-string (input-string)
  (let* ((decomposed-string (ucs-normalize-NFD-string input-string))
         (grapheme-clusters (split-grapheme-clusters decomposed-string)))
    (apply #'concat (reverse grapheme-clusters))))

(provide 'reverse-string)
;;; reverse-string.el ends here
