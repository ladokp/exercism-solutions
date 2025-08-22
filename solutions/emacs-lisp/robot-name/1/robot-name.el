;;; robot-name.el --- Robot Name (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Build a robot with a name like AA000, that can be reset
;; to a new name. Instructions are in README.md
;;
(require 'cl)

(defun build-robot ()
  (cl-flet* ((rand-range (fr to) (+ (random (+ to 1 (- fr))) fr)))
	(apply 'string (list (rand-range ?A ?Z)
						 (rand-range ?A ?Z)
						 (rand-range ?0 ?9)
						 (rand-range ?0 ?9)
						 (rand-range ?0 ?9)))))

(defun robot-name (robot) robot)

(defmacro reset-robot (robot) `(setq robot ,(build-robot)))

(provide 'robot-name)
;;; robot-name.el ends here
