;; personal/basic/autoload/default.el -*- lexical-binding: t; -*-

;;;###autoload
(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.

     Move point to the first non-whitespace character on this line.
     If point was already at that position, move point to beginning of line."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (if (equal major-mode 'org-mode)
             (org-beginning-of-line)
           (beginning-of-line)))))

