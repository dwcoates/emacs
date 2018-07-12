;; personal/basic/autoload/default.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +smart-beginning-of-line ()
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


;;;###autoload
(defun +add-file-name-to-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename
         (if (equal major-mode 'dired-mode)
             (file-truename (dired-file-name-at-point))
           (buffer-file-name))))
    (message "Copied \'%s\' to clipboard" (kill-new filename))))

;;;###autoload
(defun +toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
         (next-win-buffer (window-buffer (next-window)))
         (this-win-edges (window-edges (selected-window)))
         (next-win-edges (window-edges (next-window)))
         (this-win-2nd (not (and (<= (car this-win-edges)
                     (car next-win-edges))
                     (<= (cadr this-win-edges)
                     (cadr next-win-edges)))))
         (splitter
          (if (= (car this-win-edges)
             (car (window-edges (next-window))))
          'split-window-horizontally
        'split-window-vertically)))
    (delete-other-windows)
    (let ((first-win (selected-window)))
      (funcall splitter)
      (if this-win-2nd (other-window 1))
      (set-window-buffer (selected-window) this-win-buffer)
      (set-window-buffer (next-window) next-win-buffer)
      (select-window first-win)
      (if this-win-2nd (other-window 1))))))
