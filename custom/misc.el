(defun dwc-remove-headers ()
  (goto-char (point-min))
  (re-search-forward "^$")
  (goto-char (+ 1 (point)))
  (delete-region (point) (point-min)))


;; this works, but only because of  (message "") hack
(defun dwc-find-file-other-frame (file)
  (interactive "find file: ")
  (find-file-other-frame file)
  (toggle-scroll-bar)
  (message ""))

;; do I like calculator?
(global-set-key (kbd "C-=") 'calculator)


(provide 'misc)
