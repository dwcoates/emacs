;;; Code --- summary:
;;
;; Author: Dodge W. Coates
;; A miscellaneous assortment of functions for org agenda
;;
;;; Commentary:
;;
;;; Code:

(require 'org)

(defun org-switch-to-agenda (&optional redo)
  "Swtich to `org-agenda' buffer if it exists, else create one.
If an agenda needs to be created, it will be a weekly view.
If REDO is non-nil, rebuild the agenda view after visiting it."
  (interactive)
  (let ((buf (get-buffer "*Org Agenda*")))
    (if buf
        (progn
          (switch-to-buffer-other-window buf)
          (when redo (org-agenda-redo)))
      (org-agenda nil "a"))))

(defun org-get-agenda ()
  "Same as `org-switch-to-agenda' with non-nil REDO."
  (interactive)
  (org-switch-to-agenda t))

(defun org-agenda-kill-entries (beg end)
  "Kill multiple lines in the agenda from BEG to END.
If END is not at the end of line, it wont delete that line."
  (interactive (if (use-region-p)
                   (list (line-number-at-pos (region-beginning))
                         (line-number-at-pos (if (equal (region-end)
                                                        (save-excursion (org-end-of-line) (point)))
                                                 (region-end)
                                               (max (save-excursion
                                                      (previous-line) (org-end-of-line) (point))
                                                    (region-beginning)))))
                 (list (line-number-at-pos) (line-number-at-pos))))
  (transient-mark-mode nil)
  (goto-line beg)
  (loop for i from beg to end
        collect (ignore-errors (org-agenda-kill)))
  (transient-mark-mode 1))

(provide 'ag-misc)

;;; ag-misc.el ends here
