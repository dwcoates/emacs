(defun my-org-mode-hook ()
  (org-mode)
  'turn-on-auto-fill)

(setq org-src-fontify-natively t)

;(add-hook 'org-mode-hook 'my-org-mode-hook)

(defun my-latex-mode-hook ()
  (latex-mode))

(global-set-key (kbd "C-c o k") 'org-mode-delete-column)
(global-set-key (kbd "C-c o i") 'org-mode-insert-column)
(global-set-key (kbd "C-c o j") 'org-mode-move-column-left)
(global-set-key (kbd "C-c o l") 'org-mode-move-column-right)

(global-set-key (kbd "C-c o RET") 'org-mode-todo-heading)


;; move point to top-level heading
(defun org-back-to-top-level-heading ()
  "Go back to the current top level heading."
  (interactive)
  (or (re-search-backward "^\* " nil t)
      (goto-char (point-min))))

;; make todo's check recursively when determining the number of todo's under it
(setq org-hierarchical-todo-statistics nil)


;; This is for makig sure that the top-level todo automatically is marked DONE if all sub-levels
;; are DONE. TODO otherwise.
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)


(provide 'setup-org-mode)
