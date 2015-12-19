(defun my-org-mode-hook ()
  (org-mode))

(setq org-src-fontify-natively t)

;(add-hook 'org-mode-hook 'my-org-mode-hook)

(defun my-latex-mode-hook ()
  (latex-mode))

(global-set-key [M-S-<left>] 'org-mode-delete-column)
(global-set-key [M-S-<right>] 'org-mode-insert-column)
(global-set-key [M-<left>] 'org-mode-move-column-left)
(global-set-key [M-<right>] 'org-mode-move-column-right)

(global-set-key [M-S-<RET>] 'org-mode-todo-heading)


;; move point to top-level heading
(defun org-back-to-top-level-heading ()
  "Go back to the current top level heading."
  (interactive)
  (or (re-search-backward "^\* " nil t)
      (goto-char (point-min))))

;; make todo's check recursively when determining the number of todo's under it
(setq org-hierarchical-todo-statistics nil)

(provide 'setup-org-mode)
