(defun my-org-mode-hook ()
  (org-mode))

(setq org-src-fontify-natively t)

;(add-hook 'org-mode-hook 'my-org-mode-hook)

(defun my-latex-mode-hook ()
  (latex-mode))

(provide 'setup-org-mode)
