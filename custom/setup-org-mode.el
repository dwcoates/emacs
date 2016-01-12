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

(setq org-goto-interface 'outline-path-completion
      org-goto-max-level 10)

;; org-capture allows you to take a note anywhere, which it will write to the org-default-notes-file.
;; this is set in ~/.emacs.d/custom/setup-specifics.el
(global-set-key (kbd "C-c c") 'org-capture)


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

;; This is a snippet from John Wiegley. It shows org agenda after emacs has been idle for a certain
;; amount of time.
(defun jump-to-org-agenda ()
  (interactive)
  (let ((buf (get-buffer "*Org Agenda*"))
        wind)
    (if buf
        (if (setq wind (get-buffer-window buf))
            (select-window wind)
          (if (called-interactively-p)
              (progn
                (select-window (display-buffer buf t t))
                (org-fit-window-to-buffer)
                ;; (org-agenda-redo)
                )
            (with-selected-window (display-buffer buf)
              (org-fit-window-to-buffer)
              ;; (org-agenda-redo)
              )))
      (call-interactively 'org-agenda-list)))
  ;;(let ((buf (get-buffer "*Calendar*")))
  ;;  (unless (get-buffer-window buf)
  ;;    (org-agenda-goto-calendar)))
  )
(run-with-idle-timer 300 t 'jump-to-org-agenda)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; KEY BINDINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'setup-org-mode)
