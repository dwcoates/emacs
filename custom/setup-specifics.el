;; start up buffers.
(defun on-emacs-startup ()
  (interactive)
  ;; FRAME 1
  (find-file "~/workspace/organization/everything.org")
  (org-agenda)
  (delete-window)     ; this is here because for whatever reason, org-agenda insists on opening horizontal=
  (split-window-horizontally)
  (windmove-right)
  (switch-to-buffer "*Org Agenda*")
  (windmove-left)

  ;; FRAME 2
  )
(on-emacs-startup)


(defun smart-open-in-horizontal (filename)
  (interactive)
  (if (=  (length 'window-list) 1)
      (if (not (= buffer-file-name "/home/dodge/workspace/organization/everything.org"))
          '(split-window-horizontally
           find-file "~/workspace/organization/everything.org")             )
    (if (= (get-buffer-window "everything.org") nil)
        (switch-to-buffer "everything.org")
      (next-multiframe-window)
        )
    )
  (previous-buffer)
  )


;(global-set-key (kbd "C-c o e") (smart-open-in-horizontal "~/workspace/organization/everything.org"))
;(global-set-key (kbd "C-c e i") (smart-open-in-horizontal "~/.emacs.d/init.el"))



(provide 'setup-specifics)
