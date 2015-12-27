(defun on-emacs-startup ()
  (interactive)
  (find-file "~/workspace/organization/everything.org")
  (split-window-horizontally)
  (next-multiframe-window)
  (ztree-dir "~/workspace")
  (next-multiframe-window)
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
