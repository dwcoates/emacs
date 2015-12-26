(defun on-emacs-startup ()
  (interactive)
  (find-file "~/workspace/organization/everything.org")
  (split-window-horizontally)
  (next-multiframe-window)
  (ztree-dir "~/workspace")
  (next-multiframe-window)
  )
(on-emacs-startup)


(defun open-everything-org-in-horizontal ()
  (interactive)
  (if (not (>  (length (window-list)) 1))
      (split-window-horizontally)
    (next-multiframe-window)
    (find-file "~/workspace/organization/everything.org")
    )
  (previous-buffer)
  )

(global-set-key (kbd "C-c o e") 'open-everything-org-in-horizontal)


                                        ;(global-set-key (kbd "C-c f e i") (find-file "~/.emacs.d/init.el"))

(provide 'setup-specifics)
