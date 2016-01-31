;; start up buffers.
(defun on-emacs-startup ()
  (interactive)
  ;; FRAME 1
  (find-file "~/workspace/organization/everything.org")
  (org-agenda)
  (delete-window)  ; this is here because for whatever reason, org-agenda insists on opening horizontal
  (split-window-horizontally)
  (windmove-right)
  (switch-to-buffer "*Org Agenda*")
  (windmove-left)
  (enlarge-window-horizontally 10)
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
  (previous-buffer))


(global-set-key (kbd "C-c o e")
                (lambda () (interactive) (find-file "~/workspace/organization/everything.org")))
(global-set-key (kbd "C-c o C-e")
                (lambda ()
                  (interactive)
                  (find-file-other-window "~/workspace/organization/everything.org")))

(global-set-key (kbd "C-c o i")
                (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "C-c o C-i")
                (lambda ()
                  (interactive)
                  (find-file-other-window "~/.emacs.d/init.el")))

(global-set-key (kbd "C-c o c")
                (lambda () (interactive) (find-file "~/.emacs.d/custom/")))
(global-set-key (kbd "C-c o C-c")
                (lambda ()
                  (interactive)
                  (find-file-other-window "~/.emacs.d/custom/")))

(global-set-key (kbd "C-c o s")
                (lambda () (interactive) (find-file "~/.emacs.d/custom/setup-specifics.el")))
(global-set-key (kbd "C-c o C-s")
                (lambda ()
                  (interactive)
                  (find-file-other-window "~/.emacs.d/custom/setup-specifics.el")))

(global-set-key (kbd "C-c o p")
                (lambda () (interactive) (find-file "~/.emacs.d/custom/setup-smartparens.el")))
(global-set-key (kbd "C-c o C-p")
                (lambda ()
                  (interactive)
                  (find-file-other-window "~/.emacs.d/custom/setup-smartparens.el")))

(global-set-key (kbd "C-c o a")
                (lambda ()
                  (interactive)
                  (find-file "~/.emacs.d/custom/setup-appearance-and-navigation.el")))
(global-set-key (kbd "C-c o C-a")
                (lambda ()
                  (interactive)
                  (find-file-other-window "~/.emacs.d/custom/setup-appearance-and-navigation.el")))


;; this doesnt work until dwc-find-file-other-frame is fixed (custom/misc.el)
(global-set-key (kbd "C-c o f e")
                (lambda ()
                  (interactive)
                  (dwc-find-file-other-frame "~/.emacs.d/init.el")
                  (split-window-horizontally)
                  (windmove-right)
                  (find-file "~/.emacs.d/custom/")))
(global-set-key (kbd "C-c o f C-e")
                (lambda ()
                  (interactive)
                  (find-file-other-window "~/.emacs.d/custom/")))




;; default notes file.
(setq org-default-notes-file "~/workspace/organization/everything.org")



;(global-set-key (kbd "C-c o e") (smart-open-in-horizontal "~/workspace/organization/everything.org"))
;(global-set-key (kbd "C-c e i") (smart-open-in-horizontal "~/.emacs.d/init.el"))



(provide 'setup-specifics)
