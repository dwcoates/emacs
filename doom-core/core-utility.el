;;; ../doom-core/core-utility.el -*- lexical-binding: t; -*-

(add-hook 'occur-mode-find-occurrence-hook 'recenter)

(map! :map occur-mode-map
      "<down>" #'occur-step-forward
      "<up>"   #'occur-step-backward
      :map global-map
      "M-o"     #'other-window
      "C-x C-b" nil
      "C-x o"   #'other-window
      "C-x O"   #'other-frame
      "C-c b"   #'switch-to-other-buffer
      "C-x M-t" #'transpose-windows
      "C-x 5 DEL" #'delete-frame
      "C-x DEL"   #'delete-window
      (:after org-mode
        :map org-mode-map
        "C-c o i" nil))

;; (global-unset-key (kbd "C-x 0"))
;; (global-unset-key (kbd "C-x 5 0"))
