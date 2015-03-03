(require 'flycheck)

(add-hook 'after-init-hook #'global-flycheck-mode)

(global-set-key (kbd "C-c ! n") 'flycheck-next-error)
(global-set-key (kbd "C-c ! p") 'flycheck-previous-error)
(global-set-key (kbd "C-c ! h") 'helm-flycheck)

(provide 'setup-flycheck)
