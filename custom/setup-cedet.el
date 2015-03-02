(require 'cc-mode)
(require 'semantic)


(global-semanticdb-minor-mode 1)

(global-semantic-idle-scheduler-mode 1)
(global-semantic-stickyfunc-mode 1)


(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(set-default 'semantic-case-fold t)

(semantic-mode 1)

(defun dodge-cedet-hook ()
  (local-set-key "\C-c\C-j" 'semantic-ia-fast-jump)
  (local-set-key "\C-c\C-j" 'semantic-ia-fast-jump)
  (local-set-key "\C-c\C-s" 'semantic-ia-show-summary))

(add-hook 'c-mode-common-hook 'dodge-cedet-hook)
(add-hook 'c-mode-hook 'dodge-cedet-hook)
(add-hook 'c++-mode-hook 'dodge-cedet-hook)


;; add include paths for semantic
;; remove "c++-mode" to make path available to both c and c++ modes
;(semantic-add-system-include '/path/to/library' 'c++-mode)


(require 'function-args)
(fa-config-default)
(define-key c-mode-map  [("C-c C-f C-h")] 'moo-complete)
(define-key c++-mode-map  [(control tab)] 'moo-complete)
(define-key c-mode-map (kbd "M-o")  'fa-show)
(define-key c++-mode-map (kbd "M-o")  'fa-show)


;; please don't ask me if I really want to compile
(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))



;; Enable EDE only in C/C++
(require 'ede)
(global-ede-mode)

(provide 'setup-cedet)
