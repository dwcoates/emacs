;(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'rainbow-delimiters-mode-enable)
(add-hook 'eval-expression-minibuffer-setup-hook #'rainbow-delimiters-mode-enable)
(add-hook 'ielm-mode-hook             #'rainbow-delimiters-mode-enable)
(add-hook 'lisp-mode-hook             #'rainbow-delimiters-mode-enable)
(add-hook 'lisp-interaction-mode-hook #'rainbow-delimiters-mode-enable)
(add-hook 'clojure-mode-hook          #'rainbow-delimiters-mode-enable)
(add-hook 'scheme-mode-hook           #'rainbow-delimiters-mode-enable)



;; eldoc-mode shows documentation in the minibuffer when writing code
;; http://www.emacswiki.org/emacs/ElDoc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)


(provide 'setup-lisp-editing)
