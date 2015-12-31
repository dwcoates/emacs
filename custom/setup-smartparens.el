(require 'smartparens-config)
(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(setq sp-backward-delete-char 'paredit-backward-delete)
(sp-use-paredit-bindings)

(show-smartparens-global-mode +1)
(smartparens-global-mode 1)

(add-hook 'prog-mode-hook 'turn-on-smartparens-mode)
(add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)

;; Key bindings
(bind-keys
 :map smartparens-mode-map
 ("C-x p a" . sp-beginning-of-sexp)
 ("C-x p e" . sp-end-of-sexp)

 ("C-x p k" . sp-down-sexp)
 ("C-x p i"   . sp-up-sexp)
 ("C-x p j" . sp-backward-down-sexp)
 ("C-x p l"   . sp-backward-up-sexp)

 ("C-x p f" . sp-forward-sexp)
 ("C-x p b" . sp-backward-sexp)

 ("C-x p n" . sp-next-sexp)
 ("C-x p p" . sp-previous-sexp)

 ("C-x p h" . sp-forward-symbol)
 ("C-x p g" . sp-backward-symbol)

 ("C-x p e" . sp-forward-slurp-sexp)
 ("C-x p w" . sp-forward-barf-sexp)
 ("C-x p r"  . sp-backward-slurp-sexp)
 ("C-x p t"  . sp-backward-barf-sexp)

 ("C-x p C-t" . sp-transpose-sexp)
 ("C-x p k" . sp-kill-sexp)
 ("C-x p h"   . sp-kill-hybrid-sexp)
 ("C-x p C-k"   . sp-backward-kill-sexp)
 ("C-x p C-w" . sp-copy-sexp)

; ("C-x p d" . sp-delete-sexp)        ;; this function doesn

 ("<backspace>" . sp-backward-delete-char)
 ("M-<backspace>" . backward-kill-word)     ;; this should be like paredit
 ("C-<backspace>" . sp-backward-kill-word)     ;; this should be like paredit
 ([remap sp-backward-kill-word] . backward-kill-word)

 ("M-[" . sp-backward-unwrap-sexp)
 ("M-]" . sp-unwrap-sexp)

 ("C-x C-t" . sp-transpose-hybrid-sexp)

 ("C-c ("  . wrap-with-parens)
 ("C-c ["  . wrap-with-brackets)
 ("C-c {"  . wrap-with-braces)
 ("C-c '"  . wrap-with-single-quotes)
 ("C-c \"" . wrap-with-double-quotes)
 ("C-c _"  . wrap-with-underscores)
 ("C-c `"  . wrap-with-back-quotes))


(provide 'setup-smartparens)
