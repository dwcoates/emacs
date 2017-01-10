(bind-keys
 :map smartparens-mode-map
 ("C-' a" . sp-beginning-of-sexp)
 ("C-' e" . sp-end-of-sexp)

 ("C-' k" . sp-down-sexp)
 ("C-' i"   . sp-up-sexp)
 ("C-' j" . sp-backward-down-sexp)
 ("C-' l"   . sp-backward-up-sexp)

 ("C-' f" . sp-forward-sexp)
 ("C-' b" . sp-backward-sexp)

 ("C-' n" . sp-next-sexp)
 ("C-' p" . sp-previous-sexp)

 ("C-' h" . sp-forward-symbol)
 ("C-' g" . sp-backward-symbol)

 ("C-' t" . sp-forward-slurp-sexp)
 ("C-' w" . sp-forward-barf-sexp)
 ("C-' r"  . sp-backward-slurp-sexp)
 ("C-' q"  . sp-backward-barf-sexp)

 ("C-' C-t" . sp-transpose-sexp)
 ("C-' k" . sp-kill-sexp)
 ("C-' h"   . sp-kill-hybrid-sexp)
 ("C-' C-k"   . sp-backward-kill-sexp)
 ("C-' C-w" . sp-copy-sexp)

 ("C-' d" . sp-delete-sexp)        ;; this function doesnt exist?

 ("<backspace>" . sp-backward-delete-char)
 ("C-<backspace>" . backward-delete-char)     ;; this should be like paredit
 ("M-<backspace>" . sp-backward-kill-word)     ;; this should be like paredit
 ("M-s-<backspace>" . backward-kill-word)     ;; this should be like paredit
 ([remap sp-backward-kill-word] . backward-kill-word)

 ("M-[" . sp-backward-unwrap-sexp)
 ("M-]" . sp-unwrap-sexp)
 ("M-s-[" . sp-rewrap-sexp)

 ("C-x C-t" . sp-transpose-hybrid-sexp)

 ("C-c ("  . wrap-with-parens)
 ("C-c ["  . wrap-with-brackets)
 ("C-c {"  . wrap-with-braces)
 ("C-c '"  . wrap-with-single-quotes)
 ("C-c \"" . wrap-with-double-quotes) ;; This currently causes minor problem with smartparens parser
 ("C-c _"  . wrap-with-underscores)
 ("C-c `"  . wrap-with-back-quotes))
