(bind-keys*
 :map smartparens-mode-map
 ;; delete behavior
 ("<backspace>" . sp-backward-delete-char)
 ("<C-backspace>" . backward-delete-char)
 ("<M-backspace>" . sp-backward-kill-word)
 ("<C-M-backspace>" . backward-kill-word)
 ;; wrap/unwrap/rewrap
 ("M-[" . sp-backward-unwrap-sexp)
 ("M-]" . sp-unwrap-sexp)
 ("M-s-[" . sp-rewrap-sexp)
 :prefix "C-j"
 :prefix-map smartparens-mode-map
 ;; wrapping
 ("("  . (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "(")))
 ("["  . (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "[")))
 ("{"  . (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "{")))
 ("'"  . (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "'")))
 ("\"" . (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "\"")))
 ("_"  . (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "_")))
 ("`"  . (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "`")))
 ;; sexp direction
 ("a" . sp-beginning-of-sexp)
 ("e" . sp-end-of-sexp)
 ("i"   . sp-up-sexp)
 ("j" . sp-backward-down-sexp)
 ("l"   . sp-backward-up-sexp)
 ("f" . sp-forward-sexp)
 ("b" . sp-backward-sexp)
 ("n" . sp-next-sexp)
 ("p" . sp-previous-sexp)
 ;; symbol direction
 ("h" . sp-forward-symbol)
 ("g" . sp-backward-symbol)
 ;; slurping
 ("t" . sp-forward-slurp-sexp)
 ("w" . sp-forward-barf-sexp)
 ("r"  . sp-backward-slurp-sexp)
 ("q"  . sp-backward-barf-sexp)
 ;; transposing
 ("C-t" . sp-transpose-sexp)
 ("M-t" . sp-transpose-hybrid-sexp)
 ;; killing/copying
 ("k" . sp-kill-sexp)
 ("h"   . sp-kill-hybrid-sexp)
 ("C-k"   . sp-backward-kill-sexp)
 ("C-w" . sp-copy-sexp)
 ;; deleting
 ("d" . sp-delete-)
 ("C-d" . sp-delete-sexp)
 ("<C-backspace>" . sp-backward-delete-symbol)
 ("<M-backspace>" . sp-backward-kill-sexp))
