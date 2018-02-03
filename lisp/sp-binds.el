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
 ("M-s-[" . sp-rewrap-sexp))

(bind-keys*
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
 ("d" . sp-delete-word)
 ("C-d" . sp-delete-sexp)
 ("<backspace>" . sp-backward-delete-symbol)
 ("<C-backspace>" . sp-backward-kill-sexp))


(when (macrop 'defhydra)
  (defhydra hydra-smartparens (:hint none)
   "
_q_: barf backwards   _f_: forward sexp
_w_: barf forwards    _b_: backward sexp
_e_: slurp backwards  _t_: transpose sexp
_r_: slurp forwards   _n_: next line
_y_: yank             _p_: previous line"
   ("SPC" org-agenda-reset-view)
   ("q" sp-backward-barf-sexp)
   ("w" sp-forward-barf-sexp)
   ("u" sp-backward-slurp-sexp)
   ("i" sp-forward-slurp-sexp)
   ("r" nil)
   ("y" yank)
   ("t" sp-transpose-sexp)
   ("f" sp-forward-sexp)
   ("b" sp-backward-sexp)
   ("C-p" sp-previous-sexp)
   ("C-n" sp-next-sexp)
   ("a" sp-beginning-of-sexp)
   ("e" sp-end-of-sexp)
   ("C-f" sp-forward-parallel-sexp)
   ("C-b" sp-backward-parallel-sexp)
   ("p" previous-line)
   ("n" next-line)
   ("k" sp-kill-sexp)))

(let ((map (if (macrop 'bind-key*)
               override-global-map
             global-map)))
  (define-key map (kbd "M-j") 'hydra-smartparens/body))
