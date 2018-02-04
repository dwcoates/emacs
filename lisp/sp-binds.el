


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
