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

(defun sp-delete-sexp ()
    (interactive)
    (sp-forward-sexp)
    (sp-backward-sexp)
    (mark)
    (sp-forward-sexp)
  (delete-region (point) (mark)))




;; Key bindings
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
 ("C-c \"" . wrap-with-double-quotes)
 ("C-c _"  . wrap-with-underscores)
 ("C-c `"  . wrap-with-back-quotes))

;; Key bindings
(key-chord-define-global "8a" 'sp-beginning-of-sexp)
(key-chord-define-global "8e" 'sp-end-of-sexp)

(key-chord-define-global "4k" 'sp-down-sexp)
(key-chord-define-global "4i"   'sp-up-sexp)
(key-chord-define-global "4j" 'sp-backward-down-sexp)
(key-chord-define-global "4l"   'sp-backward-up-sexp)

(key-chord-define-global "8f" 'sp-forward-sexp)
(key-chord-define-global "8b" 'sp-backward-sexp)

(key-chord-define-global "4n" 'sp-next-sexp)
(key-chord-define-global "4p" 'sp-previous-sexp)

(key-chord-define-global "4h" 'sp-forward-symbol)
(key-chord-define-global "8g" 'sp-backward-symbol)

(key-chord-define-global "8t" 'sp-forward-slurp-sexp)
(key-chord-define-global "8w" 'sp-forward-barf-sexp)
(key-chord-define-global "8r"  'sp-backward-slurp-sexp)
(key-chord-define-global "8q"  'sp-backward-barf-sexp)

(key-chord-define-global "4t" 'sp-transpose-sexp)
(key-chord-define-global "4k" 'sp-kill-sexp)
(key-chord-define-global "4h" 'sp-kill-hybrid-sexp)
(key-chord-define-global "4j" 'sp-backward-kill-sexp)
(key-chord-define-global "4w" 'sp-copy-sexp)

(key-chord-define-global "8d" 'sp-delete-sexp)        ;; this function doesnt exist?

;("<backspace>" . sp-backward-delete-char)
;("C-<backspace>" . backward-delete-char)     ;; this should be like paredit
;("M-<backspace>" . sp-backward-kill-word)     ;; this should be like paredit
;("M-s-<backspace>" . backward-kill-word)     ;; this should be like paredit
;([remap sp-backward-kill-word] . backward-kill-word)

(key-chord-define-global "u9" 'sp-backward-unwrap-sexp)
(key-chord-define-global "u0" 'sp-unwrap-sexp)
(key-chord-define-global "90" 'sp-rewrap-sexp) ; this is probably a poor choice, but whatever.

(key-chord-define-global "r9"  'wrap-with-parens)
(key-chord-define-global "r["  'wrap-with-brackets)
(key-chord-define-global "r{"  'wrap-with-braces)
(key-chord-define-global "r'"  'wrap-with-single-quotes)
(key-chord-define-global "r\"" 'wrap-with-double-quotes)
(key-chord-define-global "r_"  'wrap-with-underscores)
(key-chord-define-global "r`"  'wrap-with-back-quotes)


(provide 'setup-smartparens)
