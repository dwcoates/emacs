;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;; KEY MACROS ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; opens a shell in a new window and places cursor there
(fset 'open-shell-new-window
   [?\C-x ?2 ?\C-x ?o ?\M-x ?e ?s ?h ?e ?l ?l return])

(fset 'comment-lisp-line
   "\C-a;\C-n\C-a")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;; KEY BINDINGS ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; remaps delete-other-windows with delete window
(substitute-key-definition
 'delete-other-windows 'delete-window (current-global-map))
;; reprioritizes delete-other-windows to its rightful place
(global-set-key (kbd "C-c 1") 'delete-other-windows)


;; replaces list-buffers binding with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)


;; delete-region
(global-set-key (kbd "C-c M-m n") 'apply-macro-to-region-lines)



;; iedit
(global-set-key (kbd "C-;") 'iedit-mode)


;; google c++ style popup
(global-set-key (kbd "C-c +") 'flymake-popup-current-error-menu)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; HELM ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-c M-I") 'helm-multi-swoop-all)

(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-M-x-fuzzy-math t)

(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)

(global-set-key (kbd "C-x b") 'helm-mini)

(global-set-key (kbd "C-x C-f") 'helm-find-files)






;; ggtags
;(define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
;(define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
;(define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
;(define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
;(define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
;(define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)
;(define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)

;;(key-chord-define-global "jf"     'forward-word)
;;(key-chord-define-global "jb"     'backward-word)


;; 
;;  Macro bindings
;;

(global-set-key (kbd "C-c c l") 'comment-lisp-line)

(provide 'key-bindings)
