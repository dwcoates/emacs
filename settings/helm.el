

(require 'helm)
(require 'helm-config)
;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(helm-mode 1)

;; when doing isearch, hand the word over to help-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
;;;;;;;;;; needs to get fixed... ;;;;;;;;;;
;;(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
;; save buffer when helm-multi-swoop-edit complete
(setq helm-multi-swoop-edit-save t)
;; if this value is t, split window inside the current window
(setq helm-swoop-split-with-multiple-windows t)
;; split direction. 'split-window-vertically or 'split-window-horizontally
(setq helm-swoop-split-direction 'split-window-vertically)
;; if nil, you can slightly boost invoke speed in exchange for text color
(setq helm-swoop-speed-or-color t)
;; go to the opposite side of line from the end or beginning of line
(setq helm-swoop-move-to-line-cycle t)
;; optional face for line numbers
;; face name is `helm-swoop-line-number-face`
(setq helm-swoop-use-line-number-face t)
