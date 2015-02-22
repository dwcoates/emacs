;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; APPEARANCE ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'default-frame-alist '(height . 1000))
(add-to-list 'default-frame-alist '(width . 3000))


;; set emacs gui size and location
;;(if (window-system)
;;    (set-frame-size (selected-frame) 1800 1000 1))

;;(set-frame-width (selected-frame) 500)
;;(set-frame-position (selected-frame) 2000 50)

;; no splash screen
(setq inhibit-startup-screen t)

;; set font
(add-to-list 'default-frame-alist
	     '(font .  "DejaVu Sans Mono-09"))
(set-face-attribute 'default t :font  "DejaVu Sans Mono-09")

;; disables tool bar
(tool-bar-mode -1)
;; disables menu bar, can re-enable as a popub with 'C-mouse-3'
(menu-bar-mode -99)
;; enable line numbering
(linum-mode t)

(provide 'appearance)

