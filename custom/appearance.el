;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; APPEARANCE ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'default-frame-alist '(height . 65))
(add-to-list 'default-frame-alist '(width . 200))

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
;; no scroll bar please
(toggle-scroll-bar -1)


;; enable line numbering
(global-set-key (kbd "C-c l m") 'linum-mode)


;; dont truncate lines
(toggle-truncate-lines)
(global-set-key (kbd "C-c ; t") 'toggle-truncate-lines)


(provide 'appearance)
