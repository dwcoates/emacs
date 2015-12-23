;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;          Basic Appearance Settings           ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; setup default window size
(add-to-list 'default-frame-alist '(height . 55))
(add-to-list 'default-frame-alist '(width . 220))


;; get rid of annoying stuff
(setq gc-cons-threshold 100000000)
(setq inhibit-startup-message t)


;; because why not
(defalias 'yes-or-no-p 'y-or-n-p)


;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))


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
(linum-mode t)
(global-set-key (kbd "C-c l m") 'linum-mode)


;; dont truncate lines
(toggle-truncate-lines)
(global-set-key (kbd "C-c ; t") 'toggle-truncate-lines)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;       Editing and Appearance Packages        ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; function for switching between two most recently visited buffers
(defun switch-to-other-buffer ()
  "Switch to last visited buffer."
  (interactive)
  (switch-to-buffer (other-buffer)))
(global-set-key [C-c b] 'switch-to-other-buffer)



;; display time in mode line
(display-time-mode t)
;; set modeline to powerline
(powerline-default-theme)


;; guide-key, displays possible key binding completions
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-x v" "C-x 8" "C-x +"))
(guide-key-mode 1)
(setq guide-key/recursive-key-sequence-flag t)
(setq guide-key/popup-window-position 'bottom)


;; Set keys to move between windows
(global-set-key [M-s-<left>] 'windmove-left)
(global-set-key [M-s-<right>] 'windmove-right)
(global-set-key [M-s-<up>] 'windmove-up)
(global-set-key [M-s-<down>] 'windmove-down)


;; Package: smartparens
(require 'smartparens-config)
(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(sp-use-paredit-bindings)
;; turn on smartparens
(show-smartparens-global-mode +1)
(smartparens-global-mode 1)


;; Package: golden ratio
(require 'golden-ratio)
(require 'setup-helm)
;; ensure golden-ratio compatibility with helm.
(defun pl/helm-alive-p ()
  (if (boundp 'helm-alive-p)
      (symbol-value 'helm-alive-p)))
(add-to-list 'golden-ratio-inhibit-functions 'pl/helm-alive-p)
;; list of buffers to not be resized by golden-ratio.
(setq golden-ratio-exclude-buffer-names
	  '("*Flycheck errors*"
		"*SPEEDBAR*"))
;; turn on golden ratio
(golden-ratio-mode t)


;; Package: speedbar
(setq speedbar-show-unknown-files t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'setup-appearance-and-navigation)
;;; setup-appearance-and-navigation ends here
