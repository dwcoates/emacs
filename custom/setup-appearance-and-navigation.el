;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;          Basic Appearance Settings           ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Package: key-chord
(require 'key-chord)
(key-chord-mode 1)
;; Max time delay between two key presses to be considered a key chord
(setq key-chord-two-keys-delay 0.1)
;; Max time delay between two presses of the same key to be considered a key chord.
;; Should normally be a little longer than `key-chord-two-keys-delay'.
(setq key-chord-one-key-delay 0.2)

(key-chord-define-global "fg" 'iy-go-to-char)
(key-chord-define-global "df" 'iy-go-to-char-backward)



;; setup default window size
(add-to-list 'default-frame-alist '(height . 40))
(add-to-list 'default-frame-alist '(width . 160))


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
         '(font .  "Ubuntu Mono-13"))
(set-face-attribute 'default t :font  "Ubuntu Mono-13")


(global-unset-key (kbd "C-x 5 0"))
(global-set-key (kbd "C-x 5 DEL") 'delete-frame)

(global-unset-key (kbd "C-x 0"))
(global-set-key (kbd "C-x DEL") 'delete-window)


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

(blink-cursor-mode 0)

(global-hl-line-mode 1)
(set-face-background 'hl-line "#3b3b3b")
(set-face-foreground 'highlight nil)

(global-set-key (kbd "C-x w b") 'previous-buffer)
;(global-set-key (kbd "C-x w v") '(switch-to-buffer nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;       Editing and Appearance Packages        ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; function for switching between two most recently visited buffers
(defun switch-to-other-buffer ()
  "Switch to last visited buffer."
  (interactive)
  (switch-to-buffer (other-buffer)))
(global-set-key (kbd "C-c b") 'switch-to-other-buffer)


(defun toggle-maximize-buffer ()
  "Maximize/minimize buffer"
       (interactive)
       (if (= 1 (length (window-list)))
           (jump-to-register '_)
         (progn
           (window-configuration-to-register '_)
           (delete-other-windows))))
(key-chord-define-global "xm" 'toggle-maximize-buffer)


(defun transpose-windows (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))
(global-set-key (kbd "C-x w t") 'transpose-windows)


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
(global-set-key (kbd "C-x w j") 'windmove-left)
(global-set-key (kbd "C-x w l") 'windmove-right)
(global-set-key (kbd "C-x w i") 'windmove-up)
(global-set-key (kbd "C-x w k") 'windmove-down)
;; corresponding key-chord configs
(key-chord-define-global "xj" windmove-left')
(key-chord-define-global "xl" windmove-right')
(key-chord-define-global "xi" windmove-up')
(key-chord-define-global "xk" windmove-down)



;; Package: smartparens
(require 'smartparens-config)
(setq sp-base-key-bindings 'paredit)
(setq sp-autoskibp-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(sp-use-paredit-bindings)
;; turn on smartparens
(show-smartparens-global-mode +1)
(smartparens-global-mode 1)

;; Package: rainbow-delimiters
(require 'rainbow-delimiters)
(rainbow-delimiters-mode)

;; Package: ace-window
(require 'ace-window)
(key-chord-define-global "xo" 'ace-window)


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
(global-set-key (kbd "C-x w g SPC") 'golden-ratio)
(global-set-key (kbd "C-x w g m") 'golden-ratio-mode)

;; Package: speedbar
(setq speedbar-show-unknown-files t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;   Functions;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(provide 'setup-appearance-and-navigation)
;;; setup-appearance-and-navigation ends here
