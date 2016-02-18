(defconst dwc-major-prefix "C-e")
(defconst dwc-inter-buffer-prefix "C-w")


(keyboard-translate ?\C-i ?\H-i)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; KEY GENERICS ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Direction
(defconst dwc-backward-key "j")
(defconst dwc-forward-key "l")
(defconst dwc-downward-key "k")
(defconst dwc-upward-key "i")
;; Beginning/End
(defconst dwc-beginning-key "o")
(defconst dwc-end-key "p")
;; Search
(defconst dwc-search-alpha-key "y")
(defconst dwc-search-beta-key "n")
;; Bigger/Smaller
(defconst dwc-bigger-key ">")
(defconst dwc-smaller-key "<")
;; Kill
(defconst dwc-kill-element-key "d")
(defconst dwc-kill-big-key "a")
(defconst dwc-kill-or-save-key "s")
;; Yank
(defconst dwc-yank-key "f")
;; Transpose
(defconst dwc-transpose-key "t")

(setq dwc-global-keymap (make-sparse-keymap))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;; INTRA-BUFFER ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; NAVIGATION ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;
;; Direction
(define-key dwc-global-keymap (kbd (concat "C-" dwc-backward-key)) 'backward-char)
(define-key dwc-global-keymap (kbd (concat "C-" dwc-forward-key)) 'forward-char)
(define-key dwc-global-keymap (kbd (concat "H-" dwc-upward-key)) 'previous-line)
(define-key dwc-global-keymap (kbd (concat "C-" dwc-downward-key)) 'next-line)
;
(define-key dwc-global-keymap (kbd (concat "M-" dwc-backward-key)) 'backward-word)
(define-key dwc-global-keymap (kbd (concat "M-" dwc-forward-key)) 'forward-word)
(define-key dwc-global-keymap (kbd (concat "M-" dwc-downward-key)) 'forward-paragraph)
(define-key dwc-global-keymap (kbd (concat "M-" dwc-upward-key)) 'backward-paragraph)
;
(define-key dwc-global-keymap (kbd (concat "C-S-" dwc-forward-key)) 'scroll-left)
(define-key dwc-global-keymap (kbd (concat "C-S-" dwc-backward-key)) 'scroll-right)
(define-key dwc-global-keymap (kbd (concat "C-S-" dwc-upward-key)) 'scroll-down-command)
(define-key dwc-global-keymap (kbd (concat "C-S-" dwc-downward-key)) 'scroll-up-command)
;; Beginning/End
(define-key dwc-global-keymap (kbd (concat "C-M-" dwc-upward-key)) 'scroll-other-window-down)
(define-key dwc-global-keymap (kbd (concat "C-M-" dwc-downward-key)) 'scroll-other-window)
;
(define-key dwc-global-keymap (kbd (concat "C-" dwc-beginning-key)) 'beginning-of-line)
(define-key dwc-global-keymap (kbd (concat "C-" dwc-end-key)) 'end-of-line)
;
(define-key dwc-global-keymap (kbd (concat "M-" dwc-end-key)) 'forward-to-indentation)
(define-key dwc-global-keymap (kbd (concat "M-" dwc-beginning-key)) 'backward-to-indentation)
;
(define-key dwc-global-keymap (kbd (concat "C-M-" dwc-beginning-key)) 'beginning-of-buffer)
(define-key dwc-global-keymap (kbd (concat "C-M-" dwc-end-key)) 'end-of-buffer)
;; Bigger/Smaller
(define-key dwc-global-keymap (kbd (concat "C-" dwc-smaller-key)) 'narrow-to-region)
(define-key dwc-global-keymap (kbd (concat "M-" dwc-smaller-key)) 'narrow-to-page)
(define-key dwc-global-keymap (kbd (concat "C-" dwc-bigger-key)) 'widen)
;; Search
(define-key dwc-global-keymap (kbd (concat "C-" dwc-search-alpha-key)) 'isearch-forward)
(define-key dwc-global-keymap (kbd (concat "C-" dwc-search-beta-key)) 'isearch-backward)
;; Misc
(define-key dwc-global-keymap (kbd "C-'") 'recenter-top-bottom)
;;;;;;;;;;;;;;;;;;;;;
;;;;;; EDITING ;;;;;;
;;;;;;;;;;;;;;;;;;;;;
;; Direction
(define-key dwc-global-keymap (kbd (concat "C-S-M-" dwc-upward-key)) 'upcase-region)
(define-key dwc-global-keymap (kbd (concat "C-S-M-" dwc-downward-key)) 'downcase-region)
;; Kill
(define-key dwc-global-keymap (kbd (concat "C-" dwc-kill-big-key)) 'kill-line)
(define-key dwc-global-keymap (kbd (concat "C-" dwc-kill-or-save-key)) 'kill-region)
;
(define-key dwc-global-keymap (kbd (concat "M-" dwc-kill-big-key)) 'kill-sentence)
(define-key dwc-global-keymap (kbd (concat "M-" dwc-kill-or-save-key)) 'kill-ring-save)
;
(define-key dwc-global-keymap (kbd "C-z") 'zap-to-char)
;; Whitespace
(define-key dwc-global-keymap (kbd "C-v") 'delete-horizontal-space)
(define-key dwc-global-keymap (kbd "M-v") 'delete-indentation)
;; Transpose
(define-key dwc-global-keymap (kbd (concat "C-" dwc-transpose-key)) 'transpose-chars)
(define-key dwc-global-keymap (kbd (concat "M-" dwc-transpose-key)) 'transpose-words)
;; Yank
(define-key dwc-global-keymap (kbd (concat "C-" dwc-yank-key)) 'yank)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; INTER-BUFFER ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-prefix-command 'dwc-inter-buffer-keymap)
(define-key dwc-global-keymap (kbd dwc-inter-buffer-prefix) 'dwc-inter-buffer-keymap)
;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; NAVIGATION ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;
(define-key dwc-inter-buffer-keymap (kbd dwc-forward-key) 'next-buffer)
(define-key dwc-inter-buffer-keymap (kbd dwc-backward-key) 'previous-buffer)
;
(define-key dwc-inter-buffer-keymap (kbd "U") 'switch-to-buffer-other-window)
(define-key dwc-inter-buffer-keymap (kbd "u") 'switch-to-buffer)
(define-key dwc-inter-buffer-keymap (kbd "C-u") 'helm-buffers-list)
;
(define-key dwc-inter-buffer-keymap (kbd "o") 'other-frame)
(define-key dwc-inter-buffer-keymap (kbd "O") 'other-window)
;; DIRED
(define-key dwc-inter-buffer-keymap (kbd "/") 'dired-jump)
(define-key dwc-inter-buffer-keymap (kbd "?") 'dired-jump-other-window)
;; FIND FILE
(define-key dwc-inter-buffer-keymap (kbd dwc-search-alpha-key) 'ido-find-file)
(define-key dwc-inter-buffer-keymap (kbd (concat "C-" dwc-search-alpha-key)) 'ido-find-file-other-window)
(define-key dwc-inter-buffer-keymap (kbd (upcase dwc-search-alpha-key)) 'ido-find-file-other-frame)
;;;;;;;;;;;;;;;;;;;;;
;;;;;; EDITING ;;;;;;
;;;;;;;;;;;;;;;;;;;;;
;; RESIZE
(define-key dwc-inter-buffer-keymap (kbd dwc-bigger-key) 'enlarge-window-horizontally)
(define-key dwc-inter-buffer-keymap (kbd dwc-smaller-key) 'shrink-window-horizontally)
(define-key dwc-inter-buffer-keymap (kbd (concat "C-" dwc-bigger-key)) 'enlarge-window)
(define-key dwc-inter-buffer-keymap (kbd (concat "M-" dwc-smaller-key)) 'shrink-window)
;
(define-key dwc-inter-buffer-keymap (kbd "b") 'balance-windows)
(define-key dwc-inter-buffer-keymap (kbd "B") 'shrink-window-if-larger-than-buffer)
(define-key dwc-inter-buffer-keymap (kbd "v") 'golden-ratio)
(define-key dwc-inter-buffer-keymap (kbd "V") 'golden-ratio-mode)
;
(define-key dwc-inter-buffer-keymap (kbd "z") 'suspend-frame)
;; SPLIT
(define-key dwc-inter-buffer-keymap (kbd (upcase dwc-forward-key)) 'split-window-right)
(define-key dwc-inter-buffer-keymap (kbd (upcase dwc-backward-key)) 'split-horizontally)
(define-key dwc-inter-buffer-keymap (kbd (upcase dwc-upward-key)) 'split-window-vertically)
(define-key dwc-inter-buffer-keymap (kbd (upcase dwc-downward-key)) 'split-window-below)
;; KILL
(define-key dwc-inter-buffer-keymap (kbd dwc-kill-element-key) 'kill-buffer-and-window)
(define-key dwc-inter-buffer-keymap (kbd (upcase dwc-kill-element-key)) 'kill-some-buffers)
(define-key dwc-inter-buffer-keymap (kbd (concat "C-" dwc-kill-element-key)) 'kill-buffer)
;
(define-key dwc-inter-buffer-keymap (kbd (concat "3 " (upcase dwc-kill-element-key))) 'delete-other-frames)
(define-key dwc-inter-buffer-keymap (kbd (concat "3 " dwc-kill-element-key)) 'delete-frame)
;; SAVE
(define-key dwc-inter-buffer-keymap (kbd (concat "C-" dwc-kill-or-save-key)) 'save-buffer)
(define-key dwc-inter-buffer-keymap (kbd (upcase dwc-kill-or-save-key)) 'write-file)
(define-key dwc-inter-buffer-keymap (kbd (concat "M-" dwc-kill-or-save-key)) 'save-some-bueffer)




(define-minor-mode dwc-bindings-mode
  "Organized alternative bindings to Emacs default."
  :lighter " dwcB"
  :global t
  :keymap dwc-global-keymap
  )
