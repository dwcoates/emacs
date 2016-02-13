(progn
  ;;;;;;;;;;;;;;;;;;;;
  ;;;; NAVIGATION ;;;;
  ;;;;;;;;;;;;;;;;;;;;
  ;; DIRECTION
  (bind-key "C-j" 'backward-char)
  (bind-key "C-l" 'forward-char)
  (bind-key "C-k" 'previous-line)
  (bind-key "C-i" 'next-line)
  ;; END/BEG LINE
  (bind-key "C-o" 'beginning-of-line)
  (bind-key "C-p" 'end-of-line)
  ;; SEARCHING
  (bind-key "C-y" 'isearch-forward)
  (bind-key "C-n" 'isearch-backward)
  ;; SCROLL
  (bind-key "C-m" 'scroll-down-command)
  (bind-key "M-m" 'scroll-up-command)

  ;;;;;;;;;;;;;;;;;;;;;;
  ;;;;   EDITING   ;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;
  ;; KILL
  (bind-key "C-a" 'kill-line)
  (bind-key "C-f" 'kill-region)
  ;; TRANSPOSE
  (bind-key "C-" ')
  ;; YANK
  (bind-key "C-f" 'yank)
  )
