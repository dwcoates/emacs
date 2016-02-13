;;;;;;;;;;;; NAVIGATION ;;;;;;;;;;;;;;;;
(defconst my/back-char-key "C-j")
(defconst my/forward-char-key "C-l")
(defconst my/prev-line-key "C-k")
(defconst my/next-line-key "C-i")

(defconst my/beg-line-key "C-o")
(defconst my/end-line-key "C-p")

(defconst my/isearch-forward-key "C-y")
(defconst my/isearch-back-key "C-n")

(defconst my/scroll-down-key "C-m")
(defconst my/scroll-up-key "M-m")

;;;;;;;;;;;;;; EDITING ;;;;;;;;;;;;;;;;;
(defconst my/kill-line-key "C-a")
(defconst my/kill-region-key "C-f")

(defconst my/trans-chars-key "C-,")
(defconst my/trans-words-key "M-,")

(defconst my/yank-key "C-t")


;; Wrapped in a progn for testing.
(progn
  ;;;;;;;;;;;;;;;;;;;;
  ;;;; NAVIGATION ;;;;
  ;;;;;;;;;;;;;;;;;;;;
  ;; DIRECTION
  (bind-key my/back-char-key 'backward-char)
  (bind-key my/forward-char-key 'forward-char)
  (bind-key my/prev-line-key 'previous-line)
  (bind-key my/next-line-key 'next-line)
  ;; END/BEG LINE
  (bind-key my/beg-line-key 'beginning-of-line)
  (bind-key my/end-line-key 'end-of-line)
  ;; SEARCHING
  (bind-key my/isearch-forward-key 'isearch-forward)
  (bind-key my/isearch-back-key 'isearch-backward)
  ;; SCROLL
  (bind-key my/scroll-down-key 'scroll-down-command)
  (bind-key my/scroll-up-key 'scroll-up-command)

  ;;;;;;;;;;;;;;;;;;;;;
  ;;;;   EDITING   ;;;;
  ;;;;;;;;;;;;;;;;;;;;;
  ;; KILL
  (bind-key my/kill-line-key 'kill-line)
  (bind-key my/kill-region-key 'kill-region)
  ;; TRANSPOSE
  (bind-key my/trans-chars-key 'transpose-chars)
  (bind-key my/trans-words-key 'transpose-words)
  ;; YANK
  (bind-key my/yank-key 'yank)
  )
