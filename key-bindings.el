;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; NAVIGATION GENERICS ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst my/backward-key "j")
(defconst my/forward-key "l")
(defconst my/downward-key "i")
(defconst my/upward-key "k")

(defconst my/beginning-key "o")
(defconst my/end-key "p")

(defconst my/search-alpha-key "y")
(defconst my/search-beta-key "n")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; EDITING GENERICS ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst my/kill-element-key "d")
(defconst my/transpose-key "r")
(defconst my/yank-key "t")




;;;;;;;;;;;;;;;;;;;;
;;;; NAVIGATION ;;;;
;;;;;;;;;;;;;;;;;;;;
(defconst my/kill-line-key "a")
(defconst my/kill-region-key "f")
;;;;;;;;;;;;;;;;;;;;;
;;;;   EDITING   ;;;;
;;;;;;;;;;;;;;;;;;;;;

;; Bind general namespace keys (C-?, M-? where '?' is any single character)
(bind-keys
 ;;;;;;;;;;;;;;;;;;;;
 ;;;; NAVIGATION ;;;;
 ;;;;;;;;;;;;;;;;;;;;
 ;; DIRECTION
 ((concat "C-" my/backward-key) . backward-char)
 ((concat "C-" my/forward-key) . forward-char)
 ((concat "C-" my/downward-key) . previous-line)
 ((concat "C-" my/upward-key) . next-line)

 ((concat "M-" my/backward-key) . backward-word)
 ((concat "M-" my/forward-key) . forward-word)
 ((concat "M-" my/downward-key) . scroll-up-command)
 ((concat "M-" my/upward-key) . scroll-down-command)

 ((concat "M-" my/upward-key) . scroll-down-command)
 ((concat "M-" my/upward-key) . scroll-down-command)

 ;; END/BEG
 ((concat "C-" my/beginning-key) . beginning-of-line)
 ((concat "C-" my/end-key) . end-of-line)

 ((concat "M-" my/beginning-key) . beginning-of-buffer)
 ((concat "M-" my/end-key) . end-of-buffer)
 ;; SEARCHING
 ((concat "C-" my/search-alpha-key) . isearch-forward)
 ((concat "C-" my/search-beta-key) . isearch-backward)
 ;;;;;;;;;;;;;;;;;;;;;
 ;;;;   EDITING   ;;;;
 ;;;;;;;;;;;;;;;;;;;;;
 ;; COPY and KILL
 ((concat "C-" my/kill-line-key) . kill-line)
 ((concat "C-" my/kill-region-key) . kill-region)

 ((concat "M-" my/kill-line-key) . kill-sentence)
 ((concat "M-" my/kill-region-key) . kill-ring-save)

 ((concat "C-" my/kill-region-key) . zap-to-char)
 ;; TRANSPOSE
 ((concat "C-" my/transpose-key) . transpose-chars)
 ((concat "M-" my/transpose-key) . transpose-words)
 ;; YANK
 ((concat "C-" my/yank-key) . yank)
 ;; MODIFY
 ((concat "C-S-" my/upward-key) . upcase-region)
 ((concat "C-S-" my/downward-key) . downcase-region)
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; KEY CHORDS ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(key-chord-define-global "p[" 'recenter-top-bottom)






(bind-key* "C-w N" 'beginning-of-buffer)
(bind-key* "C-S-p" 'beginning-of-buffer)

;; Bind window manipulation keys (Prefix 'C-w')
(bind-keys*
 :prefix-map global-map
 :prefix "C-w"
 ;;;;;;;;;;;;;;;;;;;;
 ;;;; NAVIGATION ;;;;
 ;;;;;;;;;;;;;;;;;;;;
 (my/forward-key . windmove-right)
 (my/backward-key . windmove-left)
 (my/upward-key . windmove-up)
 (my/downward-key . windmove-down)

 ("p" . other-window)
 ("o" . other-frame)

 ()
 ;; navigate buffers
 (my/search-alpha-key . helm-buffers-list)
 ;;;;;;;;;;;;;;;;;;;;;
 ;;;;   EDITING   ;;;;
 ;;;;;;;;;;;;;;;;;;;;;
 ;; find file
 (my/yank-key . find-file)
 ((concat "C-" my/yank-key) . find-file-other-window)
 ((upcase-word my/yank-key) . find-file-other-frame)

 (my/transpose-key . transpose-windows)

 ("z" . suspend-frame)
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst my/other-window-key "C-k")
(defconst my/other-n "C-k")

;; FILE NAVIGATION
(defconst my/other-buffer "C-k")
(defconst my/helm-buffer-list-key "C-k")
