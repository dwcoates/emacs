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

(defconst my/bigger-key ">")
(defconst my/smaller-key "<")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; EDITING GENERICS ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst my/kill-element-key "d")
(defconst my/transpose-key "r")
(defconst my/yank-key "t")

(defconst my/kill-big-key "a")
(defconst my/kill-or-save-key "f")






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;  GENERAL NAMESPACE  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
 ((concat "M-" my/downward-key) . forward-paragraph)
 ((concat "M-" my/upward-key) . backward-paragraph)

 ((concat "C-S-" my/backward-key) . scroll-left)
 ((concat "C-S-" my/forward-key) . scroll-right)
 ((concat "C-S" my/downward-key) . scroll-up-command)
 ((concat "C-S" my/upward-key) . scroll-down-command)

 ((concat "M-S-" my/downward-key) . scroll-other-window-down)
 ((concat "M-S-" my/upward-key) . scroll-other-window)
 ;; END/BEG
 ((concat "C-" my/beginning-key) . beginning-of-line)
 ((concat "C-" my/end-key) . end-of-line)

 ((concat "M-" my/end-key) . forward-to-indentation)
 ((concat "M-" my/beginning-key) . backward-to-indentation)

 ((concat "C-M-" my/beginning-key) . beginning-of-buffer)
 ((concat "C-M-" my/end-key) . end-of-buffer)
 ;; VIEW CHANGE
 ((concat "C-" "'") . recenter-top-bottom)
 ((concat "C-" my/smaller-key) . narrow-to-region)
 ((concat "C-M-" my/smaller-key) . narrow-to-page)
 ((concat "C-" my/bigger-key) . widen)
 ;; SEARCHING
 ((concat "C-" my/search-alpha-key) . isearch-forward)
 ((concat "C-" my/search-beta-key) . isearch-backward)
 ;;;;;;;;;;;;;;;;;;;;;
 ;;;;   EDITING   ;;;;
 ;;;;;;;;;;;;;;;;;;;;;
 ;; COPY and KILL
 ((concat "C-" my/kill-big-key) . kill-line)
 ((concat "C-" my/kill-or-save-key) . kill-region)

 ((concat "M-" my/kill-big-key) . kill-sentence)
 ((concat "M-" my/kill-or-save-key) . kill-ring-save)

 ((concat "C-" "z") . zap-to-char)

 ((concat "C-" "v") . delete-horizontal-space)
 ((concat "M-" "v") . delete-indentation)
 ;; TRANSPOSE
 ((concat "C-" my/transpose-key) . transpose-chars)
 ((concat "M-" my/transpose-key) . transpose-words)
 ;; YANK
 ((concat "C-" my/yank-key) . yank)
F ;; MODIFY
 ((concat "C-S-" my/upward-key) . upcase-region)
 ((concat "C-S-" my/downward-key) . downcase-region)
 )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; KEY CHORDS ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(key-chord-define-global "p[" 'recenter-top-bottom)






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;  WINDOW NAMESPACE  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

 ((upcase-word my/upward-key) . scroll-other-window)
 ((upcase-word my/downward-key) . scroll-other-window-down)
 ;; NAVIGATE BUFFERS/WINDOWS
 ((concat "M-" my/forward-key) . next-buffer)
 ((concat "M-" my/backward-key) . previous-buffer)
 ("U" . switch-to-buffer-other-window)
 ("u" . switch-to-buffer)
 ("o" . other-window)
 ("p" . other-frame)
 ;; SEARCH
 (my/search-alpha-key . helm-buffers-list)
 ;; DIRED
 ("/" . dired-jump)
 ("?" . dired-jump-other-window)

 ;;;;;;;;;;;;;;;;;;;;;
 ;;;;   EDITING   ;;;;
 ;;;;;;;;;;;;;;;;;;;;;
 ;; FIND
 (my/search-alpha-key . find-file-other-window)
 ((concat "C-" my/search-alpha-key) . find-file)
 ((upcase-word my/search-alpha-key) . find-file-other-frame)

 (my/search-beta-key . ido-find-file-other-window)
 ((concat "C-" my/search-beta-key) . ido-find-file)
 ((upcase-word my/search-beta-key) . ido-find-file-other-frame)
 ;; TRANSPOSE
 (my/transpose-key . transpose-windows)
 ;; RESIZE
 (my/bigger-key . enlarge-window-horizontally)
 (my/smaller-key . shrink-window-horizontally)
 ((concat "C-" my/bigger-key) . enlarge-window)
 ((concat "C-" my/smaller-key) . shrink-window)
 ("b" . balance-windows)
 ("B" . shrink-window-if-larger-than-buffer)
 ("v" . golden-ratio)
 ("V" . golden-ratio-mode)
 ;; SPLIT
 ((upcase my/forward-key) . 'split-window-right)
 ((upcase my/backward-key) . 'split-window-left)
 ((upcase my/upward-key) . 'split-window-up)
 ((upcase my/downward-key) . 'splite-window-down)
 ;; KILL
 (my/kill-element-key . delete-window)
 ((concat "3 " (upcase-region my/kill-element-key)) . delete-other-frame)
 ((concat "3 " (concat "C-" my/kill-element-key)) . delete-other-frames)

 ((upcase-region my/kill-element-key) . kill-buffer)
 ((concat "C-" my/kill-element-key) . kill-buffer-and-window)
 ;; SAVE
 (("C-" my/kill-or-save-key) . 'save-buffer)
 ((upcase my/kill-or-save-key) . 'write-file)
 ((concat "M-" my/kill-or-save-key) . 'save-some-buffer)

 ("z" . suspend-frame)
 )
