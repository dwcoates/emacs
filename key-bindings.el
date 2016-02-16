;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; NAVIGATION GENERICS ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst my/backward-key "j")
(defconst my/forward-key "l")
(defconst my/downward-key "k")
(defconst my/upward-key "i")

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
(defconst my/transpose-key "t")
(defconst my/yank-key "f")

(defconst my/kill-big-key "a")
(defconst my/kill-or-save-key "s")

;; Allows for the mapping to C-i without remapping <tab> as well. This happens
;; otherwise because C-i and <tab> share the same ascii code. Regard "H-i"
;; as "C-i" in the following bindings.
(keyboard-translate ?\C-i ?\H-i)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  INTRA-BUFFER  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Bind general namespace keys (C-?, M-? where '?' is any single character)
(bind-keys*
    ;;;;;;;;;;;;;;;;;;;;
    ;;;; NAVIGATION ;;;;
    ;;;;;;;;;;;;;;;;;;;;
 ;; DIRECTION
 ((concat "C-" my/backward-key) . backward-char)
 ((concat "C-" my/forward-key) . forward-char)
 ((concat "H-" my/upward-key) . previous-line)
 ((concat "C-" my/downward-key) . next-line)

 ((concat "M-" my/backward-key) . backward-word)
 ((concat "M-" my/forward-key) . forward-word)
 ((concat "M-" my/downward-key) . forward-paragraph)
 ((concat "M-" my/upward-key) . backward-paragraph)

 ((concat "C-S-" my/forward-key) . scroll-left)
 ((concat "C-S-" my/backward-key) . scroll-right)
 ((concat "C-S-" my/upward-key) . scroll-down-command)
 ((concat "C-S-" my/downward-key) . scroll-up-command)

 ((concat "C-M-" my/upward-key) . scroll-other-window-down)
 ((concat "C-M-" my/downward-key) . scroll-other-window)
 ;; end/beg
 ((concat "C-" my/beginning-key) . beginning-of-line)
 ((concat "C-" my/end-key) . end-of-line)

 ((concat "M-" my/end-key) . forward-to-indentation)
 ((concat "M-" my/beginning-key) . backward-to-indentation)

 ((concat "C-M-" my/beginning-key) . beginning-of-buffer)
 ((concat "C-M-" my/end-key) . end-of-buffer)
 ;; view change
 ((concat "C-" "'") . recenter-top-bottom)
 ((concat "C-" my/smaller-key) . narrow-to-region)
 ((concat "M-" my/smaller-key) . narrow-to-page)
 ((concat "C-" my/bigger-key) . widen)
 ;; searching
 ((concat "C-" my/search-alpha-key) . isearch-forward)
 ((concat "C-" my/search-beta-key) . isearch-backward)
    ;;;;;;;;;;;;;;;;;;;;;
    ;;;;   editing   ;;;;
    ;;;;;;;;;;;;;;;;;;;;;
 ;; copy and kill
 ((concat "C-" my/kill-big-key) . kill-line)
 ((concat "C-" my/kill-or-save-key) . kill-region)

 ((concat "M-" my/kill-big-key) . kill-sentence)
 ((concat "M-" my/kill-or-save-key) . kill-ring-save)

 ((concat "C-" "z") . zap-to-char)

 ((concat "C-" "v") . delete-horizontal-space)
 ((concat "M-" "v") . delete-indentation)
 ;; transpose
 ((concat "C-" my/transpose-key) . transpose-chars)
 ((concat "M-" my/transpose-key) . transpose-words)
 ;; yank
 ((concat "C-" my/yank-key) . yank)
 ;; modify
 ((concat "C-S-M-" my/upward-key) . upcase-region)
 ((concat "C-S-M-" my/downward-key) . downcase-region)
 )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  INTER-BUFFER	 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(bind-keys*
 :prefix-map global-map
 :prefix "C-w"
 ;;;;;;;;;;;;;;;;;;;;
 ;;;; NAVIGATION ;;;;
 ;;;;;;;;;;;;;;;;;;;;
 ((concat "C-" my/forward-key) . windmove-right)
 ((concat "C-" my/backward-key) . windmove-left)
 ((concat "C-" my/upward-key) . windmove-up)
 ((concat "C-" my/downward-key) . windmove-down)

 (my/forward-key . next-buffer)
 (my/backward-key . previous-buffer)

 ("U" . switch-to-buffer-other-window)
 ("u" . switch-to-buffer)
 ("C-u" . helm-buffers-list)

 ("o" . other-frame)
 ("O" . other-window)
 ;; DIRED
 ("/" . dired-jump)
 ("?" . dired-jump-other-window)
 ;;;;;;;;;;;;;;;;;;;;;
 ;;;;	EDITING	  ;;;;
 ;;;;;;;;;;;;;;;;;;;;;
 ;; FIND FILE
 (my/search-alpha-key . ido-find-file)
 ((concat "C-" my/search-alpha-key) . ido-find-file-other-window)
 ((upcase my/search-alpha-key) . ido-find-file-other-frame)

 (my/search-beta-key . find-file)
 ((concat "C-" my/search-beta-key) . find-file-other-window)
 ((upcase my/search-beta-key) . find-file-other-frame)
 ;; TRANSPOSE
 (my/transpose-key . transpose-windows)
 ;; RESIZE
 (my/bigger-key . enlarge-window-horizontally)
 (my/smaller-key . shrink-window-horizontally)
 ((concat "C-" my/bigger-key) . enlarge-window)
 ((concat "M-" my/smaller-key) . shrink-window)

 ("b" . balance-windows)
 ("B" . shrink-window-if-larger-than-buffer)
 ("v" . golden-ratio)
 ("V" . golden-ratio-mode)

 ("z" . suspend-frame)
 ;; SPLIT
 ((upcase my/forward-key) . split-window-right)
 ((upcase my/backward-key) . split-horizontally)
 ((upcase my/upward-key) . split-window-vertically)
 ((upcase my/downward-key) . split-window-below)
 ;; KILL
 (my/kill-element-key . kill-buffer-and-window)
 ((upcase my/kill-element-key) . kill-some-buffers)
 ((concat "C-" my/kill-element-key) . kill-buffer)

 ((concat "3 " (upcase my/kill-element-key)) . delete-other-frames)
 ((concat "3 " my/kill-element-key) . delete-frame)
 ;; SAVE
 ((concat "C-" my/kill-or-save-key) . save-buffer)
 ((upcase my/kill-or-save-key) . write-file)
 ((concat "M-" my/kill-or-save-key) . save-some-bueffer)
 )
