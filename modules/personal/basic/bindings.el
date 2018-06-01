;;; private/default/+bindings.el -*- lexical-binding: t; -*-

;; This files defines a Spacemacs-esque keybinding scheme

(map! :map global-map
      "S-<return>" #'dired-find-file-conservatively
      "RET"        #'newline-and-indent
      "C-x w"      #'whitespace-mode
      "C-c <delete>" #'delete-region

      )

(map! :map global-map
      :prefix "C-x"
      "b" #'doom/previous-buffer
      "d" #'git-gutter:previous-hunk
      "w" #'+workspace/switch-left
      "e" #'previous-error
      "h" #'smart-backward)

(unbind-key "C-z" global-map) ;; Remove suspend-frame binding (also bound to C-x C-z)

(map!
 (:desc "workspace" :prefix "C-z"
   :desc "Display tab bar"          "TAB" #'+workspace/display
   :desc "Cycle to next workspace"  "n"   #'+workspace/cycle
   :desc "Create new workspace"     "c"   #'+workspace/new
   :desc "Load workspace from file" "l"   #'+workspace/load
   :desc "Load last session"        "L"   (λ! (+workspace/load-session))
   :desc "Save workspace to file"   "s"   #'+workspace/save
   :desc "Autosave current session" "S"   #'+workspace/save-session
   :desc "Switch workspace"         "."   #'+workspace/switch-to
   :desc "Delete session"           "X"   #'+workspace/kill-session
   :desc "Delete this workspace"    "d"   #'+workspace/delete
   :desc "Load session"             "L"   #'+workspace/load-session
   :desc "Next workspace"           "]"   #'+workspace/switch-right
   :desc "Previous workspace"       "["   #'+workspace/switch-left
   :desc "Switch to 1st workspace"  "1"   (λ! (+workspace/switch-to 0))
   :desc "Switch to 2nd workspace"  "2"   (λ! (+workspace/switch-to 1))
   :desc "Switch to 3rd workspace"  "3"   (λ! (+workspace/switch-to 2))
   :desc "Switch to 4th workspace"  "4"   (λ! (+workspace/switch-to 3))
   :desc "Switch to 5th workspace"  "5"   (λ! (+workspace/switch-to 4))
   :desc "Switch to 6th workspace"  "6"   (λ! (+workspace/switch-to 5))
   :desc "Switch to 7th workspace"  "7"   (λ! (+workspace/switch-to 6))
   :desc "Switch to 8th workspace"  "8"   (λ! (+workspace/switch-to 7))
   :desc "Switch to 9th workspace"  "9"   (λ! (+workspace/switch-to 8))))

(map!
 (:desc "popups" :prefix "C-x"
   :desc "Cycle through popup windows" "p" #'doom/other-popup))

(unbind-key "C-x f" global-map)

(map!
 (:desc "file" :prefix "C-x"
   :desc "Find file"                 "C-f" #'find-file
   :desc "Find file in project"      "M-f" #'projectile-find-file
   (:prefix "f"
     :desc "Sudo find file"            "F" #'doom/sudo-find-file
     :desc "Find file from here"       "?" #'counsel-file-jump
     :desc "Find other file"           "a" #'projectile-find-other-file
     :desc "Open project editorconfig" "c" #'editorconfig-find-current-editorconfig
     :desc "Recent files"              "r" #'recentf-open-files
     :desc "Recent project files"      "R" #'projectile-recentf
     :desc "Yank filename"             "y" #'+default/yank-buffer-filename)))

(map!
 (:desc "buffer" :prefix "C-x"
   :desc "Switch buffer"           "B" #'switch-to-buffer
   :desc "Kill buffer"             "k" #'doom/kill-this-buffer
   :desc "Kill other buffers"      "o" #'doom/kill-other-buffers
   :desc "Pop scratch buffer"      "x" #'doom/open-scratch-buffer
   :desc "Bury buffer"             "z" #'bury-buffer
   :desc "Next buffer"             "]" #'doom/next-buffer
   :desc "Previous buffer"         "[" #'doom/previous-buffer
   :desc "Sudo edit this file"     "S" #'doom/sudo-this-file))

(map!
 (:desc "help" :prefix "C-h"
   :desc "Apropos"                "a" #'apropos
   :desc "Reload theme"           "R" #'doom//reload-theme
   :desc "Find library"           "l" #'find-library
   :desc "Toggle Emacs log"       "m" #'doom/popup-toggle-messages
   :desc "Command log"            "L" #'global-command-log-mode
   :desc "Describe function"      "f" #'describe-function
   :desc "Describe key"           "k" #'describe-key
   :desc "Describe char"          "c" #'describe-char
   :desc "Describe mode"          "M" #'describe-mode
   :desc "Describe variable"      "v" #'describe-variable
   :desc "Describe face"          "F" #'describe-face
   :desc "Describe DOOM setting"  "s" #'doom/describe-setting
   :desc "Describe DOOM module"   "d" #'doom/describe-module
   :desc "Find definition"        "." #'+jump/definition
   :desc "Find references"        "/" #'+jump/references
   :desc "Find documentation"     "h" #'+jump/documentation
   :desc "What face"              "'" #'doom/what-face
   :desc "What minor modes"       ";" #'doom/what-minor-mode
   :desc "Info"                   "i" #'info
   :desc "Toggle profiler"        "p" #'doom/toggle-profiler))

(define-key input-decode-map [?\C-m] [C-m]) ;; Distinguis C-m from carriage return

(map!
 (:desc "git" :prefix "<C-m>"
   :desc "Git status"             "s" #'magit-status
   :desc "Git blame"              "b" #'magit-blame
   :desc "Git time machine"       "t" #'git-timemachine-toggle
   :desc "Git stage hunk"         "a" #'git-gutter:stage-hunk
   :desc "Git revert hunk"        "r" #'git-gutter:revert-hunk
   :desc "Git revert buffer"      "R" #'vc-revert
   :desc "List gists"             "g" #'+gist:list
   :desc "Next hunk"              "]" #'git-gutter:next-hunk
   :desc "Previous hunk"          "[" #'git-gutter:previous-hunk))

(map!
 :prefix "C-x"
 (:desc "toggle" :prefix "t"
   :desc "Flyspell"               "s" #'flyspell-mode
   :desc "Flycheck"               "f" #'flycheck-mode
   :desc "Line numbers"           "l" #'doom/toggle-line-numbers
   :desc "Fullscreen"             "f" #'doom/toggle-fullscreen
   :desc "Indent guides"          "i" #'highlight-indentation-mode
   :desc "Indent guides (column)" "I" #'highlight-indentation-current-column-mode
   :desc "Impatient mode"         "h" #'+impatient-mode/toggle
   :desc "Big mode"               "b" #'doom-big-font-mode))

(map!
 :prefix "C-x"
 (:desc "project" :prefix "P"
   :desc "Browse project"           "." #'+default/browse-project
   :desc "Find file in project"     "/" #'projectile-find-file
   :desc "Run cmd in project root"  "!" #'projectile-run-shell-command-in-root
   :desc "Switch project"           "p" #'projectile-switch-project
   :desc "Recent project files"     "r" #'projectile-recentf
   :desc "List project tasks"       "t" #'+ivy/tasks
   :desc "Pop term in project"      "o" #'+term/open-popup-in-project
   :desc "Invalidate cache"         "x" #'projectile-invalidate-cache))

(unbind-key "C-x s" global-map)

(map!
 :prefix "C-x"
 (:desc "snippets" :prefix "s"
   :desc "New snippet"             "n" #'yas-new-snippet
   :desc "Insert snippet"          "i" #'yas-insert-snippet
   :desc "Find snippet for mode"   "s" #'yas-visit-snippet-file
   :desc "Find snippet"            "S" #'+default/find-in-snippets))

(map!
 :prefix "C-x"
 (:desc "code" :prefix "c"
   :desc "List errors"                "x" #'flycheck-list-errors
   :desc "Evaluate buffer/region"     "e" #'+eval/buffer
   :desc "Evaluate region"            "e" #'+eval/region
   :desc "Evaluate & replace region"  "E" #'+eval:replace-region
   :desc "Build tasks"                "b" #'+eval/build
   :desc "Jump to definition"         "d" #'+jump/definition
   :desc "Jump to references"         "D" #'+jump/references
   :desc "Open REPL"                  "r" #'+eval/open-repl))

(unbind-key "C-x e" global-map)

(map!
 :prefix "C-x"
 (:desc "errors" :prefix "e"
   :desc "Next error" "n" #'next-error
   :desc "Previous error" "p" #'previous-error
   :desc "Correct next word spelling" "w" #'flyspell-correct-word-generic
   :desc "Correct previous word spelling" "W" #'flyspell-correct-previous-word-generic
   (:after flycheck
     :map flycheck-error-list-mode-map
     "C-n" #'flycheck-error-list-next-error
     "C-p" #'flycheck-error-list-previous-error
     "j"   #'flycheck-error-list-next-error
     "RET" #'flycheck-error-list-goto-error)))

;; (map! 
;;       [remap find-tag]         #'projectile-find-tag

;;       ;; Other sensible, textmate-esque global bindings
;;       :ne "M-r"   #'+eval/buffer
;;       :ne "M-R"   #'+eval/region-and-replace
;;       :ne "M-b"   #'+eval/build
;;       :ne "M-a"   #'mark-whole-buffer
;;       :ne "M-f"   #'swiper

;;       "C-x p"     #'doom/other-popup

;;       ;; --- <leader> -------------------------------------
;;       :desc "Pop up scratch buffer"   :nv "x"  #'doom/open-scratch-buffer
;;       :desc "Org Capture"             :nv "X"  #'+org-capture/open

;;       ;; Most commonly used
;;       :desc "Find file in project"    :n "SPC" #'projectile-find-file
;;       :desc "Toggle last popup"       :n "~"   #'doom/popup-toggle
;;       :desc "Blink cursor line"       :n "DEL" #'+doom/blink-cursor
;;       :desc "Jump to bookmark"        :n "RET" #'bookmark-jump

;;       (:desc "insert" :prefix "i"
;;         :desc "From kill-ring"        :nv "y" #'counsel-yank-pop
;;         :desc "From snippet"          :nv "s" #'yas-insert-snippet)

;;       (:desc "notes" :prefix "n"
;;         :desc "Find file in notes"    :n  "n" #'+default/find-in-notes
;;         :desc "Browse notes"          :n  "N" #'+default/browse-notes
;;         :desc "Org capture"           :n  "x" #'+org-capture/open
;;         :desc "Browse mode notes"     :n  "m" #'+org/browse-notes-for-major-mode
;;         :desc "Browse project notes"  :n  "p" #'+org/browse-notes-for-project)

;;       (:desc "open" :prefix "o"
;;         :desc "Default browser"       :n  "b" #'browse-url-of-file
;;         :desc "Debugger"              :n  "d" #'+debug/open
;;         :desc "REPL"                  :n  "r" #'+eval/open-repl
;;         :v  "r" #'+eval:repl
;;         :desc "Neotree"               :n  "n" #'+neotree/toggle
;;         :desc "Terminal"              :n  "t" #'+term/open-popup
;;         :desc "Terminal in project"   :n  "T" #'+term/open-popup-in-project

;;         ;; applications
;;         :desc "APP: elfeed"           :n "E" #'=rss
;;         :desc "APP: email"            :n "M" #'=email
;;         :desc "APP: twitter"          :n "T" #'=twitter
;;         :desc "APP: regex"            :n "X" #'=regex

;;         ;; macos
;;         (:when IS-MAC
;;           :desc "Reveal in Finder"          :n "o" #'+macos/reveal-in-finder
;;           :desc "Reveal project in Finder"  :n "O" #'+macos/reveal-project-in-finder
;;           :desc "Send to Transmit"          :n "u" #'+macos/send-to-transmit
;;           :desc "Send project to Transmit"  :n "U" #'+macos/send-project-to-transmit
;;           :desc "Send to Launchbar"         :n "l" #'+macos/send-to-launchbar
;;           :desc "Send project to Launchbar" :n "L" #'+macos/send-project-to-launchbar))

;;       (:desc "quit" :prefix "q"
;;         :desc "Quit"                   :n "q" #'evil-save-and-quit
;;         :desc "Quit (forget session)"  :n "Q" #'+workspace/kill-session-and-quit)

;;       (:desc "remote" :prefix "r"
;;         :desc "Upload local"           :n "u" #'+upload/local
;;         :desc "Upload local (force)"   :n "U" (λ! (+upload/local t))
;;         :desc "Download remote"        :n "d" #'+upload/remote-download
;;         :desc "Diff local & remote"    :n "D" #'+upload/diff
;;         :desc "Browse remote files"    :n "." #'+upload/browse
;;         :desc "Detect remote changes"  :n ">" #'+upload/check-remote)

;;       ;; --- Plugin bindings ------------------------------
;;       ;; auto-yasnippet
;;       :i  [C-tab] #'aya-expand
;;       :nv [C-tab] #'aya-create

;;       ;; company-mode (vim-like omnicompletion)
;;       :i "C-SPC"  #'+company/complete
;;       (:prefix "C-x"
;;         :i "C-l"   #'+company/whole-lines
;;         :i "C-k"   #'+company/dict-or-keywords
;;         :i "C-f"   #'company-files
;;         :i "C-]"   #'company-etags
;;         :i "s"     #'company-ispell
;;         :i "C-s"   #'company-yasnippet
;;         :i "C-o"   #'company-capf
;;         :i "C-n"   #'company-dabbrev-code
;;         :i "C-p"   #'+company/dabbrev-code-previous)
;;       (:after company
;;         (:map company-active-map
;;           ;; Don't interfere with `evil-delete-backward-word' in insert mode
;;           "C-w"        nil
;;           "C-o"        #'company-search-kill-others
;;           "C-n"        #'company-select-next
;;           "C-p"        #'company-select-previous
;;           "C-h"        #'company-quickhelp-manual-begin
;;           "C-S-h"      #'company-show-doc-buffer
;;           "C-S-s"      #'company-search-candidates
;;           "C-s"        #'company-filter-candidates
;;           "C-SPC"      #'company-complete-common
;;           "C-h"        #'company-quickhelp-manual-begin
;;           [tab]        #'company-complete-common-or-cycle
;;           [backtab]    #'company-select-previous
;;           [escape]     (λ! (company-abort) (evil-normal-state 1)))
;;         ;; Automatically applies to `company-filter-map'
;;         (:map company-search-map
;;           "C-n"        #'company-search-repeat-forward
;;           "C-p"        #'company-search-repeat-backward
;;           "C-s"        (λ! (company-search-abort) (company-filter-candidates))
;;           [escape]     #'company-search-abort))

;;       ;; counsel
;;       (:after counsel
;;         (:map counsel-ag-map
;;           [backtab]  #'+ivy/wgrep-occur      ; search/replace on results
;;           "C-SPC"    #'ivy-call-and-recenter ; preview
;;           "M-RET"    (+ivy-do-action! #'+ivy-git-grep-other-window-action)))

;;       ;; git-timemachine
;;       (:after git-timemachine
;;         (:map git-timemachine-mode-map
;;           :n "C-p" #'git-timemachine-show-previous-revision
;;           :n "C-n" #'git-timemachine-show-next-revision
;;           :n "[["  #'git-timemachine-show-previous-revision
;;           :n "]]"  #'git-timemachine-show-next-revision
;;           :n "q"   #'git-timemachine-quit
;;           :n "gb"  #'git-timemachine-blame))

;;       ;; gist
;;       (:after gist
;;         :map gist-list-menu-mode-map
;;         :n "RET" #'+gist/open-current
;;         :n "b"   #'gist-browse-current-url
;;         :n "c"   #'gist-add-buffer
;;         :n "d"   #'gist-kill-current
;;         :n "f"   #'gist-fork
;;         :n "q"   #'quit-window
;;         :n "r"   #'gist-list-reload
;;         :n "s"   #'gist-star
;;         :n "S"   #'gist-unstar
;;         :n "y"   #'gist-print-current-url)

;;         (:after helm-files
;;           (:map helm-generic-files-map
;;             :e "ESC"     #'helm-keyboard-quit)
;;           (:map helm-find-files-map
;;             "C-w" #'helm-find-files-up-one-level
;;             "TAB" #'helm-execute-persistent-action))

;;         (:after helm-ag
;;           (:map helm-ag-map
;;             "<backtab>"  #'helm-ag-edit)))

;;       ;; hl-todo
;;       :m  "]t" #'hl-todo-next
;;       :m  "[t" #'hl-todo-previous

;;       ;; ivy
;;       (:after ivy
;;         :map ivy-minibuffer-map
;;         [escape] #'keyboard-escape-quit
;;         "C-SPC" #'ivy-call-and-recenter
;;         "M-v" #'yank
;;         "M-z" #'undo
;;         "C-r" #'evil-paste-from-register
;;         "C-k" #'ivy-previous-line
;;         "C-j" #'ivy-next-line
;;         "C-l" #'ivy-alt-done
;;         "C-w" #'ivy-backward-kill-word
;;         "C-u" #'ivy-kill-line
;;         "C-b" #'backward-word
;;         "C-f" #'forward-word)

;;       ;; neotree
;;       (:after neotree
;;         :map neotree-mode-map
;;         :n "g"         nil
;;         :n [tab]       #'neotree-quick-look
;;         :n "RET"       #'neotree-enter
;;         :n [backspace] #'evil-window-prev
;;         :n "c"         #'neotree-create-node
;;         :n "r"         #'neotree-rename-node
;;         :n "d"         #'neotree-delete-node
;;         :n "j"         #'neotree-next-line
;;         :n "k"         #'neotree-previous-line
;;         :n "n"         #'neotree-next-line
;;         :n "p"         #'neotree-previous-line
;;         :n "h"         #'+neotree/collapse-or-up
;;         :n "l"         #'+neotree/expand-or-open
;;         :n "J"         #'neotree-select-next-sibling-node
;;         :n "K"         #'neotree-select-previous-sibling-node
;;         :n "H"         #'neotree-select-up-node
;;         :n "L"         #'neotree-select-down-node
;;         :n "G"         #'evil-goto-line
;;         :n "gg"        #'evil-goto-first-line
;;         :n "v"         #'neotree-enter-vertical-split
;;         :n "s"         #'neotree-enter-horizontal-split
;;         :n "q"         #'neotree-hide
;;         :n "R"         #'neotree-refresh)

;;       ;; realgud
;;       (:after realgud
;;         :map realgud:shortkey-mode-map
;;         :n "j" #'evil-next-line
;;         :n "k" #'evil-previous-line
;;         :n "h" #'evil-backward-char
;;         :n "l" #'evil-forward-char
;;         :m "n" #'realgud:cmd-next
;;         :m "b" #'realgud:cmd-break
;;         :m "B" #'realgud:cmd-clear
;;         :n "c" #'realgud:cmd-continue)

;;       ;; rotate-text
;;       :n  "!"  #'rotate-text

;;       ;; smart-forward
;;       :nv "K"  #'smart-up
;;       :m  "g]" #'smart-forward
;;       :m  "g[" #'smart-backward

;;       ;; undo-tree -- undo/redo for visual regions
;;       :v "C-u" #'undo-tree-undo
;;       :v "C-r" #'undo-tree-redo

;;       ;; yasnippet
;;       (:after yasnippet
;;         (:map yas-keymap
;;           "C-e"           #'+snippets/goto-end-of-field
;;           "C-a"           #'+snippets/goto-start-of-field
;;           "<M-right>"     #'+snippets/goto-end-of-field
;;           "<M-left>"      #'+snippets/goto-start-of-field
;;           "<M-backspace>" #'+snippets/delete-to-start-of-field
;;           [escape]        #'evil-normal-state
;;           [backspace]     #'+snippets/delete-backward-char
;;           [delete]        #'+snippets/delete-forward-char-or-field)
;;         (:map yas-minor-mode-map
;;           :i "<tab>" yas-maybe-expand
;;           :v "<tab>" #'+snippets/expand-on-region))


;;       ;; --- Major mode bindings --------------------------
;;       (:after markdown-mode
;;         (:map markdown-mode-map
;;           ;; fix conflicts with private bindings
;;           "<backspace>" nil
;;           "<M-left>"    nil
;;           "<M-right>"   nil))


;;       ;; --- Custom evil text-objects ---------------------
;;       :textobj "a" #'evil-inner-arg                    #'evil-outer-arg
;;       :textobj "B" #'evil-textobj-anyblock-inner-block #'evil-textobj-anyblock-a-block
;;       :textobj "i" #'evil-indent-plus-i-indent         #'evil-indent-plus-a-indent
;;       :textobj "I" #'evil-indent-plus-i-indent-up      #'evil-indent-plus-a-indent-up
;;       :textobj "J" #'evil-indent-plus-i-indent-up-down #'evil-indent-plus-a-indent-up-down


;;       ;; --- Built-in plugins -----------------------------
;;       (:after comint
;;         ;; TAB auto-completion in term buffers
;;         :map comint-mode-map [tab] #'company-complete)

;;       (:after debug
;;         ;; For elisp debugging
;;         :map debugger-mode-map
;;         :n "RET" #'debug-help-follow
;;         :n "e"   #'debugger-eval-expression
;;         :n "n"   #'debugger-step-through
;;         :n "c"   #'debugger-continue)

;;       (:map help-mode-map
;;         :n "[["  #'help-go-back
;;         :n "]]"  #'help-go-forward
;;         :n "o"   #'ace-link-help
;;         :n "q"   #'quit-window
;;         :n "Q"   #'+ivy-quit-and-resume)

;;       (:after vc-annotate
;;         :map vc-annotate-mode-map
;;         :n "q"   #'kill-this-buffer
;;         :n "d"   #'vc-annotate-show-diff-revision-at-line
;;         :n "D"   #'vc-annotate-show-changeset-diff-revision-at-line
;;         :n "SPC" #'vc-annotate-show-log-revision-at-line
;;         :n "]]"  #'vc-annotate-next-revision
;;         :n "[["  #'vc-annotate-prev-revision
;;         :n "TAB" #'vc-annotate-toggle-annotation-visibility
;;         :n "RET" #'vc-annotate-find-revision-at-line))


;; ;;
;; ;; Keybinding fixes
;; ;;

;; ;; This section is dedicated to "fixing" certain keys so that they behave
;; ;; properly, more like vim, or how I like it.

(map! 
 (:map input-decode-map
   [S-iso-lefttab] [backtab]
   (:unless window-system "TAB" [tab])) ; Fix TAB in terminal

      ;; I want C-a and C-e to be a little smarter. C-a will jump to
      ;; indentation. Pressing it again will send you to the true bol. Same goes
      ;; for C-e, except it will ignore comments and trailing whitespace before
      ;; jumping to eol.
      :i "C-a" #'doom/backward-to-bol-or-indent
      :i "C-e" #'doom/forward-to-last-non-comment-or-eol
      :i "C-u" #'doom/backward-kill-to-bol-and-indent

      ;; textmate-esque newline insertion
      :i [M-return]     #'evil-open-below
      :i [S-M-return]   #'evil-open-above
      ;; textmate-esque deletion
      [M-backspace]     #'doom/backward-kill-to-bol-and-indent
      :i [backspace]    #'delete-backward-char
      :i [M-backspace]  #'doom/backward-kill-to-bol-and-indent
      ;; Emacsien motions for insert mode
      :i "C-b" #'backward-word
      :i "C-f" #'forward-word

      ;; Highjacks space/backspace to:
      ;;   a) balance spaces inside brackets/parentheses ( | ) -> (|)
      ;;   b) delete space-indented blocks intelligently
      ;;   c) do none of this when inside a string
      :i "SPC"                          #'doom/inflate-space-maybe
      :i [remap delete-backward-char]   #'doom/deflate-space-maybe
      :i [remap newline]                #'doom/newline-and-indent)


;; Buffer zooming
(defhydra hydra-text-zoom (:hint t :color red)
  "
      Text zoom: _j_:zoom in, _k_:zoom out, _0_:reset
"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("0" (text-scale-set 0) "reset"))

;; Window navigation
(defhydra hydra-windows (global-map "C-S-o" :hint none)
  "
      Delete: _da_ce  _dw_indow  _db_uffer  _df_rame
        Move: _s_wap, _b_alance
      Frames: _f_rame new  _df_ delete
        Misc: _a_ce  _u_ndo  _r_edo
    Windmove: _h_, _j_, _k_, _l_
      Adjust: _H_, _J_, _K_, _L_"
  ;; windmove
  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)
  ;; adjust
  ("H" hydra-move-splitter-left)
  ("J" hydra-move-splitter-down)
  ("K" hydra-move-splitter-up)
  ("L" hydra-move-splitter-right)
  ;; undo/redo
  ("u" winner-undo)
  ("r" winner-redo) ;;fixme, not working?
  ;; find file
  ("f" find-file :exit t)
  ;; move buffer
  ("z" scroll-up-line)
  ("a" scroll-down-line)
  ("i" idomenu)
  ;; arrange
  ("s" ace-swap-window)
  ("b" balance-windows)
  ;; delete
  ("da" ace-delete-window)
  ("do" delete-other-windows :exit t)
  ("dw" delete-window)
  ("db" kill-this-buffer)
  ("df" delete-frame :exit t))

;; Rectangle
(defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                                     :color pink
                                     :post (deactivate-mark))
  "
      ^_p_^     _d_elete    _s_tring
    _b_   _f_   _o_k        _y_ank
      ^_n_^     _w_ew-copy  _r_eset
    ^^^^        _e_xchange  _u_ndo
    ^^^^        ^ ^         _y_ank
    "
  ("b" rectangle-backward-char nil)
  ("f" rectangle-forward-char nil)
  ("p" rectangle-previous-line nil)
  ("n" rectangle-next-line nil)
  ("e" hydra-ex-point-mark nil)
  ("w" copy-rectangle-as-kill nil)
  ("d" delete-rectangle nil)
  ("r" (if (region-active-p)
           (deactivate-mark)
         (rectangle-mark-mode 1)) nil)
  ("y" yank-rectangle nil)
  ("u" undo nil)
  ("s" string-rectangle nil)
  ("k" kill-rectangle nil)
  ("o" nil nil))


;; Buffer menu
(defhydra hydra-buffer-menu (:color pink
                                    :hint nil)
  "
 ^Mark^             ^Unmark^           ^Actions^          ^Search
 ^^^^^^^^-----------------------------------------------------------------
 _m_: mark          _u_: unmark        _x_: execute       _R_: re-isearch
 _s_: save          _U_: unmark up     _b_: bury          _I_: isearch
 _d_: delete        ^ ^                _g_: refresh       _O_: multi-occur
 _D_: delete up     ^ ^                _T_: files only: % -28`Buffer-menu-files-only
 _~_: modified"
  ("m" Buffer-menu-mark)
  ("u" Buffer-menu-unmark)
  ("U" Buffer-menu-backup-unmark)
  ("d" Buffer-menu-delete)
  ("D" Buffer-menu-delete-backwards)
  ("s" Buffer-menu-save)
  ("~" Buffer-menu-not-modified)
  ("x" Buffer-menu-execute)
  ("b" Buffer-menu-bury)
  ("g" revert-buffer)
  ("T" Buffer-menu-toggle-files-only)
  ("O" Buffer-menu-multi-occur :color blue)
  ("I" Buffer-menu-isearch-buffers :color blue)
  ("R" Buffer-menu-isearch-buffers-regexp :color blue)
  ("c" nil "cancel")
  ("v" Buffer-menu-select "select" :color blue)
  ("o" Buffer-menu-other-window "other-window" :color blue)
  ("q" quit-window "quit" :color blue))

(map!
 ;; Core
 ;;
 ;; UI
 "C-x <backspace>" 'delete-window
 "C-x k" 'fast-kill-buffer
 ;; Editing
 [home]  'smart-beginning-of-line
 "C-a"   'smart-beginning-of-line
 "C-x ;" 'iedit-mode
 ;; Searching
 "M-s o" 'occur
 ;; Thing
 "M-c"   'duplicate-thing
 ;;
 ;; Ace-window
 ;;
 (:map override-global-map
   "C-o" 'ace-window
   "M-o" 'ace-window-all-frames)
 ;;
 ;; Smartparens
 ;;
 (:after smartparens
   (:map smartparens-mode-map
     ;; delete behavior
     "<backspace>"  'sp-backward-delete-char
     "<C-backspace>"  'backward-delete-char
     "<M-backspace>"  'sp-backward-kill-word
     "<C-M-backspace>"  'backward-kill-word
     ;; wrap/unwrap/rewrap
     "M-["  'sp-backward-unwrap-sexp
     "M-]"  'sp-unwrap-sexp
     "M-s-["  'sp-rewrap-sexp)
   (:prefix "C-j" :map smartparens-mode-map
     ;; wrapping
     "("   (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "("))
     "["   (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "["))
     "{"   (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "{"))
     "'"   (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "'"))
     "\""  (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "\""))
     "_"   (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "_"))
     "`"   (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "`"))
     ;; sexp direction
     "a"  'sp-beginning-of-sexp
     "e"  'sp-end-of-sexp
     "i"    'sp-up-sexp
     "j"  'sp-backward-down-sexp
     "l"    'sp-backward-up-sexp
     "f"  'sp-forward-sexp
     "b"  'sp-backward-sexp
     "n"  'sp-next-sexp
     "p"  'sp-previous-sexp
     ;; symbol direction
     "h"  'sp-forward-symbol
     "g"  'sp-backward-symbol
     ;; slurping
     "t"  'sp-forward-slurp-sexp
     "w"  'sp-forward-barf-sexp
     "r"   'sp-backward-slurp-sexp
     "q"   'sp-backward-barf-sexp
     ;; transposing
     "C-t"  'sp-transpose-sexp
     "M-t"  'sp-transpose-hybrid-sexp
     ;; killing/copying
     "k"  'sp-kill-sexp
     "h"    'sp-kill-hybrid-sexp
     "C-k"    'sp-backward-kill-sexp
     "C-w"  'sp-copy-sexp
     ;; deleting
     "d"  'sp-delete-word
     "C-d"  'sp-delete-sexp
     "<backspace>"  'sp-backward-delete-symbol
     "<C-backspace>"  'sp-backward-kill-sexp))
 ;;
 ;; Company
 ;;
 (:prefix "C-c"
   (:prefix "*"
     "l"   #'+company/whole-lines
     "k"   #'+company/dict-or-keywords
     "f"   #'company-files
     "C-]"   #'company-etags
     "s"     #'company-ispell
     "y"   #'company-yasnippet
     "c"   #'company-capf
     "a"   #'company-dabbrev-code
     "q"   #'+company/dabbrev-code-previous))
 (:after company
   (:map company-active-map
     ;; Don't interfere with `evil-delete-backward-word' in insert mode
     "C-w"        nil
     "C-o"        #'company-search-kill-others
     "C-n"        #'company-select-next
     "C-p"        #'company-select-previous
     "C-h"        #'company-quickhelp-manual-begin
     "C-S-h"      #'company-show-doc-buffer
     "C-S-s"      #'company-search-candidates
     "C-s"        #'company-filter-candidates
     "C-SPC"      #'company-complete-common
     "C-h"        #'company-quickhelp-manual-begin
     [tab]        #'company-complete-common-or-cycle
     [backtab]    #'company-select-previous
     [escape]     (λ! (company-abort) (evil-normal-state 1)))
   ;; Automatically applies to `company-filter-map'
   (:map company-search-map
     "C-n"        #'company-search-repeat-forward
     "C-p"        #'company-search-repeat-backward
     "C-s"        (λ! (company-search-abort) (company-filter-candidates))
     [escape]     #'company-search-abort))
 ;;
 ;; Ido
 ;;
 "C-x C-f" '+ido-find-file
 ;;
 ;; Helm
 ;;
 (:after helm-swoop
   (:map helm-swoop-map
     "M-i" 'helm-multi-swoop-all-from-helm-swoop)
    (:map isearch-mode-map
      "M-i" 'helm-swoop-from-isearch)) ; hand search over to helm-swoop
 (:after helm
   :prefix "C-x"
   "M-f" 'helm-locate
   ;; Help commands
   "7 w" 'helm-wikipedia-suggest
   "7 g" 'helm-google-suggest
   "7 s" 'helm-surfraw
   (:map helm-map
     :prefix ""
     "C-i" 'helm-execute-persistent-action ; make TAB works in terminal
     "C-z" 'helm-select-action
     "ESC"        nil
     "C-S-n"      #'helm-next-source
     "C-S-p"      #'helm-previous-source
     [left]       #'backward-char
     [right]      #'forward-char
     [escape]     #'helm-keyboard-quit
     [tab]        #'helm-execute-persistent-action
     "C-f"   'helm-apropos              ; great help function
     "C-r"   'helm-info-emacs
     "C-l"   'helm-locate-library)
   (:map helm-grep-mode-map
     "<return>" 'helm-grep-mode-jump-other-window
     "n"  'helm-grep-mode-jump-other-window-forward
     "p"  'helm-grep-mode-jump-other-window-backward)
   (:map minibuffer-local-map
     "C-M-p" 'helm-minibuffer-history)))
       
   
  ;; (define-key org-mode-map (kbd "C-a") 'smart-beginning-of-line)

;; (global-set-key (kbd "C-x SPC") 'hydra-rectangle/body)   
;; (define-key Buffer-menu-mode-map "." 'hydra-buffer-menu/body)                       

;; goes in core


;; (map!      ;; Text-scaling
;;       "M-+"       (λ! (text-scale-set 0))
;;       "M-="       #'text-scale-increase
;;       "M--"       #'text-scale-decrease

;;       ;; Simple window navigation/manipulation
;;       "C-`"       #'doom/popup-toggle
;;       "C-~"       #'doom/popup-raise
;;       "M-t"       #'+workspace/new
;;       "M-T"       #'+workspace/display
;;       "M-w"       #'delete-window
;;       "M-W"       #'+workspace/close-workspace-or-frame
;;       "M-n"       #'evil-buffer-new
;;       "M-N"       #'make-frame
;;       "M-1"       (λ! (+workspace/switch-to 0))
;;       "M-2"       (λ! (+workspace/switch-to 1))
;;       "M-3"       (λ! (+workspace/switch-to 2))
;;       "M-4"       (λ! (+workspace/switch-to 3))
;;       "M-5"       (λ! (+workspace/switch-to 4))
;;       "M-6"       (λ! (+workspace/switch-to 5))
;;       "M-7"       (λ! (+workspace/switch-to 6))
;;       "M-8"       (λ! (+workspace/switch-to 7))
;;       "M-9"       (λ! (+workspace/switch-to 8))
;;       "M-0"       #'+workspace/switch-to-last)
