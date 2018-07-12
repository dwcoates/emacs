;;; private/default/+bindings.el -*- lexical-binding: t; -*-

(unbind-key "C-z" global-map) ;; Remove suspend-frame binding (also bound to C-x C-z)
(unbind-key "C-x f" global-map) ;; Remove set-fill-column binding
(unbind-key "C-x s" global-map) ;; Remove save file (also bound to C-x C-s)
(unbind-key "C-x e" global-map) ;; Remove next-error binding
(unbind-key "C-x b" global-map) ;; Remove doom/previous-buffer
(unbind-key "C-x o" global-map) ;; Remove doom/kill-other-buffers
(unbind-key "C-x SPC" global-map) ;; Remove rectangle-mark-mode

(define-key input-decode-map [?\C-m] [C-m]) ;; Distinguish C-m from carriage return

(map!
 :map global-map
 :desc "Newline and indent" "RET" #'newline-and-indent
 :prefix "C-x"
 :desc "Execute kmacro" "SPC" #'kmacro-call-macro
 :desc "Previous buffer" "C-b" #'doom/previous-buffer
 :desc "Toggle window split" "|" #'+toggle-window-split
 (:desc "visualize"
   :desc "Whitespace mdoe"    "w"   #'whitespace-mode
   :desc "Blink cursor line"  "M-=" #'+doom/blink-cursor)
 (:desc "popups"
   :desc "Cycle through popup windows" "p" #'doom/other-popup)
 (:desc "file"
   :desc "Find file"                 "C-f" #'find-file
   :desc "Find file in project"      "M-f" #'projectile-find-file
   (:prefix "f"
     :desc "Sudo find file"            "F" #'doom/sudo-find-file
     :desc "Find file from here"       "?" #'counsel-file-jump
     :desc "Find file on line"         "<return>" #'dired-find-file-conservatively
     :desc "Find other file"           "a" #'projectile-find-other-file
     :desc "Open project editorconfig" "c" #'editorconfig-find-current-editorconfig
     :desc "Recent files"              "r" #'recentf-open-files
     :desc "Recent project files"      "R" #'projectile-recentf
     :desc "Yank filename"             "y" #'+add-file-name-to-clipboard))
 (:desc "buffer" :prefix "b"
   :desc "Switch buffer"           "B" #'switch-to-buffer
   :desc "Kill buffer"             "k" #'doom/kill-this-buffer
   :desc "Kill other buffers"      "o" #'doom/kill-other-buffers
   :desc "Pop scratch buffer"      "x" #'doom/open-scratch-buffer
   :desc "Bury buffer"             "z" #'bury-buffer
   :desc "Next buffer"             "]" #'doom/next-buffer
   :desc "Previous buffer"         "[" #'doom/previous-buffer
   :desc "Sudo edit this file"     "S" #'doom/sudo-this-file)
 (:desc "toggle" :prefix "t"
   :desc "Flyspell"               "s" #'flyspell-mode
   :desc "Flycheck"               "f" #'flycheck-mode
   :desc "Line numbers"           "l" #'doom/toggle-line-numbers
   :desc "Fullscreen"             "f" #'doom/toggle-fullscreen
   :desc "Indent guides"          "i" #'highlight-indentation-mode
   :desc "Indent guides (column)" "I" #'highlight-indentation-current-column-mode
   :desc "Impatient mode"         "h" #'+impatient-mode/toggle
   :desc "Big mode"               "b" #'doom-big-font-mode)
 (:desc "project" :prefix "P"
  :desc "Browse project"           "." #'+default/browse-project
  :desc "Find file in project"     "/" #'projectile-find-file
  :desc "Run cmd in project root"  "!" #'projectile-run-shell-command-in-root
  :desc "Switch project"           "p" #'projectile-switch-project
  :desc "Recent project files"     "r" #'projectile-recentf
  :desc "List project tasks"       "t" #'+ivy/tasks
  :desc "Pop term in project"      "o" #'+term/open-popup-in-project
  :desc "Invalidate cache"         "x" #'projectile-invalidate-cache)
 (:desc "snippets" :prefix "s"
  :desc "New snippet"             "n" #'yas-new-snippet
  :desc "Insert snippet"          "i" #'yas-insert-snippet
  :desc "Find snippet for mode"   "s" #'yas-visit-snippet-file
  :desc "Find snippet"            "S" #'+default/find-in-snippets)
 (:desc "code" :prefix "c"
  :desc "List errors"                "x" #'flycheck-list-errors
  :desc "Evaluate buffer/region"     "e" #'+eval/buffer
  :desc "Evaluate region"            "e" #'+eval/region
  :desc "Evaluate & replace region"  "E" #'+eval:replace-region
  :desc "Build tasks"                "b" #'+eval/build
  :desc "Jump to definition"         "d" #'+jump/definition
  :desc "Jump to references"         "D" #'+jump/references
  :desc "Open REPL"                  "r" #'+eval/open-repl)
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
    "RET" #'flycheck-error-list-goto-error))
 (:desc "open" :prefix "o"
   :desc "Default browser"        "b" #'browse-url-of-file
   :desc "Debugger"               "d" #'+debug/open
   :desc "REPL"                   "r" #'+eval/open-repl
   :desc "Neotree"                "n" #'neotree-toggle
   :desc "Terminal"               "t" #'+term/open-popup
   :desc "Terminal in project"    "T" #'+term/open-popup-in-project))

(map!
 :prefix "C-c"
 (:after yasnippet
   (:map yas-keymap
     "e"           #'+snippets/goto-end-of-field
     "a"           #'+snippets/goto-start-of-field
     "DEL" #'+snippets/delete-to-start-of-field
     [backspace]     #'+snippets/delete-backward-char
     [delete]        #'+snippets/delete-forward-char-or-field)
   (:map yas-minor-mode-map
     "<tab>" yas-maybe-expand
     "<tab>" #'+snippets/expand-on-region)))

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

(map!
 (:desc "git" :prefix "<C-m>"
   :desc "Git status"             "s" #'magit-status
   :desc "Git blame"              "b" #'magit-blame
   :desc "Git time machine"       "T" #'git-timemachine-toggle
   :desc "Git stage hunk"         "a" #'git-gutter:stage-hunk
   :desc "Git revert hunk"        "r" #'git-gutter:revert-hunk
   :desc "Git revert buffer"      "R" #'vc-revert
   :desc "List gists"             "g" #'+gist:list
   :desc "Next hunk"              "]" #'git-gutter:next-hunk
   :desc "Previous hunk"          "[" #'git-gutter:previous-hunk
   (:after gist    ;;TODO install gist
     :map gist-list-menu-mode-map
     :prefix "i"
     "RET" #'+gist/open-current
     "b"   #'gist-browse-current-url
     "c"   #'gist-add-buffer
     "d"   #'gist-kill-current
     "f"   #'gist-fork
     "q"   #'quit-window
     "r"   #'gist-list-reload
     "s"   #'gist-star
     "S"   #'gist-unstar
     "y"   #'gist-print-current-url)
   (:after git-timemachine
     :prefix "t"
     (:map git-timemachine-mode-map
       "C-p" #'git-timemachine-show-previous-revision
       "C-n" #'git-timemachine-show-next-revision
       "["  #'git-timemachine-show-previous-revision
       "]"  #'git-timemachine-show-next-revision
       "q"   #'git-timemachine-quit
       "b"  #'git-timemachine-blame))))

(map!
 (:after neotree
   :map neotree-mode-map
   "g"         nil
   [tab]       #'neotree-quick-look
   "RET"       #'neotree-enter
   [backspace] #'evil-window-prev
   "c"         #'neotree-create-node
   "r"         #'neotree-rename-node
   "d"         #'neotree-delete-node
   "j"         #'neotree-next-line
   "k"         #'neotree-previous-line
   "n"         #'neotree-next-line
   "p"         #'neotree-previous-line
   "h"         #'+neotree/collapse-or-up
   "l"         #'+neotree/expand-or-open
   "J"         #'neotree-select-next-sibling-node
   "K"         #'neotree-select-previous-sibling-node
   "H"         #'neotree-select-up-node
   "L"         #'neotree-select-down-node
   "G"         #'evil-goto-line
   "gg"        #'evil-goto-first-line
   "v"         #'neotree-enter-vertical-split
   "s"         #'neotree-enter-horizontal-split
   "q"         #'neotree-hide
   "R"         #'neotree-refresh))

(map!
 (:map input-decode-map
   [S-iso-lefttab] [backtab]
   (:unless window-system "TAB" [tab])) ; Fix TAB in terminal
      ;; I want C-a and C-e to be a little smarter. C-a will jump to
      ;; indentation. Pressing it again will send you to the true bol. Same goes
      ;; for C-e, except it will ignore comments and trailing whitespace before
      ;; jumping to eol.
      "C-a" #'doom/backward-to-bol-or-indent
      ; "C-e" #'doom/forward-to-last-non-comment-or-eol
      "C-u" #'doom/backward-kill-to-bol-and-indent

      ;; textmate-esque newline insertion
      [M-return]     #'evil-open-below
      [S-M-return]   #'evil-open-above
      ;; textmate-esque deletion
      [M-backspace]     #'doom/backward-kill-to-bol-and-indent
      [backspace]    #'delete-backward-char
      [M-backspace]  #'doom/backward-kill-to-bol-and-indent

      ;; Highjacks space/backspace to:
      ;;   a) balance spaces inside brackets/parentheses ( | ) -> (|)
      ;;   b) delete space-indented blocks intelligently
      ;;   c) do none of this when inside a string
      "SPC"                          #'doom/inflate-space-maybe
      [remap delete-backward-char]   #'doom/deflate-space-maybe
      [remap newline]                #'doom/newline-and-indent)

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
     "C-k"  'sp-kill-sexp
     "h"    'sp-kill-hybrid-sexp
     "k"    'sp-kill-symbol
     "C-w"  'sp-copy-sexp
     ;; deleting
     "C-d"  'sp-delete-sexp
     "d"  'sp-delete-symbol
     "<backspace>"  'sp-backward-delete-symbol
     "<C-backspace>"  'sp-backward-kill-sexp))
 ;;
 ;; Company
 ;;
 :desc "Completion at point" "C-;" #'company-complete
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
