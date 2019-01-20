(require 'core (concat user-emacs-directory "doom-core/core"))

(doom! :feature
      ;debugger          ; FIXME stepping through code, to help you add bugs
       eval              ; run code, run (also, repls)
      ;evil              ; come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       jump              ; helping you get around
       snippets          ; my elves. They type so I don't have to
       spellcheck        ; tasing you for misspelling mispelling
       syntax-checker    ; tasing you for every semicolon you forget
       version-control   ; remember, remember that commit in November
       workspaces        ; tab emulation, persistence & separate workspaces

       :core-ui
       company           ; the ultimate code completion backend
       helm              ; a search engine for love and life
       ivy
       ido

       :gui
       theme
       hl-todo
       nav-flash
       theme
       unicode
       modeline
       vi-tilde-fringe
       window-select

       :tools
       dired             ; making dired pretty [functional]
       electric-indent   ; smarter, keyword-based electric-indent
       eshell            ; a consistent, cross-platform shell (WIP)
       gist              ; interacting with github gists
       imenu             ; an imenu sidebar and searchable code index
       impatient-mode    ; show off code over HTTP
                                        ;macos             ; MacOS-specific commands
       make              ; run make tasks from Emacs
       neotree           ; a project drawer, like NERDTree for vim
       password-store    ; password manager for nerds
       rotate-text       ; cycle region at point between text candidates
       term              ; terminals in Emacs
       tmux              ; an API for interacting with tmux
       upload            ; map local to remote projects via ssh/ftp

       :lang
       cc                ; C/C++/Obj-C madness
       data              ; config/data formats
       lisp              ; drown in parentheses
       javascript        ; all(hope(abandon(ye(who(enter(here))))))
       latex             ; writing papers in Emacs has never been so fun
       markdown          ; writing docs for people to ignore
       (org              ; organize your plain life in plain text
        +attach          ; custom attachment system
        +babel           ; running code in org
        +capture         ; org-capture in and outside of Emacs
        +export          ; centralized export system + more backends
        +present         ; Emacs for presentations
        ;+atchka-org-theme  ; theme for org-mode
        +atchka-org-blocks ; themify code blocks
        ;; TODO +publish
        )
       perl              ; write code no one else can comprehend
       php               ; make php less awful to work with
       python            ; beautiful is better than ugly
       ruby              ; 1.step do {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       scala             ; java, but good
       sh                ; she sells (ba|z)sh shells on the C xor
       web               ; the tubes

       ;; Applications are complex and opinionated modules that transform Emacs
       ;; toward a specific purpose. They may have additional dependencies and
       ;; should be loaded late.
       :app
                                        ;email             ; emacs as an email client
                                        ;irc               ; how neckbeards socialize
                                        ;rss               ; emacs as an RSS reader
                                        ;twitter           ; twitter client https://twitter.com/vnought
                                        ;write             ; emacs as a word processor (latex + org + markdown)

       ;; Private modules are where you place your personal configuration files.
       ;; By default, they are not tracked. There is one module included here,
       ;; the defaults module. It contains a Spacemacs-inspired keybinding
       ;; scheme and additional ex commands for evil-mode. Use it as a reference
       ;; for your own.
       :personal basic)
(put 'narrow-to-region 'disabled nil)
