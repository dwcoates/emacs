;; -*- no-byte-compile: t; -*-
;;; core/packages.el

;; core-os.el
;; In case this config is shared across multiple computers (like mine is), let's
;; protect these from autoremoval.
(package! exec-path-from-shell :ignore (not IS-MAC))
(package! osx-clipboard        :ignore (not IS-MAC))

;; core-ui.el
(package! all-the-icons)
(package! fringe-helper)
(package! highlight-indentation)
(package! highlight-numbers)
(unless (boundp 'display-line-numbers)
  (package! nlinum)
  (package! nlinum-hl)
  (package! nlinum-relative))
(package! rainbow-delimiters)
(package! visual-fill-column)
(package! ws-butler)

;; core-popups.el
(package! shackle)

;; core-editor.el
(package! ace-link)
(package! ace-window)
(package! avy)
(package! command-log-mode)
(package! editorconfig)
(package! expand-region)
(package! iedit)
;; (package! help-fns+)      ; not currently available on melpa
(package! pcre2el)
(package! smart-forward)
(package! smartparens)
(package! undo-tree)
(package! wgrep)
(package! duplicate-thing)
(package! whole-line-or-region)

;; core-projects.el
(package! projectile)

;; core-keybinds.el
(package! which-key)
(package! hydra)