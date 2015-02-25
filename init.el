; Dodge Coates

;; key chords  **************** DOESNT WORK **********************

(setq default-directory "/home/dodge/")
(cd "./.emacs.d") ; Temporarily exists for now so I can configure emacs a bit faster. 

;; set path to dependencies
(setq settings-dir 
      (expand-file-name "settings" user-emacs-directory))

(setq site-lisp-dir 
      (expand-file-name "site-lisp" user-emacs-directory))

;; set up load path
(add-to-list 'load-path settings-dir)
(add-to-list 'load-path site-lisp-dir)

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; set up appearance early
(require 'appearance)

;; Add external projects to load path
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)


;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))


(require 'setup-package)
;; Install extensions if they're missing
(defun init--install-packages ()
  (packages-install
   '(magit
     paredit
     move-text
     gist
     htmlize
     visual-regexp
     markdown-mode
     fill-column-indicator
     flycheck
     flycheck-pos-tip
     flycheck-clojure
     flx
     flx-ido
     dired-details
     css-eldoc
     yasnippet
     smartparens
     ido-vertical-mode
     ido-at-point
     simple-httpd
     guide-key
     nodejs-repl
     restclient
     highlight-escape-sequences
     whitespace-cleanup-mode
     elisp-slime-nav
     git-commit-mode
     gitconfig-mode
     dockerfile-mode
     gitignore-mode
     clojure-mode
     groovy-mode
     prodigy
     cider
     )))


;;(condition-case nil
;;    (init--install-packages)
;;  (error
;;   (package-refresh-contents)
;;   (init--install-packages)))
(use-package cc-mode
  :mode (("\\.h\\(h?\\|xx\\|pp\\)\\'" . c++-mode)
         ("\\.m\\'"                   . c-mode)
         ("\\.mm\\'"                  . c++-mode))
  :init
  (progn
    (defun my-paste-as-check ()
      (interactive)
      (save-excursion
        (insert "/*\n")
        (let ((beg (point)) end)
          (yank)
          (setq end (point-marker))
          (goto-char beg)
          (while (< (point) end)
            (forward-char 2)
            (insert "CHECK: ")
            (forward-line 1)))
        (insert "*/\n")))

    ;; (defun my-c-indent-or-complete ()
    ;;   (interactive)
    ;;   (let ((class (syntax-class (syntax-after (1- (point))))))
    ;;     (if (or (bolp) (and (/= 2 class)
    ;;                         (/= 3 class)))
    ;;         (call-interactively 'indent-according-to-mode)
    ;;       (call-interactively 'auto-complete))))

    (defvar printf-index 0)

    (defun insert-counting-printf (arg)
      (interactive "P")
      (if arg
          (setq printf-index 0))
      (if t
          (insert (format "std::cerr << \"step %d..\" << std::endl;\n"
                          (setq printf-index (1+ printf-index))))
        (insert (format "printf(\"step %d..\\n\");\n"
                        (setq printf-index (1+ printf-index)))))
      (forward-line -1)
      (indent-according-to-mode)
      (forward-line))

    (defun my-c-mode-common-hook ()
      (abbrev-mode 1)
      (gtags-mode 1)
      (hs-minor-mode 1)
      (hide-ifdef-mode 1)
      (whitespace-mode 1)
      (which-function-mode 1)
      ;; (auto-complete-mode 1)
      (yas-minor-mode 1)
      (bug-reference-prog-mode 1)

      (diminish 'gtags-mode)
      (diminish 'hs-minor-mode)
      (diminish 'hide-ifdef-mode)

      (bind-key "C-c p" 'insert-counting-printf c-mode-base-map)

      ;; (setq ac-sources (list (if (and (fboundp 'semantic-active-p)
      ;;                                 (funcall #'semantic-active-p))
      ;;                            'ac-source-semantic
      ;;                          'ac-source-gtags)))
      ;; (bind-key "<A-tab>" 'ac-complete c-mode-base-map)

      ;;(doxymacs-mode 1)
      ;;(doxymacs-font-lock)

      (bind-key "<return>" 'newline-and-indent c-mode-base-map)

      ;; (set (make-local-variable 'yas-fallback-behavior)
      ;;      '(apply my-c-indent-or-complete . nil))
      ;; (bind-key "<tab>" 'yas-expand-from-trigger-key c-mode-base-map)

      (unbind-key "M-j" c-mode-base-map)
      (bind-key "C-c C-i" 'c-includes-current-file c-mode-base-map)
      (bind-key "C-c C-y" 'my-paste-as-check c-mode-base-map)

      (set (make-local-variable 'parens-require-spaces) nil)
      (setq indicate-empty-lines t)
      (setq fill-column 72)

      (bind-key "M-q" 'c-fill-paragraph c-mode-base-map)

      (let ((bufname (buffer-file-name)))
        (when bufname
          (cond
           ((string-match "/ledger/" bufname)
            (c-set-style "ledger"))
           ((string-match "/ansi/" bufname)
            (c-set-style "ti")
            (substitute-key-definition 'fill-paragraph 'ti-refill-comment
                                       c-mode-base-map global-map)
            (bind-key "M-q" 'ti-refill-comment c-mode-base-map))
           ((string-match "/edg/" bufname)
            (c-set-style "edg"))
           (t
            (c-set-style "clang")))))

      (font-lock-add-keywords 'c++-mode '(("\\<\\(assert\\|DEBUG\\)("
                                           1 font-lock-warning-face t))))

    (add-hook 'c-mode-common-hook 'my-c-mode-common-hook))

  :config
  (progn
    (setq c-syntactic-indentation nil)

    (bind-key "#" 'self-insert-command c-mode-base-map)
    (bind-key "{" 'self-insert-command c-mode-base-map)
    (bind-key "}" 'self-insert-command c-mode-base-map)
    (bind-key "/" 'self-insert-command c-mode-base-map)
    (bind-key "*" 'self-insert-command c-mode-base-map)
    (bind-key ";" 'self-insert-command c-mode-base-map)
    (bind-key "," 'self-insert-command c-mode-base-map)
    (bind-key ":" 'self-insert-command c-mode-base-map)
    (bind-key "(" 'self-insert-command c-mode-base-map)
    (bind-key ")" 'self-insert-command c-mode-base-map)
    (bind-key "<" 'self-insert-command c++-mode-map)
    (bind-key ">" 'self-insert-command c++-mode-map)

    (use-package cedet
      :disabled t
      :init
      (progn
        ;; Add further minor-modes to be enabled by semantic-mode.  See
        ;; doc-string of `semantic-default-submodes' for other things you can
        ;; use here.
        (dolist (submode '(global-semantic-idle-summary-mode
                           global-semantic-mru-bookmark-mode
                           global-semantic-idle-local-symbol-highlight-mode
                           global-semantic-show-unmatched-syntax-mode))
          (add-to-list 'semantic-default-submodes submode t))

        ;; Enable Semantic
        (semantic-mode 1)

        (when nil              ; jww (2012-06-20): this kills buffers
          ;; if you want to enable support for gnu global
          (use-package semanticdb-global)

          (semanticdb-enable-gnu-global-databases 'c-mode)
          (semanticdb-enable-gnu-global-databases 'c++-mode))))

    (add-to-list 'c-style-alist
                 '("ti"
                   (indent-tabs-mode . nil)
                   (c-basic-offset . 3)
                   (c-comment-only-line-offset . (0 . 0))
                   (c-hanging-braces-alist
                    . ((substatement-open before after)
                       (arglist-cont-nonempty)))
                   (c-offsets-alist
                    . ((statement-block-intro . +)
                       (knr-argdecl-intro . 5)
                       (substatement-open . 0)
                       (substatement-label . 0)
                       (label . 0)
                       (case-label . +)
                       (statement-case-open . 0)
                       (statement-cont . +)
                       (arglist-intro . c-lineup-arglist-intro-after-paren)
                       (arglist-close . c-lineup-arglist)
                       (inline-open . 0)
                       (brace-list-open . 0)
                       (topmost-intro-cont
                        . (first c-lineup-topmost-intro-cont
                                 c-lineup-gnu-DEFUN-intro-cont))))
                   (c-special-indent-hook . c-gnu-impose-minimum)
                   (c-block-comment-prefix . "")))

    (add-to-list 'c-style-alist
                 '("edg"
                   (indent-tabs-mode . nil)
                   (c-basic-offset . 2)
                   (c-comment-only-line-offset . (0 . 0))
                   (c-hanging-braces-alist
                    . ((substatement-open before after)
                       (arglist-cont-nonempty)))
                   (c-offsets-alist
                    . ((statement-block-intro . +)
                       (knr-argdecl-intro . 5)
                       (substatement-open . 0)
                       (substatement-label . 0)
                       (label . 0)
                       (case-label . +)
                       (statement-case-open . 0)
                       (statement-cont . +)
                       (arglist-intro . +)
                       (arglist-close . +)
                       (inline-open . 0)
                       (brace-list-open . 0)
                       (topmost-intro-cont
                        . (first c-lineup-topmost-intro-cont
                                 c-lineup-gnu-DEFUN-intro-cont))))
                   (c-special-indent-hook . c-gnu-impose-minimum)
                   (c-block-comment-prefix . "")))

    (add-to-list 'c-style-alist
                 '("ledger"
                   (indent-tabs-mode . nil)
                   (c-basic-offset . 2)
                   (c-comment-only-line-offset . (0 . 0))
                   (c-hanging-braces-alist
                    . ((substatement-open before after)
                       (arglist-cont-nonempty)))
                   (c-offsets-alist
                    . ((statement-block-intro . +)
                       (knr-argdecl-intro . 5)
                       (substatement-open . 0)
                       (substatement-label . 0)
                       (label . 0)
                       (case-label . 0)
                       (statement-case-open . 0)
                       (statement-cont . +)
                       (arglist-intro . +)
                       (arglist-close . +)
                       (inline-open . 0)
                       (brace-list-open . 0)
                       (topmost-intro-cont
                        . (first c-lineup-topmost-intro-cont
                                 c-lineup-gnu-DEFUN-intro-cont))))
                   (c-special-indent-hook . c-gnu-impose-minimum)
                   (c-block-comment-prefix . "")))

    (add-to-list 'c-style-alist
                 '("clang"
                   (indent-tabs-mode . nil)
                   (c-basic-offset . 2)
                   (c-comment-only-line-offset . (0 . 0))
                   (c-hanging-braces-alist
                    . ((substatement-open before after)
                       (arglist-cont-nonempty)))
                   (c-offsets-alist
                    . ((statement-block-intro . +)
                       (knr-argdecl-intro . 5)
                       (substatement-open . 0)
                       (substatement-label . 0)
                       (label . 0)
                       (case-label . 0)
                       (statement-case-open . 0)
                       (statement-cont . +)
                       (arglist-intro . +)
                       (arglist-close . +)
                       (inline-open . 0)
                       (brace-list-open . 0)
                       (topmost-intro-cont
                        . (first c-lineup-topmost-intro-cont
                                 c-lineup-gnu-DEFUN-intro-cont))))
                   (c-special-indent-hook . c-gnu-impose-minimum)
                   (c-block-comment-prefix . "")))

    (defun opencl ()
      (interactive)
      (find-file "~/src/ansi/opencl.c")
      (find-file-noselect "~/Contracts/TI/bugslayer/cl_0603/cl_0603.c")
      (find-file-noselect "~/Contracts/TI/bugslayer")
      (magit-status "~/src/ansi")
      (gud-gdb "gdb --fullname ~/Contracts/TI/bin/c60/acpia6x"))

    (defun ti-refill-comment ()
      (interactive)
      (let ((here (point)))
        (goto-char (line-beginning-position))
        (let ((begin (point)) end
              (marker ?-) (marker-re "\\(-----\\|\\*\\*\\*\\*\\*\\)")
              (leader-width 0))
          (unless (looking-at "[ \t]*/\\*[-* ]")
            (search-backward "/*")
            (goto-char (line-beginning-position)))
          (unless (looking-at "[ \t]*/\\*[-* ]")
            (error "Not in a comment"))
          (while (and (looking-at "\\([ \t]*\\)/\\* ")
                      (setq leader-width (length (match-string 1)))
                      (not (looking-at (concat "[ \t]*/\\*" marker-re))))
            (forward-line -1)
            (setq begin (point)))
          (when (looking-at (concat "[^\n]+?" marker-re "\\*/[ \t]*$"))
            (setq marker (if (string= (match-string 1) "-----") ?- ?*))
            (forward-line))
          (while (and (looking-at "[^\n]+?\\*/[ \t]*$")
                      (not (looking-at (concat "[^\n]+?" marker-re
                                               "\\*/[ \t]*$"))))
            (forward-line))
          (when (looking-at (concat "[^\n]+?" marker-re "\\*/[ \t]*$"))
            (forward-line))
          (setq end (point))
          (let ((comment (buffer-substring-no-properties begin end)))
            (with-temp-buffer
              (insert comment)
              (goto-char (point-min))
              (flush-lines (concat "^[ \t]*/\\*" marker-re "[-*]+\\*/[ \t]*$"))
              (goto-char (point-min))
              (while (re-search-forward "^[ \t]*/\\* ?" nil t)
                (goto-char (match-beginning 0))
                (delete-region (match-beginning 0) (match-end 0)))
              (goto-char (point-min))
              (while (re-search-forward "[ \t]*\\*/[ \t]*$" nil t)
                (goto-char (match-beginning 0))
                (delete-region (match-beginning 0) (match-end 0)))
              (goto-char (point-min)) (delete-trailing-whitespace)
              (goto-char (point-min)) (flush-lines "^$")
              (set-fill-column (- 80    ; width of the text
                                  6     ; width of "/*  */"
                                  leader-width))
              (goto-char (point-min)) (fill-paragraph nil)
              (goto-char (point-min))
              (while (not (eobp))
                (insert (make-string leader-width ? ) "/* ")
                (goto-char (line-end-position))
                (insert (make-string (- 80 3 (current-column)) ? ) " */")
                (forward-line))
              (goto-char (point-min))
              (insert (make-string leader-width ? )
                      "/*" (make-string (- 80 4 leader-width) marker) "*/\n")
              (goto-char (point-max))
              (insert (make-string leader-width ? )
                      "/*" (make-string (- 80 4 leader-width) marker) "*/\n")
              (setq comment (buffer-string)))
            (goto-char begin)
            (delete-region begin end)
            (insert comment)))
        (goto-char here)))))



(require 'sane-defaults)


;; guide-key, displays possible key binding completions
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-x v" "C-x 8" "C-x +"))
(guide-key-mode 1)
(setq guide-key/recursive-key-sequence-flag t)
(setq guide-key/popup-window-position 'bottom)

(eval-after-load 'org '(require 'setup-org))
(eval-after-load 'dired '(require 'setup-dired))
(eval-after-load 'magit '(require 'setup-magit))
(eval-after-load 'grep '(require 'setup-rgrep))
(eval-after-load 'shell '(require 'setup-shell))

(require 'setup-yasnippet)

(eval-after-load 'flycheck '(require 'setup-flycheck))

(require 'setup-helm)
;(require 'ggtags)
;(add-hook 'c-mode-common-hook
;          (lambda ()
;            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
;              (ggtags-mode 1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; COMPANY ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(company-mode 1)
;;(eval-after-load 'company
;;  '(add-to-list 'company-backends 'company-irony))
;; (optional) adds CC special commands to `company-begin-commands' in order to
;; trigger completion at interesting places, such as after scope operator
;;     std::|
;;(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; AUTO-COMPLETE ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; start auto-complete with emacs
(require 'auto-complete)
;; do default config for auto-completion
(require 'auto-complete-config)
(ac-config-default)
;; initialized auto-complete-c-headers with hook
(defun my:ac-c-header-init ()
  (require  'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers))
(add-hook 'c++-mode-hook 'my:ac-c-header-init)
(add-hook 'c-mode-hook 'my:ac-c-header-init)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; FLYMAKE ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flymake-google
(defun my:flymake-google-init ()
  (require 'flymake-google-cpplint)
  (custom-set-variables
   ;; cpplint executable can be found at:
   ;; http://google-styleguide.googlecode.com/svn/trunk/cpplint/cpplint.py
   '(flymake-google-cpplint-command "/usr/local/bin/cpplint.py"))
  (flymake-google-cpplint-load))
(add-hook 'c-mode-hook 'my:flymake-google-init)
(add-hook 'c++-mode-hook 'my:flymake-google-init)
;;start google-c-style with emacs
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; SEMANTIC ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; turn on Semantic
(semantic-mode 1)
;; adds semantic as a suggestion backend to auto-complete
(defun my:add-semantic-to-autocomplete ()
  (add-to-list 'ac-sources 'ac-source-semantic))
(add-hook 'c-mode-common-hook 'my:add-semantic-to-autocomplete)
;; turn on ede mode
(global-ede-mode 1)
;; create a project for 'demo' program
;; you can use the system-include-path for setting up the system header file locations.
(ede-cpp-root-project "my project" :file "~/demo/src/main.cpp"
		      :include-path '("/../inc"))
;; prompts semantic to parse open buffers in its spare time
(global-semantic-idle-scheduler-mode 1)
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; JDEE ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JDEE initialization            *** COMMENTED UNTIL JDEE SET UP AGAIN ***
;;(add-to-list 'load-path "~/.emacs.d/jdee-2.4.1/list")
;;(load "jde")

; set up plain keybindings
(require 'key-bindings)
; configure keychord mode and key chords
(require 'setup-keychord)

;;(add-to-list 'load-path (expand-file-name "/home/dodge/.emacs.d/elpa/emacs-xkcd-1.0/emacs-xkcd.el"))
;;(require 'xkcd-mode)
;;(global-set-key (kbd "C-x C-k r") 'xkcd-rand)
;;(global-set-key (kbd "C-x C-k l") 'xkcd-latest)

;; conclude init by setting up specifics for the current user
;;(when (file-exists-p user-settings-dir)
;;  (mapc 'load (directory-files user-settings-dir nil "^[^#].*el$")))
