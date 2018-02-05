;;; core-editor.el -*- lexical-binding: t; -*-

(defvar doom-large-file-size 1
  "Size (in MB) above which the user will be prompted to open the file literally
to avoid performance issues. Opening literally means that no major or minor
modes are active and the buffer is read-only.")

(defvar doom-large-file-modes-list
  '(archive-mode tar-mode jka-compr git-commit-mode image-mode
    doc-view-mode doc-view-mode-maybe ebrowse-tree-mode pdf-view-mode)
  "Major modes that `doom|check-large-file' will ignore.")

(setq-default
 vc-follow-symlinks t
 ;; Save clipboard contents into kill-ring before replacing them
 save-interprogram-paste-before-kill t
 ;; Bookmarks
 bookmark-default-file (concat doom-etc-dir "bookmarks")
 bookmark-save-flag t
 ;; Formatting
 delete-trailing-lines nil
 fill-column 80
 sentence-end-double-space nil
 word-wrap t
 ;; Scrolling
 hscroll-margin 1
 hscroll-step 1
 scroll-conservatively 1001
 scroll-margin 0
 scroll-preserve-screen-position t
 ;; Whitespace (see `editorconfig')
 indent-tabs-mode nil
 require-final-newline t
 tab-always-indent t
 tab-width 4
 tabify-regexp "^\t* [ \t]+" ; for :retab
 ;; Wrapping
 truncate-lines t
 truncate-partial-width-windows 50
 ;; whitespace-mode
 whitespace-line-column fill-column
 whitespace-style
 '(face indentation tabs tab-mark spaces space-mark newline newline-mark
   trailing lines-tail)
 whitespace-display-mappings
 '((tab-mark ?\t [?› ?\t])
   (newline-mark ?\n [?¬ ?\n])
   (space-mark ?\  [?·] [?.])))

;; ediff
(setq ediff-diff-options "-w"
      ediff-split-window-function #'split-window-horizontally
      ediff-window-setup-function #'ediff-setup-windows-plain)

(defun doom|dont-kill-scratch-buffer ()
  "Don't kill the scratch buffer."
  (or (not (string= (buffer-name) "*scratch*"))
      (ignore (bury-buffer))))
(add-hook 'kill-buffer-query-functions #'doom|dont-kill-scratch-buffer)

;; temporary windows often have q bound to `quit-window', which only buries the
;; contained buffer. I rarely don't want that buffer killed, so...
(defun doom*quit-window (orig-fn &optional kill window)
  (funcall orig-fn (not kill) window))
(advice-add #'quit-window :around #'doom*quit-window)

(defun doom|check-large-file ()
  "Check if the buffer's file is large (see `doom-large-file-size'). If so, ask
for confirmation to open it literally (read-only, disabled undo and in
fundamental-mode) for performance sake."
  (let* ((filename (buffer-file-name))
         (size (nth 7 (file-attributes filename))))
    (when (and (not (memq major-mode doom-large-file-modes-list))
               size (> size (* 1024 1024 doom-large-file-size))
               (y-or-n-p
                (format (concat "%s is a large file, open literally to "
                                "avoid performance issues?")
                        (file-relative-name filename))))
      (setq buffer-read-only t)
      (buffer-disable-undo)
      (fundamental-mode))))
(add-hook 'find-file-hook #'doom|check-large-file)

(push '("/LICENSE$" . text-mode) auto-mode-alist)


;;
;; Built-in plugins
;;

;; revert buffers for changed files
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)

;; enabled by default in Emacs 25+. No thanks.
(electric-indent-mode -1)

;; savehist / saveplace
(setq savehist-file (concat doom-cache-dir "savehist")
      savehist-save-minibuffer-history t
      savehist-autosave-interval nil ; save on kill only
      savehist-additional-variables '(kill-ring search-ring regexp-search-ring)
      save-place-file (concat doom-cache-dir "saveplace"))
(add-hook! 'doom-init-hook #'(savehist-mode save-place-mode))

;; Keep track of recently opened files
(def-package! recentf
  :hook (doom-init . recentf-mode)
  :config
  (setq recentf-save-file (concat doom-cache-dir "recentf")
        recentf-max-menu-items 0
        recentf-max-saved-items 300
        recentf-filename-handlers '(file-truename)
        recentf-exclude
        (list "^/tmp/" "^/ssh:" "\\.?ido\\.last$" "\\.revive$" "/TAGS$"
              "^/var/folders/.+$"
              ;; ignore private DOOM temp files (but not all of them)
              (concat "^" (file-truename doom-local-dir)))))


;;
;; Core Plugins
;;

;; Handles whitespace (tabs/spaces) settings externally. This way projects can
;; specify their own formatting rules.
(def-package! editorconfig
  :config
  (add-hook 'doom-init-hook #'editorconfig-mode)

  ;; editorconfig cannot procure the correct settings for extension-less files.
  ;; Executable scripts with a shebang line, for example. So why not use Emacs'
  ;; major mode to drop editorconfig a hint? This is accomplished by temporarily
  ;; appending an extension to `buffer-file-name' when we talk to editorconfig.
  (defvar doom-editorconfig-mode-alist
    '((sh-mode     . "sh")
      (python-mode . "py")
      (ruby-mode   . "rb")
      (perl-mode   . "pl")
      (php-mode    . "php"))
    "An alist mapping major modes to extensions. Used by
`doom*editorconfig-smart-detection' to give editorconfig filetype hints.")

  (defun doom*editorconfig-smart-detection (orig-fn &rest args)
    "Retrieve the properties for the current file. If it doesn't have an
extension, try to guess one."
    (let ((buffer-file-name
           (if (file-name-extension buffer-file-name)
               buffer-file-name
             (format "%s%s" buffer-file-name
                     (let ((ext (cdr (assq major-mode doom-editorconfig-mode-alist))))
                       (or (and ext (concat "." ext))
                           ""))))))
      (apply orig-fn args)))
  (advice-add #'editorconfig-call-editorconfig-exec :around #'doom*editorconfig-smart-detection)

  ;; Editorconfig makes indentation too rigid in Lisp modes, so tell
  ;; editorconfig to ignore indentation. I prefer dynamic indentation support
  ;; built into Emacs.
  (dolist (mode '(emacs-lisp-mode lisp-mode))
    (setq editorconfig-indentation-alist
      (assq-delete-all mode editorconfig-indentation-alist)))

  (defvar whitespace-style)
  (defun doom|editorconfig-whitespace-mode-maybe (&rest _)
    "Show whitespace-mode when file uses TABS (ew)."
    (when indent-tabs-mode
      (let ((whitespace-style '(face tabs tab-mark trailing-lines tail)))
        (whitespace-mode +1))))
  (add-hook 'editorconfig-custom-hooks #'doom|editorconfig-whitespace-mode-maybe))

(def-package! editorconfig-conf-mode
  :mode "\\.?editorconfig$")

;; Auto-close delimiters and blocks as you type
(def-package! smartparens
  :hook (doom-init . smartparens-global-mode)
  :config
  (bind-keys*
   :map smartparens-mode-map
   ;; delete behavior
   ("<backspace>" . sp-backward-delete-char)
   ("<C-backspace>" . backward-delete-char)
   ("<M-backspace>" . sp-backward-kill-word)
   ("<C-M-backspace>" . backward-kill-word)
   ;; wrap/unwrap/rewrap
   ("M-[" . sp-backward-unwrap-sexp)
   ("M-]" . sp-unwrap-sexp)
   ("M-s-[" . sp-rewrap-sexp))

  (bind-keys*
   :prefix "C-j"
   :prefix-map smartparens-mode-map
   ;; wrapping
   ("("  . (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "(")))
   ("["  . (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "[")))
   ("{"  . (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "{")))
   ("'"  . (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "'")))
   ("\"" . (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "\"")))
   ("_"  . (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "_")))
   ("`"  . (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "`")))
   ;; sexp direction
   ("a" . sp-beginning-of-sexp)
   ("e" . sp-end-of-sexp)
   ("i"   . sp-up-sexp)
   ("j" . sp-backward-down-sexp)
   ("l"   . sp-backward-up-sexp)
   ("f" . sp-forward-sexp)
   ("b" . sp-backward-sexp)
   ("n" . sp-next-sexp)
   ("p" . sp-previous-sexp)
   ;; symbol direction
   ("h" . sp-forward-symbol)
   ("g" . sp-backward-symbol)
   ;; slurping
   ("t" . sp-forward-slurp-sexp)
   ("w" . sp-forward-barf-sexp)
   ("r"  . sp-backward-slurp-sexp)
   ("q"  . sp-backward-barf-sexp)
   ;; transposing
   ("C-t" . sp-transpose-sexp)
   ("M-t" . sp-transpose-hybrid-sexp)
   ;; killing/copying
   ("k" . sp-kill-sexp)
   ("h"   . sp-kill-hybrid-sexp)
   ("C-k"   . sp-backward-kill-sexp)
   ("C-w" . sp-copy-sexp)
   ;; deleting
   ("d" . sp-delete-word)
   ("C-d" . sp-delete-sexp)
   ("<backspace>" . sp-backward-delete-symbol)
   ("<C-backspace>" . sp-backward-kill-sexp))

  (defun sp-delete-sexp (arg)
     "Deletes sexp at point. Does not save to kill ring."
     (interactive "p")
     (sp-kill-sexp arg)
     (pop kill-ring))

  (defun sp-backward-delete-sexp (arg)
     "Deletes sexp at point. Does not save to kill ring."
     (interactive "p")
     (sp-backward-kill-sexp arg)
     (pop kill-ring))

  (setq sp-autowrap-region t ; let evil-surround handle this
        sp-highlight-pair-overlay nil
        sp-cancel-autoskip-on-backward-movement nil
        sp-show-pair-delay 0
        sp-max-pair-length 3)

  ;; disable smartparens in evil-mode's replace state (they conflict)
  (add-hook 'evil-replace-state-entry-hook #'turn-off-smartparens-mode)
  (add-hook 'evil-replace-state-exit-hook  #'turn-on-smartparens-mode)

  (sp-local-pair '(xml-mode nxml-mode php-mode) "<!--" "-->"
                 :post-handlers '(("| " "SPC")))
  :diminish 'smartparens-mode)

;; Branching undo
(def-package! undo-tree
  :config
  (add-hook 'doom-init-hook #'global-undo-tree-mode)
  ;; persistent undo history is known to cause undo history corruption, which
  ;; can be very destructive! So disable it!
  (setq undo-tree-auto-save-history nil
        undo-tree-history-directory-alist
        (list (cons "." (concat doom-cache-dir "undo-tree-hist/")))))


;;
;; Autoloaded Plugins
;;

(def-package! ace-link
  :commands (ace-link-help ace-link-org))

(def-package! avy
  :commands (avy-goto-char-2 avy-goto-line)
  :bind
  (("C-l"   . avy-goto-line)
   ("C-S-w"   . avy-kill-region)
   ("C-S-l" . avy-copy-line)
   ("<C-m>" . avy-goto-char-timer)
   ("C-."   . avy-goto-char)
   ("C-s" . avy-goto-char-in-paragraph)
   ("C-r" . avy-goto-char-in-line))
  :bind*
  (("C-M-s"   . avy-goto-word-1))
  :config
  (setq avy-all-windows nil
        avy-background  t
        avy-keys '(97  115 100 102 106 108 104 113 119
                   101 114 116 121 117 111 122 120 118
                   98  109 44  46)
        avy-dispatch-alist '((?c . avy-action-copy)
                             (?k . avy-action-kill-move)
                             (?K . avy-action-kill-stay)
                             (?m . avy-action-mark)
                             (?\; . avy-action-execute-code)
                             (?n . avy-narrow-region)
                             (?p . avy-action-copy-and-yank))
        avy-timeout-seconds .2)

  (defun avy-action-copy (pt)
    "Copy sexp starting on PT."
    (save-excursion
      (let (str)
        (goto-char pt)
        (avy-forward-item)
        (setq str ( (buffer-substring pt (point))))
        (when (fboundp 's-trim)
          (setq str (s-trim str)))
        (kill-new str)
        (message "Copied: %s" str)))
    (let ((dat (ring-ref avy-ring 0)))
      (select-frame-set-input-focus
       (window-frame (cdr dat)))
      (select-window (cdr dat))
      (goto-char (car dat))))

  (defun avy-action-copy-and-yank (pt)
    "Copy and yank sexp starting on PT."
    (avy-action-copy pt)
    (yank))

  (defun avy-action-execute-code (pt)
    (let* ((string (progn (avy-action-copy pt)
                          (substring-no-properties (pop kill-ring)))))
      (if (functionp 's-trim)
          (setq string (s-trim string))
        (warn "No s-trim function found, avy-action-execute-code may work poorly with Python code."))
      (python-send-string string)))

  (defun avy-narrow-region (pt)
    (narrow-to-region
     (save-excursion (beginning-of-line) (point))
     (save-excursion (avy-action-goto pt)
                     (end-of-line)
                     (point))))

  (defun avy-goto-char-in-paragraph (char)
  "Jump to the currently visible CHAR in current paragraph."
  (interactive (list (read-char "char: " t)))
  (let (beg end)
    (save-excursion
      (forward-paragraph)
      (setq end (point))
      (backward-paragraph)
      (setq beg (point)))
    (avy-with avy-goto-char
              (avy--generic-jump
               (regexp-quote (string char))
               nil
               avy-style
               beg
               end)))))

(def-package! command-log-mode
  :commands (command-log-mode global-command-log-mode)
  :config
  (set! :popup "*command-log*" :size 40 :align 'right :noselect t)
  (setq command-log-mode-auto-show t
        command-log-mode-open-log-turns-on-mode t))

(def-package! expand-region
  :commands (er/expand-region er/contract-region er/mark-symbol er/mark-word))

;; Currently not available?
;; (def-package! help-fns+ ; Improved help commands
;;   :commands (describe-buffer describe-command describe-file
;;              describe-keymap describe-option describe-option-of-type))

(def-package! pcre2el
  :commands rxt-quote-pcre)

(def-package! smart-forward
  :commands (smart-up smart-down smart-backward smart-forward))

(def-package! wgrep
  :commands (wgrep-setup wgrep-change-to-wgrep-mode)
  :config (setq wgrep-auto-save-buffer t))

(provide 'core-editor)
;;; core-editor.el ends here