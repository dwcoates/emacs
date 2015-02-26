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


;; use-package, courtesy of John Wiegley
(require 'use-package)


; cc-mode
(require 'setup-cc-mode)


;; ace-jump-mode
(use-package ace-jump-mode
  :bind ("M-h" . ace-jump-mode)
  :config
  (setq ace-jump-mode-submode-list
        '(ace-jump-char-mode
          ace-jump-word-mode
          ace-jump-line-mode)))

(defun char-mapping (key char)
  (bind-key key `(lambda () (interactive) (insert ,char))))

(char-mapping "A-G" "Γ")
(char-mapping "A-l" "λ x → ")
(char-mapping "A-:" " ∷ ")
(char-mapping "A-r" " → ")
(char-mapping "A-~" " ≅ ")
(char-mapping "A-=" " ≡ ")

(use-package ascii
  :commands (ascii-on ascii-toggle)
  :init
  (progn
    (defun ascii-toggle ()
      (interactive)
      (if ascii-display
          (ascii-off)
        (ascii-on)))
    (bind-key "C-c e A" 'ascii-toggle)))

(use-package tex-site
  :load-path "site-lisp/auctex/preview/"
  :defines (latex-help-cmd-alist latex-help-file)
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :config
  (progn
    (defun latex-help-get-cmd-alist ()  ;corrected version:
      "Scoop up the commands in the index of the latex info manual.
   The values are saved in `latex-help-cmd-alist' for speed."
      ;; mm, does it contain any cached entries
      (if (not (assoc "\\begin" latex-help-cmd-alist))
          (save-window-excursion
            (setq latex-help-cmd-alist nil)
            (Info-goto-node (concat latex-help-file "Command Index"))
            (goto-char (point-max))
            (while (re-search-backward "^\\* \\(.+\\): *\\(.+\\)\\." nil t)
              (let ((key (buffer-substring (match-beginning 1) (match-end 1)))
                    (value (buffer-substring (match-beginning 2)
                                             (match-end 2))))
                (add-to-list 'latex-help-cmd-alist (cons key value))))))
      latex-help-cmd-alist)

    (use-package latex-mode
      :defer t
      :config
      (progn
        (use-package preview)
        (use-package ac-math)

        (defun ac-latex-mode-setup ()
          (nconc ac-sources
                 '(ac-source-math-unicode ac-source-math-latex
                                          ac-source-latex-commands)))

        (add-to-list 'ac-modes 'latex-mode)
        (add-hook 'latex-mode-hook 'ac-latex-mode-setup)

        (info-lookup-add-help :mode 'latex-mode
                              :regexp ".*"
                              :parse-rule "\\\\?[a-zA-Z]+\\|\\\\[^a-zA-Z]"
                              :doc-spec '(("(latex2e)Concept Index" )
                                          ("(latex2e)Command Index")))))))



;(use-package auto-complete-config
;  :disabled t
;  :diminish auto-complete-mode
;  :init
;  (progn
;    (use-package pos-tip)
;    (ac-config-default))

;  :config
;  (progn
;    (ac-set-trigger-key "TAB")
;    (ac-set-trigger-key "<backtab>")
;    (setq ac-use-menu-map t)

;    (bind-key "A-M-?" 'ac-last-help)
;    (unbind-key "C-s" ac-completing-map)))


(use-package cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'"         . cmake-mode)))


(use-package compile
  :defer t
  :config
  (progn
    (defun cmake-project-filename ()
      (let ((filename (match-string-no-properties 1)))
        (save-match-data
          (with-temp-buffer
            (insert-file-contents-literally "cmake_install.cmake")
            (goto-char (point-min))
            (re-search-forward "Install script for directory: \\(.+\\)")
            (cons filename (match-string-no-properties 1))))))

    (push 'cmake compilation-error-regexp-alist)

    (push '(cmake "^CMake Error at \\(.+?\\):\\([0-9]+\\)"
                  (cmake-project-filename) 2 2 2)
          compilation-error-regexp-alist-alist)

    (push '(cmake "^\\(?:CMake Error at \\|  \\)\\(.+?\\):\\([0-9]+\\) ([A-Za-z_][A-Za-z0-9_]*)"
                  (cmake-project-filename) 2)
          compilation-error-regexp-alist-alist)

    (defun find-directory (dir name)
      (catch 'file
        (let ((files
               (delete
                nil
                (mapcar
                 (lambda (entry)
                   (and (not (string-match
                              "\\`\\." (file-name-nondirectory entry)))
                        (file-directory-p entry)
                        entry))
                 (directory-files dir t)))))
          (dolist (file files)
            (if (string= (file-name-nondirectory file) name)
                (throw 'file file)))
          (dolist (file files)
            (let ((result (find-directory file name)))
              (if result (throw 'file result)))))))

    (defun ghc-project-filename ()
      (let ((filename (match-string-no-properties 1)))
        (save-excursion
          (goto-char (point-min))
          (if (re-search-forward "^Building \\(.+?\\)-[0-9]" nil t)
              (cons filename (find-directory default-directory
                                             (match-string 1)))))))

    (add-hook 'compilation-finish-functions
              (lambda (buf why)
                (display-buffer buf)))))


(use-package ispell
  :bind (("C-c i c" . ispell-comments-and-strings)
         ("C-c i d" . ispell-change-dictionary)
         ("C-c i k" . ispell-kill-ispell)
         ("C-c i m" . ispell-message)
         ("C-c i r" . ispell-region)))


(use-package gtags
  :commands gtags-mode
  :diminish gtags-mode
  :config
  (progn
    (defun my-gtags-or-semantic-find-tag ()
      (interactive)
      (if (and (fboundp 'semantic-active-p)
               (funcall #'semantic-active-p))
          (call-interactively #'semantic-complete-jump)
        (call-interactively #'gtags-find-tag)))

    (bind-key "M-." 'my-gtags-or-semantic-find-tag gtags-mode-map)

    (bind-key "C-c t ." 'gtags-find-rtag)
    (bind-key "C-c t f" 'gtags-find-file)
    (bind-key "C-c t p" 'gtags-parse-file)
    (bind-key "C-c t g" 'gtags-find-with-grep)
    (bind-key "C-c t i" 'gtags-find-with-idutils)
    (bind-key "C-c t s" 'gtags-find-symbol)
    (bind-key "C-c t r" 'gtags-find-rtag)
    (bind-key "C-c t v" 'gtags-visit-rootdir)

    (bind-key "<mouse-2>" 'gtags-find-tag-from-here gtags-mode-map)

    (use-package helm-gtags
      :bind ("M-T" . helm-gtags-select)
      :config
      (bind-key "M-," 'helm-gtags-resume gtags-mode-map))))


;; sane-defaults
(require 'sane-defaults)


;; guide-key, displays possible key binding completions
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-x v" "C-x 8" "C-x +"))
(guide-key-mode 1)
(setq guide-key/recursive-key-sequence-flag t)
(setq guide-key/popup-window-position 'bottom)


;; setup
(eval-after-load 'org '(require 'setup-org))
(eval-after-load 'dired '(require 'setup-dired))
(eval-after-load 'magit '(require 'setup-magit))
(eval-after-load 'grep '(require 'setup-rgrep))
(eval-after-load 'shell '(require 'setup-shell))
(eval-after-load 'flycheck '(require 'setup-flycheck))
(require 'setup-yasnippet)
(require 'setup-helm)
(require 'setup-flymake)

; set up plain keybindings
(require 'key-bindings)
; configure keychord mode and key chords
(require 'setup-keychord)


;;(add-to-list 'load-path (expand-file-name "/home/dodge/.emacs.d/elpa/emacs-xkcd-1.0/emacs-xkcd.el"))
;;(require 'xkcd-mode)
;(global-set-key (kbd "C-x C-k r") 'xkcd-rand)
;(global-set-key (kbd "C-x C-k l") 'xkcd-latest)

;; conclude init by setting up specifics for the current user
;;(when (file-exists-p user-settings-dir)
;;  (mapc 'load (directory-files user-settings-dir nil "^[^#].*el$")))
