;; Keep track of loading time
(defconst emacs-start-time (current-time))

;; Directory for personal stuff. Personal org documents, emacs history, etc.
(defconst personal-dir (file-name-as-directory "~/personal"))
(unless (file-exists-p personal-dir)
  (message "%s: %s" "Creating personal directory for first time" personal-dir)
  (make-directory personal-dir))

;; Directory for emacs saves
(defconst saveplace-dir
  (concat (file-name-as-directory (concat personal-dir ".emacs-saves")))
  "Where all the saves go (stuff like cursor position, autosaves, etc).")

;; Directory for lisp source code
(defconst lisp-dir
  (file-name-as-directory (concat user-emacs-directory "lisp"))
  "Where Lisp and packages not added by package.el belong.")
(add-to-list 'load-path lisp-dir)

;; load system-specific settings best loaded first
(let ((pre (concat personal-dir ".exclusive/pre.el")))
  (if (file-exists-p pre)
      (load pre)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; SET UP PACKAGE MANAGEMENT AND USE-PACKAGE ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq package-user-dir (concat user-emacs-directory "dependencies"))

(package-initialize)

;; Load Emacs' package manager
(require 'package)

;; Add various emacs package repositories to the pool. This is where we
;; look for packages.
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("elpy" . "https://jorgenschaefer.github.io/packages/"))

;; Load all of the repositories that we've added.
(when (not package-archive-contents)
  (package-refresh-contents))

;;
;; USE-PACKAGE
;;
;; use-package is a core package in modern emacs (at least if you have
;; a large config that you want to be managable as a plug-and-play
;; release). The following installs it if not already installed. It's
;; done here so it can be used to insall org-mode, which is used for
;; organizing the rest of this emacs config. The last several lines of
;; this file call an org function that handles that.
;;
;;
(unless (package-installed-p 'use-package)
  (package-install 'use-package))


;; Configure use-package
(setq use-package-verbose t)
(require 'use-package)
(use-package auto-compile
  :ensure t
  :config (auto-compile-on-load-mode))
(setq load-prefer-newer t)
(setq use-package-always-ensure t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; PRE-LOAD APPEARANCE SETTINGS ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; These are here, and not in config.org, because they should be present
; during the loading and saving of config.el (generated by babel).
; If any code in config.org fails, the basic appearance of emacs will
; be preserved. I got tired of seeing ugly emacs on failure.

;; Load wilson theme:
;(load "~/.emacs.d/wilson-theme.el")

;;
;; Some basic editing and appearance defaults
;; Things like tab width, word-wrapping, cursor blinking, etc.
;;
(setq-default
 ;; Formatting
 delete-trailing-lines              nil
 fill-column                        78
 apropos-do-all                     t           ; make `apropos' more useful
 echo-keystrokes                    0.02        ; show me what I type
 enable-recursive-minibuffers       nil         ; no minibufferception
 bookmark-save-flag                 t
 history-length                     1000
 ;; Spaces, not tabs
 indent-tabs-mode                   nil
 require-final-newline              t
 tab-always-indent                  t
 tab-width                          4
 ;; Wrapping
 truncate-lines                     t
 truncate-partial-width-windows     50
 visual-fill-column-center-text     nil
 word-wrap                          t
 ;; Scrolling
 hscroll-margin                     1
 hscroll-step                       1
 scroll-conservatively              1001
 scroll-margin                      0
 scroll-preserve-screen-position    t
 scroll-bar-mode                    nil
 ;; Regions
 shift-select-mode                  t
 ;; Whitespace
 tabify-regexp                      "^\t* [ \t]+"
 whitespace-line-column             fill-column
 whitespace-style                   '(face tabs tab-mark
                                           trailing indentation
                                           lines-tail)
 whitespace-display-mappings        '((tab-mark ?\t [?› ?\t])
                                      (newline-mark 10 [36 10]))
 split-width-threshold 1
 ;;          INTERFACE
 ;v; no splash message
 inhibit-startup-message            t
 ;; cursor
 blink-cursor-mode                  nil
 cursor-type                        'box
 ;; highlight current line
 global-hl-line-mode                nil
 display-time-mode                  t
 ;; current display column number in modeline
 column-number-mode                 t
 )

;; display time in modeline
(display-time-mode 1)

(set-scroll-bar-mode nil)

(setq-default cursor-in-non-selected-windows nil)
;; no toolbar
(tool-bar-mode -1)

;; set transparency
(set-frame-parameter (selected-frame) 'alpha '(100 92))
(add-to-list 'default-frame-alist '(alpha 100 92))

;; don't include scroll bars in new frames
(defun my/disable-scroll-bars (frame)
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions 'my/disable-scroll-bars)

;; key binding for turning the menu bar on and off
(defun toggle-menu-bar ()
  (interactive)
  (if menu-bar-mode
      (menu-bar-mode 0)
    (menu-bar-mode 1)))
(global-set-key (kbd "C-x m") 'toggle-menu-bar)


;; Setup default window size:
(add-to-list 'default-frame-alist '(height . 40))
(add-to-list 'default-frame-alist '(width . 160))

;; No scroll bar
(set-frame-parameter (selected-frame) 'scroll-bar-mode nil)
(add-to-list 'default-frame-alist '(scroll-bar-mode nil))

;; Use 'y' instead of "yes" and 'n' instead of "no" at prompt.
(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "C-c ; t") 'toggle-truncate-lines)

;; Highlight current line:
(require 'hl-line)
(set-face-background 'hl-line "#3b3b3b")
(set-face-foreground 'highlight nil)

;; Dont ask me if I want to use these features before I do:
(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'upcase-region 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; SAVES ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Save point across sessions
(require 'saveplace)
(setq-default
 save-place-file (concat saveplace-dir "places_")
 save-place-mode t)
(when (>= emacs-major-version 25)
  (save-place-mode +1))

;; Save history across sessions
(require 'savehist)
(setq-default savehist-file (expand-file-name "hist_" saveplace-dir)
      savehist-additional-variables
      '(kill-ring search-ring regexp-search-ring))
(savehist-mode 1)

;; Keep track of recently opened files
(require 'recentf)
(setq-default recentf-save-file (expand-file-name "recent_" saveplace-dir)
              recentf-exclude '("/tmp/" "/ssh:" "\\.?ido\\.last$" "\\.revive$" "/TAGS$"
                                "emacs\\.d/private/cache/.+" "emacs\\.d/workgroups/.+$"
                                "wg-default" "/company-statistics-cache.el$"
                                "^/var/folders/.+$" "^/tmp/.+")
              recentf-max-menu-items 0
              recentf-max-saved-items 250
              recentf-auto-cleanup 600
              recentf-filename-handlers '(abbreviate-file-name))
(recentf-mode 1)

;; File Autosaves
(setq-default auto-save-list-file-prefix (concat saveplace-dir ".saves-"))

(setq-default bookmark-default-file (concat saveplace-dir "bookmarks"))

(setq-default ede-project-placeholder-cache-file
              (concat saveplace-dir "ede-projects.el"))

;; window config undo/redo winner is a minor-mode for undoing and
;; redoing window configuration changes.
(setq-default winner-dont-bind-my-keys t)
(use-package winner)
(winner-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;; GARBAGE COLLECTION ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; set garbage collection threshold very high for the duration of
;; initialization
(setq gc-cons-threshold 50000000)

(add-hook 'emacs-startup-hook 'my/set-gc-threshold)
(defun my/set-gc-threshold ()
  "Reset `gc-cons-threshold' to its default value."
  (setq gc-cons-threshold 800000))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;; THEMES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set default face (used for background, normal text, etc)
(set-face-attribute 'default nil
                    :background "#222222"
                    :foreground "light gray"
                    :height 110
                    :foundry "unknown"
                    :family "Ubuntu Mono")

;; Useful function for loading icons
(defun load-directory (dir)
  (let ((load-it (lambda (f)
                   (load-file (concat (file-name-as-directory dir) f)))
                 ))
    (mapc load-it (directory-files dir nil "\\.el$"))))

;; Set up modeline
(use-package smart-mode-line
  :init
  (setq sml/name-width 16)
  (setq sml/no-confirm-load-theme t)
  (add-hook 'after-init-hook 'smart-mode-line-enable))

;; load up the main theme
(add-to-list 'load-path (concat user-emacs-directory "atchka"))
(use-package doom-themes
  :init
  (require 'atchka-theme)
  (add-hook 'after-init-hook (lambda ()
                               (load-theme 'atchka t)))
  :config
  ;; brighten minibuffers (does this even work??)
  (add-hook 'minibuffer-setup-hook 'doom-brighten-minibuffer))

(sml/setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; LOAD CONFIG.ORG AND SAVE ITS ELISP TO CONFIG.EL ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This use-package call will ensure that the correct, up-to-date
;; version of org installed, along with all its subpackages (that's
;; the "contrib" bit).
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(use-package org
  :mode (("\\.org$" . org-mode))
  :ensure org-plus-contrib)

;; Make a .el file out of the code in config.org, then run it.  This
;; bit of code is responsible for the loading other 2k+ lines of code
;; for this config.
(org-babel-load-file
 (expand-file-name "config.org"
                   user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;; WRAP UP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load system-specific personal settings
(let ((post (concat personal-dir ".exclusive/post.el")))
  (if (file-exists-p post)
      (load post)))
