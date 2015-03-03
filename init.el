(setq default-directory "/home/dodge/")
(cd "./.emacs.d") ; Temporarily exists for now so I can configure emacs a bit faster.


(add-to-list 'load-path "~/.emacs.d/custom")


(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)


(setq gc-cons-threshold 100000000)
(setq inhibit-startup-message t)


(require 'appearance)

(defalias 'yes-or-no-p 'y-or-n-p)


;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))


(defconst dep-packages
  '(anzu
    company
    duplicate-thing
    ggtags
    helm
    dash-at-point
    dash-functional
    helm-gtags
    helm-projectile
    helm-swoop
    golden-ratio
    function-args
    clean-aindent-mode
    comment-dwim-2
    dtrt-indent
    ws-butler
    iedit
    yasnippet
    smartparens
    projectile
    volatile-highlights
    undo-tree
    helm-company
    guide-key
    zygospore))

(defun install-packages ()
  "Install all required packages."
  (interactive)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package dep-packages)
    (unless (package-installed-p package)
      (package-install package))))

(install-packages)


(require 'setup-helm)
(require 'setup-helm-gtags)
(require 'setup-cedet)
(require 'setup-editing)
(require 'setup-rgrep)
(require 'setup-flycheck)


;; guide-key, displays possible key binding completions
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-x v" "C-x 8" "C-x +"))
(guide-key-mode 1)
(setq guide-key/recursive-key-sequence-flag t)
(setq guide-key/popup-window-position 'bottom)


(add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends) '(company-elisp))))


(windmove-default-keybindings)


;; company
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-backends (delete 'company-semantic company-backends))

(define-key c-mode-map  [(control tab)] 'company-complete)
(define-key c++-mode-map  [(control tab)] 'company-complete)

(global-set-key (kbd "C-c <tab>") 'helm-company)

;; company-c-headers
;(add-to-list 'company-c-headers-path-system "/usr/include/c++/4.8/")
(add-to-list 'company-backends 'company-c-headers)

;; hs-minor-mode for folding source code
(add-hook 'c-mode-common-hook 'hs-minor-mode)

;; Available C style:
;; “gnu”: The default style for GNU projects
;; “k&r”: What Kernighan and Ritchie, the authors of C used in their book
;; “bsd”: What BSD developers use, aka “Allman style” after Eric Allman.
;; “whitesmith”: Popularized by the examples that came with Whitesmiths C, an early commercial C compiler.
;; “stroustrup”: What Stroustrup, the author of C++ used in his book
;; “ellemtel”: Popular C++ coding standards as defined by “Programming in C++, Rules and Recommendations,” Erik Nyquist and Mats Henricson, Ellemtel
;; “linux”: What the Linux developers use for kernel development
;; “python”: What Python developers use for extension modules
;; “java”: The default style for java-mode (see below)
;; “user”: When you want to define your own style
(setq
 c-default-style "linux" ;; set style to "linux"
 )

(global-set-key (kbd "RET") 'newline-and-indent)  ; automatically indent when press RET

;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

;; use space to indent by default
(setq-default indent-tabs-mode nil)

;; set appearance of a tab that is represented by 4 spaces
(setq-default tab-width 4)

;; Compilation
(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))

;; setup GDB
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )



;; this variables must be set before load helm-gtags
(setq helm-gtags-prefix-key "\C-cg")

(require 'clean-aindent-mode)
(add-hook 'prog-mode-hook 'clean-aindent-mode)

;; Package: dtrt-indent
(require 'dtrt-indent)
(dtrt-indent-mode 1)

;; Package: ws-butler
(require 'ws-butler)
(add-hook 'prog-mode-hook 'ws-butler-mode)

;; Package: yasnippet
(require 'yasnippet)
(yas-global-mode 1)



;; Package: smartparens
(require 'smartparens-config)
(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(sp-use-paredit-bindings)

(show-smartparens-global-mode +1)
(smartparens-global-mode 1)



;; Package: golden ratio
(require 'golden-ratio)
(golden-ratio-mode t)



;; ensure golden-ratio compatibility with helm
(defun pl/helm-alive-p ()
  (if (boundp 'helm-alive-p)
      (symbol-value 'helm-alive-p)))

(add-to-list 'golden-ratio-inhibit-functions 'pl/helm-alive-p)



;; Package: projectile
(require 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching t)

(require 'helm-projectile)
(helm-projectile-on)
(setq projectile-completion-system 'helm)
(setq projectile-indexing-method 'alien)


;; Package: speedbar
(setq speedbar-show-unknown-files t)
;(setq golden-ratio-exclude-modes '("speed-bar-mode"))


;; Package zygospore
(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(blink-cursor-delay 1.5)
 '(blink-cursor-mode t)
 '(company-c-headers-path-system
   (quote
    ("/usr/include/" "/usr/local/include/" "/usr/include/c++/4.6/")))
 '(company-c-headers-path-user (quote ("/home/dodge/workspace")))
 '(custom-enabled-themes (quote (jazz)))
 '(custom-safe-themes
   (quote
    ("789844278c5a75283b5015c1fc7bebe7e4cf97843b8f8cffe21fafa05e81e90a" "c87cc60d01cf755375759d165c1d60d9586c6a31f0b5437a0378c2a93cfc8407" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "saddle brown")))))
