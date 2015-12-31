(require 'package)

;; setup package repos
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)


;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))


;; no dependency notes
(defvar dep-packages
  '(
    use-package
    ;; clojure
    clojure-mode
    clojure-mode-extra-font-locking
    flycheck-clojure
    cider
    ;clj-refactor
    ;; helm
    helm
    helm-gtags
    helm-projectile
    helm-swoop
    ;; misc
    anzu
    company
    duplicate-thing
    ggtags
    paredit
    dash-at-point
    dash-functional
    smex
    projectile
    rainbow-delimiters
    tagedit
    golden-ratio
    function-args
    clean-aindent-mode
    comment-dwim-2
    dtrt-indent
    ws-butler
    flycheck
    iedit
    company-c-headers
    company-irony
    yasnippet
    smartparens
    projectile
    volatile-highlights
    undo-tree
    helm-company
    guide-key
    magit
    nyan-mode
    powerline
    ztree
    geiser
    ;;    epc
    ;;    jedi
    zygospore))

(load-file "~/.emacs.d/geiser/elisp/geiser.el")


;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(if (eq system-type 'darwin)
    (add-to-list 'dep-packages 'exec-path-from-shell))

(dolist (p dep-packages)
  (when (not (package-installed-p p))
    (package-install p)))


(defun install-packages ()
  "Install all required packages."
  (interactive)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package dep-packages)
    (unless (package-installed-p package)
      (package-install package))))

(install-packages)

(provide 'setup-packages)
