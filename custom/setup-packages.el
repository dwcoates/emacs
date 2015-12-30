(require 'package)

;; setup package repos
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.org/packages/") t)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; no dependency notes
(defconst dep-packages
  '(
    ;; clojure
    clojure-mode
    clojure-mode-extra-font-locking
    flycheck-clojure
    cider
    clj-refactor
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
