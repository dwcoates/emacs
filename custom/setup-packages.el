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
  '(anzu
    company
    duplicate-thing
    ggtags
    clojure-mode
    clojure-mode-extra-font-locking
    helm
    dash-at-point
    dash-functional
    helm-gtags
    helm-projectile
    helm-swoop
    cider
    smex
    projectile
    rainbow-delimiters
    tagedit
     golden-ratio
    function-args
    clean-aindent-mode
    comment-dwim-2
    dtrt-indent
    ws-butler
    flycheck
    clj-refactor
    iedit
    company-c-headers
    company-irony
    yasnippet
    smartparens
    flycheck-clojure
    projectile
    volatile-highlights
    undo-tree
    helm-company
    guide-key
    magit
    nyan-mode
    powerline
    ztree
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
