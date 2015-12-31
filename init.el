;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq debug-on-error t)

;; setup load path
(add-to-list 'load-path "~/.emacs.d/custom")

;; Keep emacs-generated custom settings in a separate file so they don't pollute init.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)



;; Sets up dependencies for the rest of the configurations.
;; Must be loaded before rest of package configurations, obviously
(require 'setup-packages)


;; this variables must be set before load helm-gtags
(setq helm-gtags-prefix-key "\C-cg")

;; ****** Setup rest of package configurations *******
(require 'setup-appearance-and-navigation)
(require 'setup-helm)
(require 'setup-helm-gtags)
(require 'setup-cedet)
(require 'setup-rgrep)
(require 'setup-flycheck)
(require 'setup-org-mode)
(require 'setup-company)
(require 'setup-magit)
(require 'setup-clojure)
(require 'setup-editing)
;; Package: projectile
(require 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching t)
(require 'helm-projectile)
(helm-projectile-on)
(setq projectile-completion-system 'helm)
(setq projectile-indexing-method 'alien)
;; Package: yasnippet
(require 'yasnippet)
(yas-global-mode 1)
;; Package: clean-aindent-mode
(require 'clean-aindent-mode)
(add-hook 'prog-mode-hook 'clean-aindent-mode)
;; Package: dtrt-indent
(require 'dtrt-indent)
(dtrt-indent-mode 1)
;; Package: ws-butler
(require 'ws-butler)
(add-hook 'prog-mode-hook 'ws-butler-mode)
;; Package: tramp
(setq tramp-default-method "ssh")

(push (substitute-in-file-name "path-to-ztree-directory") load-path)
(require 'ztree)


(add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends) '(company-elisp))))


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
 gdb-show-main t)


;; Package zygospore
(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-window)


(require 'setup-specifics)


(provide 'init)
;;; init.el ends here
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
