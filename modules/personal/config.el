;;; personal/config.el -*- lexical-binding: t; -*-

(load! bindings)

;;
;; Plugins
;;

(def-package! emacs-snippets :after yasnippet)

;;
;; Config
;;

(after! epa
  (setq epa-file-encrypt-to (or epa-file-encrypt-to user-mail-address)
        ;; With GPG 2.1, this forces gpg-agent to use the Emacs minibuffer to
        ;; prompt for the key passphrase.
        epa-pinentry-mode 'loopback))

