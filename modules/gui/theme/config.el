;;; gui/theme/config.el -*- lexical-binding: t; -*-

(add-to-list 'load-path (concat doom-lisp-dir "atchka"))

(require 'atchka-theme)

(add-hook 'after-init-hook (lambda () (load-theme 'atchka t)))
