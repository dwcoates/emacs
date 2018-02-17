;;; gui/theme/package.el -*- lexical-binding: t; -*-

(package! atchka-theme
  :recipe (:fetcher github
           :repo "dwcoates/atchka"
           :files ("*")))

;; Brighten non-source buffers
(package! solaire-mode)
