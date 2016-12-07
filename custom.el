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
 '(company-minimum-prefix-length 2)
 '(company-tooltip-margin 2)
 '(custom-enabled-themes nil)
 '(custom-safe-themes
   (quote
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "1af615aabe926740357c6b6c3a611470054efd3bdee28dae6a1bd054d0b43f7d" "7630f2ea026c651a07239948bba7450bc73ec0fa247bc3d123ba4558fab6818a" "422662ca327aec391aaee6e36836af1a489e44b2ead0ad6ad5f22775fa2b0c06" "930a202ae41cb4417a89bc3a6f969ebb7fcea5ffa9df6e7313df4f7a2a631434" "b8c99a002227c98b796605d479bcca6fe71c1c20f22fe68fb9d65b91a9373563" "c87cc60d01cf755375759d165c1d60d9586c6a31f0b5437a0378c2a93cfc8407" default)))
 '(fci-rule-color "#151515")
 '(fringe-mode (quote (4 . 4)) nil (fringe))
 '(org-agenda-files (quote ("~/personal/everything.org")))
 '(org-deadline-warning-days 2)
 '(org-default-notes-file "/home/dodge/personal/everything.org")
 '(org-directory "~/personal/")
 '(org-extend-today-until 4)
 '(org-footnote-section nil)
 '(org-from-is-user-regexp "\\<Dodge Coates\\>")
 '(org-goto-interface (quote outline-path-completion))
 '(org-goto-max-level 10)
 '(org-scheduled-delay-days 0)
 '(org-src-fontify-natively t)
 '(package-selected-packages
   (quote
    (tagedit smex clojure-mode-extra-font-locking geiser ztree rainbow-delimiters jedi epc powerline nyan-mode nyan-prompt magit helm company flycheck-clojure clj-refactor cider px latex-preview-pane company-c-headers company-irony zygospore yasnippet ws-butler volatile-highlights undo-tree smartparens light-soap-theme iedit helm-swoop helm-projectile helm-gtags helm-company guide-key golden-ratio ggtags function-args flycheck duplicate-thing dtrt-indent dash-functional dash-at-point comment-dwim-2 clean-aindent-mode anzu)))
 '(python-shell-prompt-input-regexps
   (quote
    ("$ " "\\.\\.\\. " "In \\[[0-9]+\\]: " "In : " "\\.\\.\\.: ")))
 '(sml/theme (quote dark)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#222222" :foreground "light gray" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 128 :width normal :foundry "unknown" :family "Ubuntu Mono"))))
 '(button ((t (:inherit link))))
 '(company-preview ((t (:background "gray12" :foreground "wheat"))))
 '(company-preview-common ((t (:inherit company-preview :foreground "navajo white"))))
 '(company-scrollbar-bg ((t (:inherit company-tooltip :background "dim gray"))))
 '(company-scrollbar-fg ((t (:background "gray14"))))
 '(company-tooltip ((t (:background "gray10" :foreground "navajo white"))))
 '(company-tooltip-annotation ((t (:inherit company-tooltip :foreground "black"))))
 '(company-tooltip-common ((t (:foreground "firebrick"))))
 '(company-tooltip-mouse ((t (:inherit highlight :background "gray42"))))
 '(company-tooltip-search ((t (:inherit company-tooltip-selection))))
 '(company-tooltip-selection ((t (:background "dim gray"))))
 '(custom-button ((t (:background "LightYellow3" :foreground "black" :box (:line-width 2 :style released-button)))))
 '(custom-comment ((t (:background "bisque4"))))
 '(helm-M-x-key ((t (:foreground "red3" :underline t))))
 '(helm-candidate-number ((t (:background "salmon2" :foreground "black"))))
 '(helm-selection ((t (:background "sienna"))))
 '(highlight ((t (:background "light goldenrod"))))
 '(linum ((t (:background "DarkGoldenrod4" :foreground "black"))))
 '(mode-line ((t (:background "burlywood4" :foreground "gray60" :inverse-video nil :box nil))))
 '(mode-line-buffer-id ((t (:foreground "aquamarine" :weight bold))))
 '(mode-line-highlight ((t (:box (:line-width 2 :color "yellow" :style released-button)))))
 '(mode-line-inactive ((t (:background "gray22" :foreground "gray52" :inverse-video nil :box nil))))
 '(org-agenda-done ((t (:foreground "olive drab" :weight ultra-light))))
 '(org-block-background ((t (:background "gray17"))))
 '(org-block-begin-line ((t (:foreground "gray14"))) t)
 '(org-block-end-line ((t (:foreground "gray14"))) t)
 '(org-level-1 ((t (:foreground "olive drab"))))
 '(org-level-2 ((t (:foreground "light coral"))))
 '(org-level-4 ((t (:inherit outline-4))))
 '(org-level-6 ((t (:inherit outline-7))))
 '(org-level-7 ((t (:foreground "yellow green"))))
 '(org-scheduled ((t (:foreground "yellow3"))))
 '(org-scheduled-today ((t (:foreground "dark orange" :weight bold))))
 '(org-upcoming-deadline ((t (:foreground "gray" :weight bold))))
 '(org-warning ((t (:foreground "red3" :weight bold))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "indian red"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "light sea green"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "orchid"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "goldenrod"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "olive drab"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "deep sky blue"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "violet red"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "SeaGreen2"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "chocolate"))))
 '(rainbow-delimiters-unmatched-face ((t (:foreground "red"))))
 '(sml/filename ((t (:inherit sml/global :foreground "lemon chiffon" :weight bold))))
 '(sml/global ((t (:foreground "DarkOrange4" :inverse-video nil))))
 '(sml/modes ((t (:inherit sml/global :foreground "black"))))
 '(sml/prefix ((t (:inherit sml/global :foreground "IndianRed4"))))
 '(sml/time ((t (:inherit sml/modes))))
 '(sp-pair-overlay-face ((t (:inherit highlight :foreground "black")))))
