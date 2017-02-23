;;; Package --- Summary:
;; Atchka theme description.  By Dodge Coates, code base taken from doom-molokai
;; by Henrik Lasser and using Henrik Lasser's doom-themes package.
;;
;;; Commentary:
;; Atchka theme
;;
;;; Code:

(require 'doom-themes)

(deftheme atchka "A dark theme.")

(let ((c '((class color) (min-colors 89)))
      ;; emphasis
      (bold   doom-enable-bold)
      (italic doom-enable-italic)
      (padding `(:line-width 5))
      ;; colors
      (bg             "#1D1F20")
      (bg-l           "#222425")
      (fg             "#D6D6D4")
      (subtle         "#aab6c7")
      (vsubtle        "#556172")
      (vvsubtle       "#354152")
      (dim-highlight  "#3f4b56")
      (black          "#000000")
      (grey           "#C0C5CF")
      (grey-.5        "#828284")
      (grey-1         "#525254")
      (grey-2         "#39393D")
      (white          "#FFFFFF")
      (white-1        "#EAEADB")
      (yellow         "#E2C770")
      (orange         "#FD971F")
      (red            "#E74C3C")
      (magenta        "#F92672")
      (violet         "#9C91E4")
      (blue           "#268BD2")
      (blue+2         "#727280")
      (cyan           "#66D9EF")
      (green          "#B6E63E")
      (green-3        "#86B20E")
      (dark-cyan      "#8FA1B3")
      (light-cyan     "#CBECFF"))
  (let* ((search-bg      "#fa8072")
         (search-fg      white)
         (search-rest-bg "gray15")
         (search-rest-fg "#fa8072")
         (highlight      orange)
         (vertical-bar   grey-2)
         (current-line   "#1F1F1F")
         (selection      "#535556")
         (builtin        orange)
         (comments       grey-1)
         (constants      green)
         (delimiters     "#c0c5ce")
         (functions      cyan)
         (keywords       magenta)
         (methods        dark-cyan)
         (operators      violet)
         (type           cyan)
         (strings        green)
         (variables      orange)

         (error-highlight red)

         (linum-bg       current-line)
         (linum-fg       "#3F3F48")
         (linum-hl-fg    orange)
         (linum-hl-bg    current-line)

         (active-minibuffer "#404046")
         (modeline-fg    white)
         (modeline-fg-2  orange)
         (modeline-fg-3  orange)
         (modeline-fg-inactive  "#80858F")
         (modeline-bg    grey-2)
         (modeline-bg-2  grey-2)
         (modeline-bg-3  grey-2)
         (modeline-bg-inactive  current-line)

         (vc-modified    grey-2)
         (vc-added       green-3)
         (vc-deleted     red))

    (custom-theme-set-faces
     'atchka
     ;; Doom faces
     `(doom-default
       ((((type graphic)) :inherit default :background ,bg-l)
        (t                :inherit default)))
     `(doom-hl-line
       ((((type graphic)) :background ,bg)
        (t                :inherit hl-line)))
     `(doom-linum
       ((((type graphic)) :inherit linum :background ,bg-l)
        (t                :inherit linum)))
     `(doom-minibuffer-active ((,c (:background ,bg-l))))
     `(doom-nlinum-highlight  ((,c (:foreground ,linum-hl-fg :bold nil))))
     `(doom-flycheck-error    ((,c (:underline nil :foreground ,black :background ,red))))
     `(doom-flycheck-warning  ((,c (:underline nil :foreground ,black :background ,yellow))))
     `(doom-flycheck-info     ((,c (:underline nil :foreground ,black :background ,green))))
     ;; Text
     `(default                             ((,c (:foreground ,fg :background ,bg))))
     `(fringe                              ((,c (:background ,bg-l :foreground ,grey-1))))
     `(cursor                              ((,c (:background ,white))))
     `(hl-line                             ((,c (:background ,bg-l))))
     `(region                              ((,c (:background ,grey-2 :foreground "Tomato"))))
     `(highlight                           ((,c (:foreground ,yellow :inverse-video t))))
     `(shadow                              ((,c (:foreground ,orange))))
     `(minibuffer-prompt                   ((,c (:foreground ,orange))))
     `(tooltip                             ((,c (:background ,grey-2 :foreground ,orange))))
     `(error                               ((,c (:foreground ,red   ))))
     `(warning                             ((,c (:foreground ,yellow))))
     `(success                             ((,c (:foreground ,green ))))
     ;; `(secondary-selection              ((,c (:background ,orange))))
     ;; `(lazy-highlight                   ((,c (:background ,orange))))
     ;; `(match                            ((,c (:background ,magenta))))
     `(bold                                ((,c (:weight bold  :foreground ,white))))
     `(italic                              ((,c (:slant italic :foreground ,subtle))))
     `(bold-italic                         ((,c (:weight bold  :slant italic :foreground ,white))))
     `(trailing-whitespace                 ((,c (:background "#884444"))))
     `(whitespace-tab                      ((,c (:foreground ,grey-2))))
     `(whitespace-newline                  ((,c (:foreground ,grey-2))))
     `(whitespace-trailing                 ((,c (:background ,grey-2))))
     `(vertical-border                     ((,c (:foreground ,vertical-bar :background ,vertical-bar))))
     `(linum                               ((,c (:foreground ,linum-fg :background ,bg :bold nil))))
     `(font-lock-builtin-face              ((,c (:foreground ,builtin))))
     `(font-lock-comment-face              ((,c (:foreground "#8fbc8f"))))
     `(font-lock-comment-delimiter-face    ((,c (:inherit font-lock-comment-face))))
     `(font-lock-doc-face                  ((,c (:foreground ,blue+2))))
     `(font-lock-doc-string-face           ((,c (:foreground ,blue+2))))
     `(font-lock-constant-face             ((,c (:foreground ,constants))))
     `(font-lock-function-name-face        ((,c (:foreground ,functions))))
     `(font-lock-keyword-face              ((,c (:foreground ,keywords))))
     `(font-lock-string-face               ((,c (:foreground ,strings))))
     `(font-lock-type-face                 ((,c (:foreground ,type))))
     `(font-lock-variable-name-face        ((,c (:foreground ,variables))))
     `(font-lock-warning-face              ((,c (:foreground ,red))))
     `(font-lock-negation-char-face        ((,c (:foreground ,operators :bold t))))
     `(font-lock-preprocessor-char-face    ((,c (:foreground ,operators :bold t))))
     `(font-lock-regexp-grouping-backslash ((,c (:foreground ,operators :bold t))))
     `(font-lock-regexp-grouping-construct ((,c (:foreground ,operators :bold t))))
     ;; Parentheses
     `(show-paren-match                    ((,c (:foreground ,magenta :inverse-video t))))
     `(sp-pair-overlay-face                ((,c (:foreground ,yellow :bold t))))

     ;; Modeline
     `(mode-line-buffer-id                 ((,c (:foreground "aquamarine" ) :weight 'bold)))
     `(mode-line-highlight                 ((,c (:box (:line-width 2 :color "yellow" :style released-button)))))
     `(mode-line-inactive                  ((,c (:background "gray22" ) :foreground "gray52" :inverse-video nil :box nil)))
     `(mode-line                           ((,c (:background "burlywood4" ) :foreground "gray60"  :inverse-video nil :box nil)))
     ;; sml
     `(sml/global ((,c (:foreground "DarkOrange4" :inverse-video nil))))
     `(sml/filename ((,c (:inherit sml/global :foreground "lemon chiffon" :weight bold))))
     `(sml/modes ((,c (:inherit sml/global :foreground "black"))))
     `(sml/prefix ((,c (:inherit sml/global :foreground "IndianRed4"))))
     `(sml/time ((,c (:inherit sml/modes))))

     ;`(mode-line-is-modified               ((,c (:foreground ,magenta :background nil :bold t))))
;     `(mode-line-buffer-file               ((,c (:foreground ,white :bold t))))
     ;`(mode-line-buffer-path               ((,c (:foreground ,grey))))
;     `(mode-line-count-face                ((,c (:foreground ,black :background ,magenta))))
;     `(spaceline-flycheck-error            ((,c (:underline nil :foreground ,black :background ,red))))
     ;`(spaceline-flycheck-warning          ((,c (:underline nil :foreground ,black :background ,yellow))))
;     `(spaceline-flycheck-info             ((,c (:underline nil :foreground ,black :background ,green))))
     ;`(spaceline-highlight-face            ((,c (:foreground ,black :background ,highlight))))
     ;; Search
     `(isearch ((,c (:background "indian red" :foreground "white"
                                 ))))


     `(lazy-highlight-face         ((,c (:foreground ,search-rest-fg :background "white"))))
     `(swiper-minibuffer-match-face-1 ((t :background "#dddddd")))
     `(swiper-minibuffer-match-face-2 ((t :background "#bbbbbb" :weight bold)))
     `(swiper-minibuffer-match-face-3 ((t :background "#bbbbff" :weight bold)))
     `(swiper-minibuffer-match-face-4 ((t :background "#ffbbff" :weight bold)))
     `(ivy-current-match ((((class color) (background light))
                           :background "#1a4b77" :foreground "white")
                          (((class color) (background dark))
                           :background "#65a7e2" :foreground "black")))
     `(ivy-current-match ((t (:background "gray15" :foreground "white"))))
     ;;
     ;; Plugins
     ;;
     ;; hide-show
     `(hs-face                     ((,c (:foreground ,comments :background ,black))))
     `(hs-fringe-face              ((,c (:foreground ,orange))))
     ;; flycheck
     `(flycheck-error              ((,c (:underline (:style wave :color ,red)    :background ,grey-2))))
     `(flycheck-warning            ((,c (:underline (:style wave :color ,yellow) :background ,grey-2))))
     `(flycheck-info               ((,c (:underline (:style wave :color ,green)  :background ,grey-2))))
     `(flyspell-incorrect          ((,c (:underline (:style wave :color ,error-highlight) :inherit unspecified))))
     ;; indent-guide, highlight-{quoted,numbers,indentation}-mode
     `(highlight-indentation-face                 ((,c (:background ,current-line))))
     `(highlight-indentation-current-column-face  ((,c (:background ,current-line))))
     `(highlight-quoted-symbol     ((,c (:foreground ,yellow))))
     `(highlight-quoted-quote      ((,c (:foreground ,magenta))))
     `(highlight-numbers-number    ((,c (:foreground ,constants))))
     `(indent-guide-face           ((,c (:foreground "#2F2F38"))))
     ;; re-builder
     `(reb-match-0                 ((,c (:foreground ,orange   :inverse-video t))))
     `(reb-match-1                 ((,c (:foreground ,magenta  :inverse-video t))))
     `(reb-match-2                 ((,c (:foreground ,green    :inverse-video t))))
     `(reb-match-3                 ((,c (:foreground ,yellow   :inverse-video t))))
     ;; workgroups2
     `(wg-current-workgroup-face   ((,c (:foreground ,black   :background ,orange))))
     `(wg-other-workgroup-face     ((,c (:foreground ,grey-.5 :background ,current-line))))
     ;; neotree
     `(neo-root-dir-face           ((,c (:foreground ,cyan))))
     `(neo-file-link-face          ((,c (:foreground ,white))))
     `(neo-dir-link-face           ((,c (:foreground ,orange))))
     `(neo-expand-btn-face         ((,c (:foreground ,magenta))))
     ;; company-mode
     `(company-tooltip             ((,c (:background ,black :foreground ,fg))))
     `(company-tooltip-common      ((,c (:foreground ,orange))))
     `(company-tooltip-search      ((,c (:foreground ,search-fg :background ,highlight))))
     `(company-tooltip-selection   ((,c (:background ,selection))))
     `(company-tooltip-mouse       ((,c (:background ,magenta :foreground ,bg))))
     `(company-scrollbar-bg        ((,c (:background ,black))))
     `(company-scrollbar-fg        ((,c (:background ,orange))))
     `(company-preview             ((,c (:foreground ,orange))))
     `(company-preview-common      ((,c (:foreground ,magenta :background ,grey-1))))
     `(company-preview-search      ((,c (:inherit company-tooltip-search))))
     ;; pop-tip
     `(popup                       ((,c (:inherit tooltip))))
     `(popup-tip-face              ((,c (:inherit tooltip))))
     ;; evil-mode
     `(evil-ex-substitute-replacement ((,c (:foreground ,magenta :background ,black :bold ,bold))))
     `(evil-search-highlight-persist-highlight-face ((,c (:background ,search-rest-bg))))
     ;; evil-snipe
     `(evil-snipe-first-match-face ((,c (:foreground ,search-fg :background ,search-bg))))
     `(evil-snipe-matches-face     ((,c (:foreground ,search-bg :underline t))))
     ;; Volatile highlights
     `(vhl/default-face            ((,c (:background ,grey-2))))
     ;; VCS
     `(diff-hl-change              ((,c (:foreground ,vc-modified))))
     `(diff-hl-delete              ((,c (:foreground ,vc-deleted))))
     `(diff-hl-insert              ((,c (:foreground ,vc-added))))
     `(git-gutter:modified         ((,c (:foreground ,vc-modified))))
     `(git-gutter:added            ((,c (:foreground ,vc-added))))
     `(git-gutter:deleted          ((,c (:foreground ,vc-deleted))))
     `(git-gutter+-modified        ((,c (:foreground ,vc-modified :background nil))))
     `(git-gutter+-added           ((,c (:foreground ,vc-added :background nil))))
     `(git-gutter+-deleted         ((,c (:foreground ,vc-deleted :background nil))))
     ;; Rainbow delimiters
     `(rainbow-delimiters-depth-1-face   ((,c (:foreground ,magenta))))
     `(rainbow-delimiters-depth-2-face   ((,c (:foreground ,orange))))
     `(rainbow-delimiters-depth-3-face   ((,c (:foreground ,yellow))))
     `(rainbow-delimiters-depth-4-face   ((,c (:foreground ,green))))
     `(rainbow-delimiters-depth-5-face   ((,c (:foreground ,cyan))))
     `(rainbow-delimiters-unmatched-face ((,c (:foreground ,red :inverse-video t))))
     ;; Helm
     `(helm-selection              ((,c (:background ,selection))))
     `(helm-match                  ((,c (:foreground ,magenta))))
     `(helm-source-header          ((,c (:background ,current-line :foreground ,grey-1))))
     `(helm-swoop-target-line-face ((,c (:foreground ,highlight :inverse-video t))))
     `(helm-ff-file              ((,c (:foreground ,grey))))
     `(helm-ff-prefix            ((,c (:foreground ,magenta))))
     `(helm-ff-dotted-directory  ((,c (:foreground ,grey-1))))
     `(helm-ff-directory         ((,c (:foreground ,orange))))
     `(helm-ff-executable        ((,c (:foreground ,white :slant italic))))
     ;; Avy
     `(avy-lead-face-0    ((,c (:background ,orange :foreground ,black))))
     `(avy-lead-face-1    ((,c (:background ,orange :foreground ,black))))
     `(avy-lead-face-2    ((,c (:background ,orange :foreground ,black))))
     `(avy-lead-face      ((,c (:background ,orange :foreground ,black))))

     ;;
     ;; Language-specific
     ;;

     ;; (css|scss)-mode
     `(css-proprietary-property ((,c (:foreground ,keywords))))
     ;; js2-mode
     `(js2-function-param  ((,c (:foreground ,variables))))
     `(js2-function-call   ((,c (:foreground ,functions))))
     `(js2-object-property ((,c (:foreground ,methods))))
     `(js2-jsdoc-tag       ((,c (:foreground ,comments))))
     ;; web-mode
     `(web-mode-doctype-face           ((,c (:foreground ,comments))))
     `(web-mode-html-tag-face          ((,c (:foreground ,methods))))
     `(web-mode-html-tag-bracket-face  ((,c (:foreground ,methods))))
     `(web-mode-html-attr-name-face    ((,c (:foreground ,type))))
     `(web-mode-html-entity-face       ((,c (:foreground ,cyan :italic t))))
     `(web-mode-block-control-face     ((,c (:foreground ,orange))))
     ;;`(web-mode-html-tag-bracket-face  ((,c (:foreground ,operators))))
     ;; markdown-mode
     `(markdown-header-face           ((,c (:foreground ,orange))))
     `(markdown-header-delimiter-face ((,c (:foreground ,orange))))
     `(markdown-blockquote-face       ((,c (:foreground ,blue+2))))
     `(markdown-markup-face           ((,c (:foreground ,cyan))))
     `(markdown-inline-face           ((,c (:foreground ,cyan))))
     `(markdown-list-face             ((,c (:foreground ,magenta))))
     `(markdown-pre-face              ((,c (:foreground ,cyan))))
     `(markdown-header-face-1         ((,c (:inherit markdown-header-face))))
     `(markdown-header-face-2         ((,c (:inherit markdown-header-face))))
     `(markdown-header-face-3         ((,c (:inherit markdown-header-face))))
     `(markdown-header-face-4         ((,c (:inherit markdown-header-face))))
     `(markdown-header-face-5         ((,c (:inherit markdown-header-face))))
     `(markdown-header-face-6         ((,c (:inherit markdown-header-face))))
     ;;`(markdown-header-rule-face       (:inherit shadow))
     ;;`(markdown-italic-face            (:inherit italic))
     ;;`(markdown-link-face              (:inherit shadow))
     ;;`(markdown-link-title-face        (:inherit link))
     ;;`(markdown-url-face               (:inherit link))
     ;; org-mode
     )

    (custom-theme-set-variables
     'atchka
     `(vc-annotate-color-map
       '((20 .  ,green)
         (40 .  ,(doom-blend yellow green (/ 1.0 3)))
         (60 .  ,(doom-blend yellow green (/ 2.0 3)))
         (80 .  ,yellow)
         (100 . ,(doom-blend orange yellow (/ 1.0 3)))
         (120 . ,(doom-blend orange yellow (/ 2.0 3)))
         (140 . ,orange)
         (160 . ,(doom-blend magenta orange (/ 1.0 3)))
         (180 . ,(doom-blend magenta orange (/ 2.0 3)))
         (200 . ,magenta)
         (220 . ,(doom-blend red magenta (/ 1.0 3)))
         (240 . ,(doom-blend red magenta (/ 2.0 3)))
         (260 . ,red)
         (280 . ,(doom-blend grey red (/ 1.0 4)))
         (300 . ,(doom-blend grey red (/ 2.0 4)))
         (320 . ,(doom-blend grey red (/ 3.0 4)))
         (340 . ,grey)
         (360 . ,grey)))
     `(vc-annotate-very-old-color nil)
     `(vc-annotate-background ,black))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name)))
  (when (not window-system)
    (custom-set-faces '(default ((t (:background nil)))))))

(provide-theme 'atchka)

;;; atchka ends here
