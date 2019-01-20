;;; lang/org/+atchka-org-blocks.el -*- lexical-binding: t; -*-

;;; atchka-org-blocks --- A nice-looking way of presenting Org code blocks
;;
;;; Commentary:
;;
;; Dodge W. Coates.  Inspired by Alisa Leshchenko, coolest gal in the world.
;;
;;; Code:

(require 'org)

(defconst atchka--org-block-header-height 1 "Height for org block lines.")

(defgroup atchka-org-faces nil
  "Open helm."
  :prefix "atchka-org-"
  :group 'faces)

(defface atchka-org-block-lines-face
  `((((background dark))
     (:background "#8b475d"
                  :foreground "#8b475d"
                  :height ,atchka--org-block-header-height))
    (((background light))
     '(:background "#8b475d"
                   :foreground "#8b475d"
                   :height ,atchka--org-block-header-height)))
  "Face used for source block content"
  :group 'atchka-org-faces)

(defface atchka-org-source-block-face
  '((((background dark)) :background "gray25")
    (((background light)) :background "SlateGray"))
  "`atchka-org' Org source block face"
  :group 'atchka-org-faces)

(defvar atchka-org-babel-languages
  '("asymptote" "C" "clojure" "css" "ditaa" "dot" "emacs-lisp" "gnuplot"
    "haskell" "js" "latex" "ledger" "lisp" "matlab" "mscgen" "ob-doc-ocaml"
    "octave" "oz" "perl" "plantuml" "python" "R" "ruby" "sass"
    "scheme" "ob-doc-screen" "sh" "sql" "sqlite" "ipython" "org" "example"))

(defvar atchka-org-src-block-faces-backup org-src-block-faces)

;; Source block content
(setq org-src-fontify-natively t     ; syntax highlighting in source blocks
      org-src-tab-acts-natively t)   ; expected tabbing in source block

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; ORG SOURCE BLOCKS ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-show-block-lines ()
  "Show the Org-block lines.
This is useful because the atchka theme obfuscates block markup."
  (interactive)
  (when (eq major-mode 'org-mode)
    (set-face-attribute 'org-block-begin-line
                        (selected-frame)
                        :height 100
                        :foreground "black")))

(defun org-hide-block-lines ()
  "Hide the org block lines."
  (interactive)
  (when (eq major-mode 'org-mode)
    (set-face-attribute 'org-block-begin-line nil
                        :height (truncate (* atchka--org-block-header-height 10))
                        :foreground (face-attribute 'org-block-begin-line :background))))

(defun org-skip-source-next-advice ()
  "Advice for the `next-line' function.
Please `next-line' past org-block headers'"
  (interactive)
  (when (and (eq major-mode 'org-mode)
             (save-excursion
               (forward-line)
               (call-interactively 'beginning-of-line)
               (or
                (re-search-forward "#\\+begin_src[ ]+?"
                                   (line-end-position) t)
                (re-search-forward "#\\+end_src[ ]*?"
                                   (line-end-position) t))))
    (forward-line)))

(defun org-skip-source-previous-advice ()
  "Advice for the `previous-line' function.
Please `previous-line' past org-block headers'"
  (interactive)
  (when (and
         (eq major-mode 'org-mode)
         (save-excursion
          (forward-line -1)
          (call-interactively 'beginning-of-line)
          (or
           (re-search-forward "#\\+begin_src[ ]+?"
                              (line-end-position) t)
           (re-search-forward "#\\+end_src[ ]*?"
                              (line-end-position) t))))
    (forward-line -1)))

(defun protect-faces-region (begin end)
  (interactive "r")
  (remove-overlays begin end 'protect-faces t)
  (goto-char begin)
  (while (< (point) end)
    (let ((face (get-text-property (point) 'face))
          (next (next-single-property-change (point) 'face nil end)))
      (cond
       ((memq face '(org-block-begin-line
                      org-block-end-line
                      atchka-org-source-block-face))
        (let ((ov (make-overlay (point) next)))
          (overlay-put ov 'priority 9999)
          (overlay-put ov 'protect-faces t)
          (overlay-put
           ov 'face 'atchka-org-block-lines-face)))
       (t nil))
      (goto-char next))))

;;;###autoload
(define-minor-mode atchka-org-minor-mode
  "Minor mode for improving org-mode source code appearance."
  :init-value nil
  :lighter " atchka"
  :keymap `(((kbd "C-c C-v C-;") . org-show-block-lines)
            ((kbd "C-c C-v C-:") . org-hide-block-lines))
  (cond (atchka-org-minor-mode
         ;; yasnippet
         (add-hook 'yas-before-expand-snippet-hook 'org-show-block-lines t)
         (add-hook 'yas-after-exit-snippet-hook 'org-hide-block-lines)
         ;; next/prev line
         (advice-add 'next-line :before 'org-skip-source-next-advice)
         (advice-add 'previous-line :before 'org-skip-source-previous-advice)
         ;; set faces
         (set-face-attribute 'org-block nil :inherit 'atchka-org-source-block-face)
         (set-face-attribute 'org-block-begin-line nil :inherit 'atchka-org-block-lines-face)
         (set-face-attribute 'org-block-end-line nil :inherit 'atchka-org-block-lines-face)
         ;; source blocks
         (setq org-src-block-faces ;; I don't know why this is still necessasry
               (mapcar (lambda (lang) (list lang 'atchka-org-source-block-face))
                       atchka-org-babel-languages)))
        (t
         ;; yasnippet
         (when (require 'yasnippet nil t)
           (remove-hook 'yas-before-expand-snippet-hook 'org-show-block-lines)
           (remove-hook 'yas-after-exit-snippet-hook 'org-hide-block-lines))
         ;; next/prev line
         (advice-remove 'next-line 'org-skip-source-next-advice)
         (advice-remove 'previous-line 'org-skip-source-previous-advice)
         ;; unset faces
         (set-face-attribute 'org-block nil :inherit nil)
         (set-face-attribute 'org-block-begin-line nil :inherit nil)
         (set-face-attribute 'org-block-end-line nil :inherit nil)
         ;; source blocks
         (setq org-src-block-faces atchka-org-src-block-faces-backup)))
  (org-restart-font-lock))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name)))
  (when (not window-system)
    (custom-set-faces '(default ((t (:background nil)))))))

(provide 'atchka-org-blocks)

;;; atchka-org ends here
