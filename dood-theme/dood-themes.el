;;; Package --- summary:
;;
;; An emacs theme
;;
;;; Commentary:
;;
;; The code base for this theme configuration is taken from:
;; Henrik Lissner <http://github/hlissner> (doom-theme)
;; This is my take on it.
;;
;;; Code:

(require 'dash)

(defgroup dood-themes nil
  "Options for dood-themes"
  :group 'faces)

(defface dood-default '((t (:inherit default)))
  "Background face for source code windows."
  :group 'dood-themes)

(defface dood-minibuffer-active '((t (:inherit mode-line)))
  "Face for active minibuffer. See `dood-enable-bright-minibuffer'."
  :group 'dood-themes)

(defface dood-linum '((t (:inherit linum)))
  "Another linum face for darker windows (like popups)."
  :group 'dood-themes)

(defface dood-nlinum-highlight '((t (:inherit linum)))
  "A face for the nlinum overlay on the current line."
  :group 'dood-themes)

(defface dood-hl-line '((t (:inherit hl-line)))
  "A face for the current line highlight."
  :group 'dood-themes)

(defface dood-org-hide '((t (:inherit org-hide)))
  "A face for hidden elements in org-mode. Only active if `dood-buffer-mode' is active."
  :group 'dood-themes)

;;
(defcustom dood-enable-bold t
  "If nil, bold will remove removed from all faces."
  :group 'dood-themes
  :type 'boolean)

(defcustom dood-enable-italic t
  "If nil, italics will remove removed from all faces."
  :group 'dood-themes
  :type 'boolean)


;; Color helper functions
;; Shamelessly *borrowed* from solarized
(defun dood-name-to-rgb (color &optional frame)
  (mapcar (lambda (x) (/ x (float (car (color-values "#ffffff")))))
          (color-values color frame)))

(defun dood-blend (color1 color2 alpha)
  (apply (lambda (r g b) (format "#%02x%02x%02x" (* r 255) (* g 255) (* b 255)))
         (--zip-with (+ (* alpha it) (* other (- 1 alpha)))
                     (dood-name-to-rgb color1)
                     (dood-name-to-rgb color2))))

(defun dood-darken (color alpha)
  (dood-blend color "#000000" (- 1 alpha)))

(defun dood-lighten (color alpha)
  (dood-blend color "#FFFFFF" (- 1 alpha)))


(defun dood--face-remap-add-relative (orig-fn &rest args)
  "Advice function "
  (when (and (display-graphic-p) dood-buffer-mode)
    (let ((remap (assq (nth 0 args) face-remapping-alist)))
      (when remap (setf (nth 0 args) (cadr remap)))))
  (apply orig-fn args))
(advice-add 'face-remap-add-relative :around 'dood--face-remap-add-relative)

;;;###autoload
(defun dood-brighten-minibuffer ()
  (with-selected-window (minibuffer-window)
    (setq-local face-remapping-alist
                (append face-remapping-alist '((default dood-minibuffer-active))))))

;;;###autoload
(define-minor-mode dood-buffer-mode
  "Brighten source buffers by remapping common faces (like default, hl-line and
linum) to their dood-theme variants."
  :lighter " dood"
  :init-value nil
  (if dood-buffer-mode
      (progn
        ;; Don't reset remapped faces on `kill-all-local-variables'
        (make-variable-buffer-local 'face-remapping-alist)
        (put 'face-remapping-alist 'permanent-local t)
        ;; Brighten up file buffers; darken special and popup buffers
        (set-face-attribute 'fringe nil :background (face-attribute 'dood-default :background))
        ;; Update `dood-org-hide'
        (when (eq major-mode 'org-mode)
          (set-face-attribute 'dood-org-hide nil
                              :inherit 'org-hide
                              :background (face-attribute 'dood-default :background)
                              :foreground (face-attribute 'dood-default :background)))
        (setq-local face-remapping-alist
                    (append face-remapping-alist
                            '((default dood-default)
                              (hl-line dood-hl-line)
                              (linum dood-linum)
                              (org-hide dood-org-hide)))))
    (set-face-attribute 'fringe nil :background (face-attribute 'default :background))
    (put 'face-remapping-alist 'permanent-local nil)
    ;; Remove face remaps
    (mapc (lambda (key) (setq-local face-remapping-alist (assq-delete-all key face-remapping-alist)))
          '(default hl-line linum org-hide))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide 'dood-themes)
;;; dood-themes.el ends here
