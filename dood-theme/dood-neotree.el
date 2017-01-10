;;; doom-neotree.el

(require 'all-the-icons)

(defgroup dood-neotree nil
  "Options for dood's neotree theme"
  :group 'dood-themes)

;;
(defface dood-neotree-folder-face '((t (:inherit neo-dir-link-face)))
  "Base face for neotree folder icons. Also see `dood-neotree-open-folder-face' and
`dood-neotree-closed-folder-face'."
  :group 'dood-neotree)

(defface dood-neotree-chevron-face '((t (:inherit neo-dir-link-face)))
  "Face for chevron icons next to folders. See
`dood-neotree-closed-chevron-icon' and `dood-neotree-open-chevron-icon'."
  :group 'dood-neotree)

(defface dood-neotree-dir-face  '((t (:inherit (variable-pitch neo-dir-link-face))))
  "Face for directory labels."
  :group 'dood-neotree)

(defface dood-neotree-file-face '((t (:inherit (variable-pitch neo-file-link-face))))
  "Face for file name labels."
  :group 'dood-neotree)

(defface dood-neotree-root-face '((t (:inherit (variable-pitch neo-root-dir-face))))
  "Face used for root entry (the project name at the top)."
  :group 'dood-neotree)


;;
(defcustom dood-neotree-project-size 1.4
  "What :height to display the project icon at the top at."
  :type 'float
  :group 'dood-neotree)

(defcustom dood-neotree-folder-size 1.05
  "What :height to display the folder icons at."
  :type 'float
  :group 'dood-neotree)

(defcustom dood-neotree-chevron-size 0.8
  "What :height to display the chevron icons at."
  :type 'float
  :group 'dood-neotree)

(defcustom dood-neotree-line-spacing 2
  "Line-spacing for neotree buffer."
  :type 'symbol
  :group 'dood-neotree)

(define-obsolete-variable-alias 'dood-neotree-enable-file-icons 'dood-neotree-file-icons)
(defcustom dood-neotree-file-icons 'simple
  "The style to use for the file icons. Can be nil (disabled), non-nil (for a
diverse iconset), or 'simple, which is closest's to Atom's style as it only
distinguishes text, source, pdfs, images and binary files."
  :type '(choice
          (const :tag "A diverse array of file icons based on file type" t)
          (const :tag "Minimalistic file icons (like Atom's)" 'simple)
          (const :tag "Disable file icons" nil))
  :group 'dood-neotree)

(defcustom dood-neotree-enable-folder-icons t
  "If non-nil, display folder icons next to each file. Different icons are used
depending on whether the folder is a repo, symlink or regular folder."
  :type 'boolean
  :group 'dood-neotree)

(defcustom dood-neotree-enable-open-chevron-icons t
  "If non-nil, display the chevron-down icon next to each expanded folder."
  :type 'boolean
  :group 'dood-neotree)

(defcustom dood-neotree-enable-closed-chevron-icons t
  "If non-nil, display the chevron-right icon next to each collapsed folder."
  :type 'boolean
  :group 'dood-neotree)

(defcustom dood-neotree-enable-variable-pitch nil
  "If non-nil, labels will be use the `dood-neotree-dir-face' and
`dood-neotree-dir-face' faces, which inherit from the `variable-pitch' face."
  :type 'boolean
  :group 'dood-neotree)

(defvar dood--neotree-file-re
  `((code    . ,(concat "\\.\\(p?html?\\|xml\\|ya?ml\\|json\\|tpl\\|conf\\|erb\\|mustache\\|twig\\|ejs\\|haml\\|pug\\|jade\\)$"))
    (media   . ,(concat "\\.\\("
                        "png\\|jpe?g\\|gif\\|tiff\\|svg\\|bmp" ; images
                        "\\|mov\\|avi\\|mp[34]\\|webm"         ; media
                        "\\)$"
                        ))
    (archive . "\\.\\(zip\\|rar\\|7z\\|tar\\(\\.gz\\)?\\)$"))
  "An alist mapping file type to regular expressions, used to determine what
type of icon to display for the file if `dood-neotree-file-icons' is set to
`simple'.")


;;
(defun dood--neotree-no-fringes ()
  "Remove fringes in neotree. They get reset each time you select the neotree
pane and are highlighted incorrectly."
  (set-window-fringes neo-global--window 1 0))

(defun dood--neotree-setup (&rest _)
  (setq line-spacing dood-neotree-line-spacing
        tab-width 1)
  (when (featurep 'hl-line)
    (set (make-local-variable 'hl-line-sticky-flag) t)
    (hl-line-mode +1)))

(defun dood--neotree-folder-icon-for (dir chevron)
  (let* ((path (expand-file-name dir))
         (chevron
          (if chevron
              (all-the-icons-octicon
               (format "chevron-%s" chevron)
               :height dood-neotree-chevron-size
               :v-adjust 0.1
               :face 'dood-neotree-chevron-face)
            spc))
         (icon
          (when dood-neotree-enable-folder-icons
            (all-the-icons-octicon
             (cond ((file-symlink-p path) "file-symlink-directory")
                   ((file-exists-p (format "%s/.git" path)) "file-submodule")
                   ((all-the-icons-dir-is-submodule path) "file-submodule")
                   (t "file-directory"))
             :height dood-neotree-folder-size
             :v-adjust 0
             :face 'dood-neotree-folder-face))))
    (concat chevron "\t" icon)))

(defun dood--neotree-file-icon-for (file-name)
  (cond ((eq dood-neotree-file-icons 'simple)
         (if file-name
             (propertize
               (cond ((string-match-p (cdr (assq 'code dood--neotree-file-re)) file-name)
                      (all-the-icons-octicon "file-code"))
                     ((string-match-p (cdr (assq 'media dood--neotree-file-re)) file-name)
                      (all-the-icons-octicon "file-media"))
                     ((string-match-p (cdr (assq 'archive dood--neotree-file-re)) file-name)
                      (all-the-icons-octicon "file-zip"))
                     ((string= (or (file-name-extension file-name) "") "pdf")
                      (all-the-icons-octicon "file-pdf"))
                     ((file-symlink-p file-name)
                      (all-the-icons-octicon "file-symlink-file"))
                     ((file-executable-p file-name)
                      (all-the-icons-octicon "file-binary"))
                     (t
                      (all-the-icons-octicon "file-text")))
               'face `(:family ,(all-the-icons-octicon-family) :height 1.3)
               'display '(raise 0))
           (all-the-icons-fileicon "default")))
        (t (all-the-icons-icon-for-file file-name))))

(defun dood--neo-insert-fold-symbol (type file-name)
  "Custom hybrid unicode theme with leading whitespace."
  (let ((spc "\t"))
    (or (and (eq type 'open)
             (insert
              (concat spc
                      (dood--neotree-folder-icon-for
                       file-name
                       (if dood-neotree-enable-open-chevron-icons "down"))
                      spc)))
        (and (eq type 'close)
             (insert
              (concat spc
                      (dood--neotree-folder-icon-for
                       file-name
                       (if dood-neotree-enable-closed-chevron-icons "right"))
                      spc)))
        (and (eq type 'leaf)
             (insert
              (concat (when (or dood-neotree-enable-open-chevron-icons
                                dood-neotree-enable-closed-chevron-icons)
                        spc)
                      (when dood-neotree-enable-folder-icons spc)
                      (when dood-neotree-file-icons
                        (concat spc (dood--neotree-file-icon-for file-name)))
                      spc))))))

(defun dood--neo-buffer--insert-root-entry (node)
  "Pretty-print pwd in neotree"
  (let ((project-name (file-name-nondirectory (substring node 0 (1- (length node)))))
        (face (if dood-neotree-enable-variable-pitch
                  'dood-neotree-root-face
                'neo-root-dir-face)))
    (insert
     (concat (propertize " " 'face 'neo-root-dir-face)
             (all-the-icons-octicon "repo"
                                    :height dood-neotree-project-size
                                    :face 'neo-root-dir-face
                                    :v-adjust -0.1)
             (propertize " " 'face 'neo-root-dir-face)
             (propertize (concat project-name "\n") 'face face)))))

(defun dood--neo-buffer--insert-dir-entry (node depth expanded)
  (let ((node-short-name (neo-path--file-short-name node)))
    (insert-char ?\s (* (- depth 1) 2)) ; indent
    (when (memq 'char neo-vc-integration)
      (insert-char ?\s 2))
    ;; Added this line
    (dood--neo-insert-fold-symbol
     (if expanded 'open 'close) node)
    ;;
    (insert-button node-short-name
                   'follow-link t
                   'face (if dood-neotree-enable-variable-pitch
                             'dood-neotree-dir-face
                           'neo-dir-link-face)
                   'neo-full-path node
                   'keymap neotree-dir-button-keymap)
    (neo-buffer--node-list-set nil node)
    (neo-buffer--newline-and-begin)))

(defun dood--neo-buffer--insert-file-entry (node depth)
  (let ((node-short-name (neo-path--file-short-name node))
        (vc (when neo-vc-integration (neo-vc-for-node node))))
    (insert-char ?\s (* (- depth 1) 2)) ; indent
    (when (memq 'char neo-vc-integration)
      (insert-char (car vc))
      (insert-char ?\s))
    ;; Added this line
    (dood--neo-insert-fold-symbol 'leaf node)
    ;;
    (insert-button node-short-name
                   'follow-link t
                   'face (if (memq 'face neo-vc-integration)
                             (cdr vc)
                           (if dood-neotree-enable-variable-pitch
                               'dood-neotree-file-face
                             'neo-file-link-face))
                   'neo-full-path node
                   'keymap neotree-file-button-keymap)
    (neo-buffer--node-list-set nil node)
    (neo-buffer--newline-and-begin)))


;;
(eval-after-load "neotree"
  (lambda ()
    ;; Enable buffer-local hl-line and adjust line-spacing
    (add-hook 'neo-after-create-hook 'dood--neotree-setup)
    ;; Incompatible
    (setq neo-vc-integration nil)
    ;; Remove fringes in Neotree pane
    (advice-add 'neo-global--select-window :after 'dood--neotree-no-fringes)
    ;; Patch neotree to use `dood--neo-insert-fold-symbol'
    (advice-add 'neo-buffer--insert-file-entry :override 'dood--neo-buffer--insert-file-entry)
    (advice-add 'neo-buffer--insert-dir-entry  :override 'dood--neo-buffer--insert-dir-entry)
    ;; Shorter pwd in neotree
    (advice-add 'neo-buffer--insert-root-entry :override 'dood--neo-buffer--insert-root-entry)))

(provide 'dood-neotree)
;;; dood-neotree.el ends here
