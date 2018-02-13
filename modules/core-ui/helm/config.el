;;; completion/helm/config.el -*- lexical-binding: t; -*-

;; Warning: since I don't use helm, this may be out of date.

(defvar +helm-global-prompt "››› "
  "The helm text prompt prefix string is globally replaced with this string.")


;;
;; Packages
;;

(def-package! helm
  :commands (helm-M-x helm-show-kill-ring helm-M-x helm-all-mark-rings helm-mini
            helm-buffers-list helm-all-mark-rings helm-occur helm-insert-command-name)
  :init
  (map! :map global-map
        "M-x" 'helm-M-x
        "M-y"  'helm-show-kill-ring
        "M-X"  'helm-M-x
        "C-h SPC"  'helm-all-mark-rings
        "C-x b"  'helm-mini
        "C-x C-o"  'helm-buffers-list
        "C-h SPC"  'helm-all-mark-rings
        "C-c s"  'helm-occur
        "C-h F"  'helm-insert-command-name)

  :config
  (message "loaded helm")
  (load "helm-autoloads" nil t)
  (add-hook 'doom-init-hook #'helm-mode)

  (helm-autoresize-mode t)

  (defvar helm-projectile-find-file-map (make-sparse-keymap))
  (require 'helm-projectile)
  (set-keymap-parent helm-projectile-find-file-map helm-map)

  ;; helm is too heavy for find-file-at-point
  (after! helm-mode
    (add-to-list 'helm-completing-read-handlers-alist '(find-file-at-point . nil)))

  (set! :popup "\\` ?\\*[hH]elm.*?\\*\\'" :size 14 :regexp t)
  (setq projectile-completion-system 'helm)

  (setq helm-scroll-amount                    4
        helm-ff-search-library-in-sexp        t
        helm-split-window-in-side-p           t
        helm-candidate-number-limit           50 ;; No reason for more matches, and it's faster this way
        helm-ff-file-name-history-use-recentf nil
        helm-move-to-line-cycle-in-source     t
        helm-buffers-fuzzy-matching           nil
        helm-autoresize-max-height            10
        helm-moccur-show-buffer-fontification t ;; Keep fontification of results. Might be slower.
        helm-autoresize-min-height            3)
  
  ;; Save current position to mark ring
  (add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

  ;; Sort helm source buffers
  (defun dwc-helm-source-buffers (buffers)
    "Return sorted source-buffers.  Helm will not sort results by default."
    (let ((last-used (subseq buffers 0 (min 5 (length buffers))))
          (buffers (subseq buffers (min 6 (length buffers))))
          dired-buffers
          other-buffers
          (buf-sort (lambda (bufs)
                      (cl-sort 
                       bufs
                       (lambda (a b)
                         (or (< (length a) (length b))
                             (and (= (length a) (length b))
                                  (string-lessp a b))))))))
      (dolist (buf buffers)
        (if (with-current-buffer buf
              (eq major-mode 'dired-mode))
            (push buf dired-buffers)
          (push buf other-buffers)))
      (append
       (funcall buf-sort last-used)
       (funcall buf-sort other-buffers)
       (funcall buf-sort dired-buffers))))

  (defun helm-buffers-sort-dired-buffers (orig-fun &rest args)
    (dwc-helm-source-buffers (apply orig-fun args)))

  (advice-add 'helm-buffers-sort-transformer :around 'helm-buffers-sort-dired-buffers)

  ;;; Helm hacks
  (defun +helm*replace-prompt (plist)
    "Globally replace helm prompts with `+helm-global-prompt'."
    (if (keywordp (car plist))
        (plist-put plist :prompt +helm-global-prompt)
      (setf (nth 2 plist) +helm-global-prompt)
      plist))
  (advice-add #'helm :filter-args #'+helm*replace-prompt)

  (defun +helm*hide-header (&rest _)
    "Hide header-line & mode-line in helm windows."
    (setq mode-line-format nil))
  (advice-add #'helm-display-mode-line :override #'+helm*hide-header)

  (map! :map global-map
        [remap apropos]                   #'helm-apropos
        [remap recentf-open-files]        #'helm-recentf
        [remap projectile-switch-to-buffer] #'helm-projectile-switch-to-buffer
        [remap projectile-recentf]        #'helm-projectile-recentf
        [remap projectile-find-file]      #'helm-projectile-find-file
        [remap imenu]                     #'helm-semantic-or-imenu
        [remap bookmark-jump]             #'helm-bookmarks
        [remap noop-show-kill-ring]       #'helm-show-kill-ring
        [remap projectile-switch-project] #'helm-projectile-switch-project
        [remap projectile-find-file]      #'helm-projectile-find-file
        [remap imenu-anywhere]            #'helm-imenu-anywhere)

  (map! :map helm-map
        "C-c C-y"  'helm-yank-selection-and-quit
        "C-i"  'helm-select-action ;; This is a big one. Use C-SPC to select entries
        "C-S-p"  'helm-previous-source
        "C-S-n"  'helm-next-source
        :map helm-buffer-map
        "C-c C-k"  'helm-buffer-run-kill-buffers)
  
  :diminish 'helm-mode)


(def-package! helm-locate
  :defer t
  :init (defvar helm-generic-files-map (make-sparse-keymap))
  :config (set-keymap-parent helm-generic-files-map helm-map))


(def-package! helm-bookmark
  :commands helm-bookmark
  :config (setq-default helm-bookmark-show-location t))


(def-package! helm-files
  :defer t
  :config
  (setq helm-boring-file-regexp-list
        (append (list "\\.projects$" "\\.DS_Store$")
                helm-boring-file-regexp-list)))


(def-package! helm-ag
  :defer t
  :config
  (map! :map helm-ag-edit-map
        [remap doom/kill-this-buffer] #'helm-ag--edit-abort
        [remap quit-window]           #'helm-ag--edit-abort))


(def-package! helm-css-scss ; https://github.com/ShingoFukuyama/helm-css-scss
  :commands (helm-css-scss
             helm-css-scss-multi
             helm-css-scss-insert-close-comment)
  :config
  (setq helm-css-scss-split-direction #'split-window-vertically
        helm-css-scss-split-with-multiple-windows t))


(def-package! helm-swoop ; https://github.com/ShingoFukuyama/helm-swoop
  :commands (helm-swoop helm-multi-swoop helm-multi-swoop-all)
  :config
  (setq helm-swoop-use-line-number-face t
        helm-swoop-candidate-number-limit 200
        helm-swoop-speed-or-color t
        helm-swoop-pre-input-function (lambda () "")))


(def-package! helm-describe-modes :commands helm-describe-modes)

