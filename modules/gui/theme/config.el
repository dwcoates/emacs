;;; gui/theme/config.el -*- lexical-binding: t; -*-

(def-package! atchka-theme)

;; Brighten buffers
(def-package! solaire-mode
  :init
  ;; Prevent color glitches when reloading either DOOM or the theme
  (add-hook! '(doom-init-ui-hook doom-reload-hook) #'solaire-mode-reset)
  :hook (after-change-major-mode . turn-on-solaire-mode)
  :hook (doom-popup-mode . turn-off-solaire-mode)
  :config
  (setq solaire-mode-real-buffer-fn
        (lambda (buf)
          (and (not buffer-file-name)
               (let ((mini-win (active-minibuffer-window)))
                 (when mini-win
                   (equal (window-buffer mini-win) buf))))
          ))

  ;; Minibuffer
  ; (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)

  (add-hook!
    (gist-mode twittering-mode mu4e-view-mode org-tree-slide-mode +regex-mode)
    #'solaire-mode))
