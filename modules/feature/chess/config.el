;;; feature/chess/config.el -*- lexical-binding: t; -*-

(def-package! chess
  :config
  (set! :popup 'chess-mode :size 100 :regexp t :autokill t :same t)
  (setq chess-images-directory
        (concat user-emacs-directory
                ".local/dist-packages/elpa/chess-2.0.4/pieces/xboard")
        chess-images-default-size 58
        ;; I like it in current frame.
        chess-images-separate-frame t))
