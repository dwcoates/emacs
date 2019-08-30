;;; feature/chess/config.el -*- lexical-binding: t; -*-

(def-package! chess
  :init
  (map!
   :map global-map
   :prefix "C-x 9"
   "f" 'chess-show-fen-at-point)
  :config
  (set! :popup 'chess-mode :size 100 :regexp t :autokill t)
  (setq chess-images-directory
        (concat user-emacs-directory
                ".local/dist-packages/elpa/chess-2.0.4/pieces/xboard")
        chess-images-default-size 58
        ;; I like it in current frame.
        chess-images-separate-frame t))
