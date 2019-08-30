;;; feature/chess/autoload.el -*- lexical-binding: t; -*-



(defun chess-get-fen-on-line ()
  (interactive)
  (let ((chess-fen-regex "\\([bnrqkpBNRQKP1-8]*/?\\)+ [bw] \\(-\\|[KQkq]+\\) \\(-\\|[1-8]\\)")
        (curr-line (buffer-substring-no-properties
                    (line-beginning-position) (line-end-position))))
    (save-excursion
      (and (string-match chess-fen-regex curr-line)
           (match-string 0 curr-line)))))

(defun chess-make-pos-from-fen (fen)
  (let* ((game (chess-game-create))
        (new-display (chess-display-create game 'chess-images nil)))
    (chess-game-set-start-position game (chess-fen-to-pos fen))
    (chess-display-set-game new-display game 1)
    (chess-display-popup new-display)))

;;;###autoload
(defun chess-show-fen-at-point ()
  (interactive)
  (make-pos-from-fen (call-interactively 'get-fen-on-line)))
