;;; core-ui/ido/autoload/ido.el -*- lexical-binding: t; -*-

(defvar +small-ido-max-window-height 1)

;;;###autoload
(defun +smex ()
  "Like `smex', but use `small-ido-max-window-height' to
limit the buffer size.`"
  (interactive)
  (let ((ido-max-window-height +small-ido-max-window-height))
    (call-interactively 'smex)))

;;;###autoload
(defun +ido-find-file ()
  "Like `ido-find-file', but use `small-ido-max-window-height' to
limit the buffer size.`"
  (interactive)
  (let ((ido-max-window-height +small-ido-max-window-height)
        (ido-mode 1))
    (call-interactively 'ido-find-file)))
