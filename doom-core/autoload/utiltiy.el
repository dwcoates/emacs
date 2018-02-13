;;; ../doom-core/autoload/utiltiy.el -*- lexical-binding: t; -*-

;;;###autoload
(defun occur-step (arg)
  (let* ((num 0)
         (arg (or arg 1))
         (foo (if (or (not arg) (>= arg 0))
                  'occur-next
                'occur-prev)))
    (while (< num (abs arg))
      (setq num (1+ num))
      (funcall foo)))
  (recenter))

;;;###autoload
(defun occur-step-forward (arg)
  (interactive "P")
  (let ((arg (if arg (abs arg) 1)))
    (occur-step arg))
  (occur-mode-display-occurrence))

;;;###autoload
(defun occur-step-backward (arg)
  (interactive "P")
  (let ((arg (if arg (- (abs arg)) -1)))
    (occur-step arg))
  (occur-mode-display-occurrence))

;;;###autoload
(defun find-file-init-dot-el ()
    (interactive)
    (find-file "~/.emacs.d/init.el"))

;;;###autoload
(defun find-file-config-dot-org ()
  (find-file-other-window "~/.emacs.d/config.org"))

;;;###autoload
(defun find-buffer-messages-buffer ()
  (find-file-other-window "*Messages*"))

;;;###autoload
(defun find-buffer-scratch-buffer ()
  (find-file-other-window "*Scratch*"))
