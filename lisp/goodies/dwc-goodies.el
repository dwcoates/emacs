;;; package --- Summary:
;;
;; Author: Dodge W. Coates
;; Some misc goodie functions and stuff
;;
;;; Commentary:
;;
;;; Code:


(require 'cl)

  ;; Courtesy of Xah
(defun get-string-from-file (filePath)
  "Return FILEPATH's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

  ;; Courtesy of Xah
(defun read-lines (filePath)
  "Return a list of lines of a file at FILEPATH."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(defun split-in-two (predicate list)
  "According to PREDICATE, split LIST in two."
  (loop for x in list
        if (funcall predicate x) collect x into yes
        else collect x into no
        finally (return (values yes no))))

(defun first-middle-last (list)
  "Return a list composed of three elements.
The first and last of which are the same as that of LIST."
  (list (car list) (nbutlast list) (car (last list))))

(defun switch-to-other-buffer ()
  "Switch to last visited buffer."
  (interactive)
  (switch-to-buffer (other-buffer) (current-buffer) 1))

(defun toggle-maximize-buffer ()
  "Maximize/minimize buffer"
       (interactive)
       (if (= 1 (length (window-list)))
           (jump-to-register '_)
         (progn
           (window-configuration-to-register '_)
           (delete-other-windows))))

(defun transpose-windows (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

(defun re-seq (regexp dwc-str)
  "Get a list of all regexp matches in a string"
  (save-match-data
    (let ((pos 0)
          matches)
      (while (string-match regexp dwc-str pos)
        (push (match-string 0 dwc-str) matches)
        (setq pos (match-end 0))
        )
      matches)
    )
  )

(defun rand-alphanum (length &optional prefix suffix)
  "Generate a random character of some length"
  (let
      ((alpha-num (cdr (split-string
                        "1234567890abcdefghijklmnopqrstyvwxyzABCDEFGHIJKLMNOPQRSTYVWXYZ"
                        "")))
       ret)
    (dotimes (i length)
      (push (elt alpha-num (random (length alpha-num))) ret))
    (concat prefix (mapconcat 'identity ret "") suffix)))

(defun insert-rand-alphanum (&optional length prefix suffix)
  (interactive)
  (let ((length (or length 6)))
    (insert (rand-alphanum length prefix suffix))
    ))

(defun my/remove-headers ()
  (goto-char (point-min))
  (re-search-forward "^$")
  (goto-char (+ 1 (point)))
  (delete-region (point) (point-min)))

(defun increment-integers-in-region (beg end &optional inc)
  "Increment the integers in current region."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end)))
               (list (point) (point)))
  (save-excursion
    (goto-char beg)
    (let ((end (save-excursion (goto-char end)
                               (forward-word-strictly) (backward-char) (point))))
      (while (< (point) end)
        (ignore-errors (increment-integer-at-point inc))
        (forward-word-strictly)))))

(defun decrement-integers-in-region (beg end &optional dec)
  "Decrement the integers in current region."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end)))
               (list (point) (point)))
  (increment-integers-in-region beg end (- (or dec 1)))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; Helm ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(if (not (require 'helm nil t))
    (warn "Cannot load `helm'. Some goodies not avaliable.")



  (defun helm-insert-color-hex ()
    "Insert the hex value at point for the color selected via helm"
    (interactive)
    (insert (apply 'color-rgb-to-hex (color-name-to-rgb (helm-colors)))))

  (defun helm-insert-command-name ()
    "Insert command name at point"
    (interactive)
    (insert (helm-M-x-read-extended-command))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;; subr-x ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(if (not (require 'subr-x nil t))
    (warn "Cannot load `subr-x'. Some goodies not available.")

  (defun get-current-line (&optional trim-left trim-right)
    (interactive)
    "Return current line as a string. Option to trim whitespace from beginning and end of string"
    (save-excursion
      (let ((string (buffer-substring-no-properties (line-beginning-position)
                                                   (line-end-position))))
        (when trim-left (setq string (string-trim-left string)))
        (when trim-right (setq string (string-trim-right string)))
        string)
      ))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; hi-lock ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (require 'hi-lock nil t)
  (defun unhighlight-all-in-buffer ()
    "Remove all highlights made by `hi-lock' from the current buffer.
The same result can also be be achieved by \\[universal-argument] \\[unhighlight-regexp]."
    (interactive)
    (unhighlight-regexp t))
  (define-key search-map "hU" #'my/unhighlight-all-in-buffer))


(provide 'dwc-goodies)


;;; dwc-goodies.el ends here
