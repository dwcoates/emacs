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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; Helm ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(if (not (require 'helm-color nil t))
    (warn "Cannot load `helm-color'. Some goodies not avaliable.")

  (defun helm-insert-color-name ()
    "Insert the color name at point for the color selected via helm"
    (interactive)
    (insert (helm-colors)))

  (defun helm-insert-color-hex ()
    "Insert the hex value at point for the color selected via helm"
    (interactive)
    (insert (apply 'color-rgb-to-hex (color-name-to-rgb (helm-colors)))))
  )


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


(provide 'dwc-goodies)

;;; dwc-goodies.el ends here
