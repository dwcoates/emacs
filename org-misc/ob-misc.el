;;; Code --- summary:
;;
;; Author: Dodge W. Coates
;; A miscellaneous assortment of functions for org babel editing
;;
;;; Commentary:
;;
;;; Code:

(require 'org)

(defun org-goto-src-headers ()
  "Move point to the first header of the source block currently containing point."
  (interactive)
  (org-babel-goto-src-block-head)
  (beginning-of-line)
  (unless (re-search-forward "#\\+begin_src[ ]+?" (line-end-position))
    (error "Error: Couldn't find source header.  This shouldn't happen")))

(defun org-get-src-headers ()
  "Get the src headers for current source block."
  (interactive)
  (save-excursion
    (org-goto-src-headers)
    (split-string
     (buffer-substring-no-properties (point) (line-end-position))
     "[ ]+")))

(defun org-get-src-headers-string (&optional headers NO-PROPERTIES)
  "Return the org block HEADERS in the string format.
If non-nil optional NO-PROPERTIES will strip properties from returned string"
  (let ((func (if NO-PROPERTIES
                  (lambda (string) (substring-no-properties string))
                'identity))
        (headers (or headers (org-get-src-headers))))
    (mapconcat (lambda (header) (funcall func header))
               headers
               " ")))

(defun org-propertize-headers (&optional headers)
  "Propertize string version of org block HEADERS list correctly for display.
HEADERS defaults to those of the current org block."
  (let* ((headers (or headers (org-get-src-headers)))
        (language (car headers))
        (args (cdr headers)))
    (concat
     (propertize language 'face '(:foreground "pink" :bold t))
     " "
     (mapconcat (lambda (header-element)
                  (propertize header-element 'face
                              (if (string-match ":[[[:alnum:]-*]]*" header-element)
                                  '(:inherit org-list-dt :bold t)
                                '(:inherit org-verbatim :bold t)
                                )))
                args
                " "))))

(defun org-edit-src-headers (headers)
  "Edit the org block HEADERS for block currently enclosing point."
  (interactive (list
                (let ((minibuffer-allow-text-properties t))
                  (read-from-minibuffer
                   "Block header: "
                   (org-propertize-headers)))))
  (save-excursion
    (org-goto-src-headers)
    (delete-region (point) (line-end-position))
    (insert headers)))

(defun org-print-src-headers ()
  "Display in the minibuffer the headers for current org block."
  (interactive)
  (save-excursion
    (org-goto-src-headers)
    (let ((headers (org-eldoc-get-src-header)))
      (message headers)
      )))


(provide 'ob-misc)

;;; ob-misc.el ends here
