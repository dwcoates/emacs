;; This library implements a Jupyter notebook back-end for Org
;; exporter, based on the Github flavored Markdown back-end.
;; TODO:
;; - [ ] pre-processing before exporting: remove COMMENT headlines
;; -
;;; Code:

(require 'ox-md)



;;; User-Configurable Variables

(defgroup org-export-jupyter nil
  "Options specific to Markdown export back-end."
  :tag "Org Jupyter Notebook"
  :group 'org-export
  :version "24.4"
  :package-version '(Org . "8.0"))

(defcustom org-jupyter-lang '(("emacs-lisp" . "lisp") ("elisp" . "lisp"))
  "Alist of languages that are not recognized by Github, to
  languages that are. Emacs lisp is a good example of this, where
  we can use lisp as a nice replacement."
  :group 'org-export-jupyter)



;;; Define Back-End

(org-export-define-derived-backend 'jupyter 'md
  :export-block '("JUPYTER" "JUPYTER NOTEBOOK")
  :filters-alist '((:filter-parse-tree . org-md-separate-elements))
  :menu-entry
  '(?j "Export to Jupyter Notebook"
       ((?J "To temporary buffer"
            (lambda (a s v b) (org-jupyter-export-as-markdown a s v)))
        (?j "To file" (lambda (a s v b) (org-jupyter-export-to-markdown a s v)))
        (?o "To file and open"
            (lambda (a s v b)
              (if a (org-jupyter-export-to-markdown t s v)
                (org-open-file (org-jupyter-export-to-markdown nil s v)))))))
  :translate-alist '((inner-template . org-jupyter-inner-template)
                     (strike-through . org-jupyter-strike-through)
                     (src-block . org-jupyter-src-block)))



;;; Transcode Functions

;;;; Src Block

(defun org-jupyter-src-block (src-block contents info)
  "Transcode SRC-BLOCK element into Github Flavored Markdown
format. CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let* ((lang (org-element-property :language src-block))
         (lang (or (assoc-default lang org-jupyter-lang) lang))
         (code (org-export-format-code-default src-block info))
         (prefix (concat "```" lang "\n"))
         (suffix "```"))
    (concat prefix code suffix)))


;;;; Strike-Through

(defun org-html-strike-through (strike-through contents info)
  "Transcode STRIKE-THROUGH from Org to Markdown (JUPYTER).
CONTENTS is the text with strike-through markup.  INFO is a plist
holding contextual information."
  (format "~~%s~~" contents))

;;;; Table of contents

(defun org-jupyter-format-toc (headline)
  "Return an appropriate table of contents entry for HEADLINE. INFO is a
plist used as a communication channel."
  (let* ((title (org-export-data
                 (org-export-get-alt-title headline info) info))
         (level (1- (org-element-property :level headline)))
         (indent (concat (make-string (* level 2) ? )))
         (ref-str (replace-regexp-in-string " " "-" (downcase title))))
    (concat indent "- [" title "]" "(#" ref-str ")")))


;;;; Template
;; Three functions that do most of the work: cell, template and buffer.
(defun org-jupyter-cell (type string &optional metadata)
  "Return string corresponding to a cell in Jupyter notebook"
  (defun pre-process-string-latex (string)
    "Replace \\(XXX\\) by $XXX$ for LaTeX fragments"
    ;;(replace-regexp-in-string (rx "\\\\(\(.*\)\\\\)") "$\1$" "Inline latex: \\\\(\\\\exp{i\\\\pi} + 1 = 0\\\\)\\n")
    ;;(replace-regexp-in-string "\\\\(\\(.*\\)\\\\)" "$\\1$" string))
    (replace-regexp-in-string (rx "\\(\(.*\)\\)") "$\\1$" string))
    ;; (with-temp-buffer
    ;;   (insert string)
    ;;   (goto-char 1)
    ;;   (replace-regexp "\\\\\\\\(\\(.*\\)\\\\\\\\)" "$\\1$")
    ;;   ;;(replace-regexp (rx "\\\\(\(.*\)\\\\)") "$\1$")
    ;;   (buffer-string)))
  ;;string)
  ;;(message (replace-regexp-in-string "\\\\" "\\" "\\begin{equation}"))
  (defun pre-process-string (string)
    "Pre-process the string"
    (pre-process-string-latex string))

  ;;(let* ((lines-list (split-string string "\n"))
  (let* ((lines-list (split-string (pre-process-string string) "\n"))
     (lines-string (with-temp-buffer
             (insert (mapconcat (lambda (l)
                          ;;(format "\"%s\\n\"," l))
                          ;;(format "%S," l)
                          ;;(let ((l "test"))
                          (format "%s\\n\"," (with-temp-buffer
                                   (insert (prin1-to-string l))
                                   (delete-char -1)
                                   (buffer-string))))
                        lines-list
                        "\n  "))
             (delete-char -1)
             (buffer-string))))

    (format "{
 \"cell_type\": \"%s\",
 \"metadata\": %s,
  %s
 \"source\": [
  %s
  ]
 }" type
 (if metadata
     metadata
   "{}")
 (if (string= type "code")
     "\"outputs\": [],\n \"execution_count\": 1,\n"
   "")
 lines-string)))


(defun org-jupyter-inner-template (contents info)
  "Return body of document after converting it to Markdown syntax.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (format "{
\"cells\": [
 %s
],
 \"metadata\": {
  \"kernelspec\": {
   \"display_name\": \"Python 2\",
   \"language\": \"python\",
   \"name\": \"python2\"
  },
  \"language_info\": {
   \"codemirror_mode\": {
    \"name\": \"ipython\",
    \"version\": 2
   },
   \"file_extension\": \".py\",
   \"mimetype\": \"text/x-python\",
   \"name\": \"python\",
   \"nbconvert_exporter\": \"python\",
   \"pygments_lexer\": \"ipython2\",
   \"version\": \"2.7.10\"
  }
 },
 \"nbformat\": 4,
 \"nbformat_minor\": 0
}" contents))


(defun export-region-as-markdown (beg end)
  "Export region as Github flavoured markdown"
  (condition-case nil
      (org-jupyter-cell "markdown"
            (org-export-string-as
             ;;(buffer-substring-no-properties beg end) 'gfm t))
             (buffer-substring-no-properties beg end) 'gfm t '(:with-toc nil)))
    (error (format "ERROR: Markdown export for region %d %d failed " beg end)
       nil)))

(defun export-buffer-to-notebook ()
  "Export current buffer to notebook"
  (interactive)


  (save-excursion
    (goto-char 1)


    (let ((start 1) output (notebook-filename (org-export-output-file-name ".ipynb")))

      (defun append-cell (cell)
    (setq output (if output
             (if cell
                 (format "%s,\n%s" output cell)
               output)
               cell)))

      ;; Add title cell to notebook: should probably be part of inner-template.
      ;;; -> change the style of the title hre
      ;;; -> should be customizable variable
      (append-cell (org-jupyter-cell "markdown"
                     (format "<h1 align=\"center\"><font color=\"0066FF\" size=110>%s</font></h1>"
                         (substring-no-properties
                          (car
                           (plist-get (org-export-get-environment)  :title))))))

      (catch 'no-more-blocks
    (while t
      (condition-case nil
          (org-next-block 1)
        (error (progn
             (message "DBG: no more blocks")
             (throw 'no-more-blocks nil)
             nil)))

      ;; Process region until next block as markdown cell
      (append-cell (export-region-as-markdown start (point)))
      ;; Update point
      (setq start (point))

      ;; Process block
      (org-babel-mark-block)
      (append-cell (org-jupyter-cell "code"
                     (buffer-substring-no-properties
                      (region-beginning) (region-end))))
      ;; Update point
      (goto-char (region-end))
      (forward-line 2)
      (setq start (point))))
      ;; No more blocks: process region until the end as markdown cell
      (append-cell (export-region-as-markdown start (point-max)))

    ;; Create notebook
    (with-temp-buffer
      (insert (org-jupyter-inner-template output nil))
      (json-mode)
      (mark-whole-buffer)
      (indent-for-tab-command)
      ;;(write-file "simple-notebook-test.ipynb"))
      (write-file notebook-filename))
    output)))






;;; Interactive function

;;;###autoload
(defun org-jupyter-export-as-markdown (&optional async subtreep visible-only)
  "Export current buffer to a Github Flavored Markdown buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org JUPYTER Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (org-export-to-buffer 'jupyter "*Org JUPYTER Export*"
    async subtreep visible-only nil nil (lambda () (text-mode))))


;;;###autoload
(defun org-jupyter-convert-region-to-md ()
  "Assume the current region has org-mode syntax, and convert it
to Github Flavored Markdown.  This can be used in any buffer.
For example, you can write an itemized list in org-mode syntax in
a Markdown buffer and use this command to convert it."
  (interactive)
  (org-export-replace-region-by 'jupyter))


;;;###autoload
(defun org-jupyter-export-to-markdown (&optional async subtreep visible-only)
  "Export current buffer to a Github Flavored Markdown file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".md" subtreep)))
    (org-export-to-file 'jupyter outfile async subtreep visible-only)))

(provide 'ox-jupyter)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; ox-jupyter.el ends here
