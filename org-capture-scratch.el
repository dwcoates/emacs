;;; package --- summary
;;; Commentary:
;;; Code:

;;;;;; MISC
;; nugget
;; notable stuff
;; quote

(defun org-template/header (task &optional priority prompt more-tags)
  "Build a capture template header where TASK is an org todo marker.

PRIORITY, PROMPT, and MORE-TAGS can be strings, nil or otherwise non-nil.
If otherwise non-nil, they will use default settings."
  (setq task (downcase task))
  (let ((priority (if priority " %^{Priority|C|A|B|C|D|E}" ""))
        (prompt (if prompt (concat
                            " " (if (stringp prompt)
                                    prompt
                                  (concat "%^{" (capitalize task) "}")))
                  " %?"))
        (more-tags (if more-tags " %G" "")))
    (concat "* " priority " " (upcase task) prompt more-tags)))

(defun org-project-template-builder (header &optional tags scheduling body watermark properties)
  "Build a capture template with HEADER, TAGS, SCHEDULING, BODY, WATERMARK.

PROPERTIES and MORE-TAGS are additional optional capture components."
  (concat
   header " "
   (when tags
     (concat
      ":"
      (if (listp tags)
          (mapconcat 'identity tags ":")
        tags)
      ":"))
   "\n"
   (when properties
     (concat ":PROPERTIES:\n"
             (mapconcat (lambda (p) (concat ":" (car p) ": " (cadr p))) properties "\n")
             "\n:END:\n"))
   (when scheduling (if (stringp scheduling) (concat scheduling "\n") "%^{Schedule|SCHEDULE|DEADLINE|}: %T"))
   "\n"
   (when body (concat (if (stringp body) body "\t%?") "\n"))
   (if watermark watermark org-template/meta-data)))

(defun t-wrapper
    (SEC GLOBAL-TAGS PREFIX
         HD-TASK HD-PRIO HD-PROMPT
         HD-MORE-TAGS SCHEDULING BODY WATERMARK
         PROPERTIES MORE-TAGS)
  "Stuff SEC GLOBAL-TAGS PREFIX HD-TASK HD-PRIO HD-PROMPT HD-MORE-TAGS.
SCHEDULING BODY WATERMARK PROPERTIES MORE-TAGS."

  (defun w-wrapper (SEC KEYWORD NULL)
    "This is a SEC KEYWORD NULL."
    (if (plist-member SEC KEYWORD)
        (plist-get SEC KEYWORD)
      NULL))

  (list
   PREFIX
   (plist-get SEC :description)
   (plist-get SEC :location)
   (org-project-template-builder
    (w-wrapper SEC :headers (org-template/header
                             HD-TASK
                             HD-PRIO
                             HD-PROMPT
                             HD-MORE-TAGS))
    (append GLOBAL-TAGS (w-wrapper SEC :scheduling nil))
    (w-wrapper SEC :scheduling SCHEDULING)
    (w-wrapper SEC :body BODY)
    (w-wrapper SEC :watermark WATERMARK)
    (w-wrapper SEC :properties PROPERTIES)
    )
   (plist-get SEC :keywords)))

(setq basic '(:description "hello, folks!" :location '(id "w293482j-12384985") :todo '(:scheduling "!!!!!LKJDFI!!!!!!")))
(let ((vv (plist-get basic :todo)))
  (t-wrapper vv '("sometag" "anothertag") "fft" "todo" t nil nil t nil nil nil nil))


(defmacro org-make-project-templates (prefix global-tags &rest args)
  "A macro that takes PREFIX and GLOBAL-TAGS and ARGS."
  (let ((basic (plist-get args :basic))
        (study (plist-get args :study))
        (project (plist-get args :project)))
    (append
     (apply
      'cons
      (t-wrapper (plist-get basic :todo) (concat prefix "t") global-tags "todo" t nil nil t nil nil nil) ;; tasks
      (t-wrapper (plist-get basic :idea) (concat prefix "i") global-tags "idea" t nil nil t nil nil nil) ;; idea
      (t-wrapper (plist-get basic :note) (concat prefix "n") global-tags "note" t nil nil t nil nil nil)) ;; note
     (when study
       (apply
        'cons
        (t-wrapper (plist-get study :question) (concat prefix "U") global-tags "question" t "Question" nil nil nil nil nil) ;; question
        (t-wrapper (plist-get study :quick-question) (concat prefix "u") global-tags "question" t "Quick Question" nil nil nil nil nil) ;; quick question
        (t-wrapper (plist-get study :review) (concat prefix "r") global-tags "review" t nil nil t nil nil nil) ;; refresh)
        (t-wrapper (plist-get study :learn) (concat prefix "l") global-tags "learn" t nil nil t nil nil nil))) ;; learn)
     (when project
       (apply
        'cons
        (t-wrapper (plist-get project :issue) (concat prefix "s") global-tags "issue" t t nil t nil nil nil) ;; issue
        (t-wrapper (plist-get project :bug) (concat prefix "b") global-tags "bug" t t nil t nil nil nil) ;; bug
        (t-wrapper (plist-get project :feature) (concat prefix "f") global-tags "feature" t t nil t nil nil nil nil)))  ;; feature
     )))
