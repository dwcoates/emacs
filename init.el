;; Keep track of loading time
(defconst emacs-start-time (current-time))

(require 'org)
(org-babel-load-file
 (expand-file-name "config.org"
                   user-emacs-directory))
