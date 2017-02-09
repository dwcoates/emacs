;;; guide-key-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "guide-key" "guide-key.el" (22683 58181 154585
;;;;;;  243000))
;;; Generated autoloads from guide-key.el

(defvar guide-key-mode nil "\
Non-nil if Guide-Key mode is enabled.
See the `guide-key-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `guide-key-mode'.")

(custom-autoload 'guide-key-mode "guide-key" nil)

(autoload 'guide-key-mode "guide-key" "\
Toggle guide key mode.

In guide key mode, Guide following keys to an input key sequence
automatically and dynamically.
With a prefix argument ARG, enable guide key mode if ARG is
positive, otherwise disable.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("guide-key-pkg.el") (22683 57681 118572
;;;;;;  651000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; guide-key-autoloads.el ends here
