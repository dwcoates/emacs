;;; demangle-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "demangle-mode" "demangle-mode.el" (23326 62260
;;;;;;  139508 494000))
;;; Generated autoloads from demangle-mode.el

(autoload 'demangle-mode "demangle-mode" "\
Toggle demangle mode.

Interactively with no argument, this command toggles the mode.  A
positive prefix argument enables the mode; any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, while `toggle' toggles the state.

When Demangle mode is enabled, mangled C++ symbols appearing
within the buffer are demangled, making their decoded C++ forms
visible.

Visit `https://github.com/liblit/demangle-mode/issues' or use
\\[demangle-mode-submit-bug-report] to report bugs in
`demangle-mode'.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; demangle-mode-autoloads.el ends here
