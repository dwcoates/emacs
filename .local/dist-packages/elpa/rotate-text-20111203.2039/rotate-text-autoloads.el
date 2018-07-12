;;; rotate-text-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "rotate-text" "rotate-text.el" (23294 21734
;;;;;;  175175 516000))
;;; Generated autoloads from rotate-text.el

(autoload 'rotate-text "rotate-text" "\
Rotate the text at point. If there is nothing to rotate at point and DEFAULT-STRING is non-nil,
DEFAULT-STRING is inserted at point.

COM-SYMBOLS, COM-WORDS and COM-PATTERNS are per-command addition to `rotate-text-symbols',
`rotate-text-words' and `rotate-text-patterns', respectively.

\(fn ARG &optional DEFAULT-STRING COM-SYMBOLS COM-WORDS COM-PATTERNS)" t nil)

(autoload 'rotate-text-backward "rotate-text" "\
Rotate the text at point backwards. If there is nothing to rotate at point and DEFAULT-STRING is non-nil,
DEFAULT-STRING is inserted at point.

COM-SYMBOLS, COM-WORDS and COM-PATTERNS are per-command addition to `rotate-text-symbols',
`rotate-text-words' and `rotate-text-patterns', respectively.

\(fn ARG &optional DEFAULT-STRING COM-SYMBOLS COM-WORDS COM-PATTERNS)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; rotate-text-autoloads.el ends here
