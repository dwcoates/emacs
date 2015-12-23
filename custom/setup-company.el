(require 'company)

(add-hook 'after-init-hook 'global-company-mode)
(setq company-backends (delete 'company-semantic company-backends))

(define-key c-mode-map  [(control tab)] 'company-complete)
(define-key c++-mode-map  [(control tab)] 'company-complete)

(global-set-key (kbd "C-c <tab>") 'helm-company)

;; company-c-headers
;(add-to-list 'company-c-headers-path-system "/usr/include/c++/4.8/")
(add-to-list 'company-backends 'company-c-headers)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))
;; (optional) adds CC special commands to `company-begin-commands' in order to
;; trigger completion at interesting places, such as after scope operator std::|
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

;; hs-minor-mode for folding source code
(add-hook 'c-mode-common-hook 'hs-minor-mode)


;; *********** Available C style: ***************
;; “gnu”:    The default style for GNU projects
;; “k&r”:    What Kernighan and Ritchie, the authors of C used in their book
;; “bsd”:    What BSD developers use, aka “Allman style” after Eric Allman.
;; “whitesmith”: Popularized by the examples that came with Whitesmiths C, an early commercial C compiler.
;; “stroustrup”: What Stroustrup, the author of C++ used in his book
;; “ellemtel”:  Popular C++ coding standards as defined by “Programming in C++, Rules and Recommendations,”
;;             Erik Nyquist and Mats Henricson, Ellemtel
;; “linux”:  What the Linux developers use for kernel development
;; “python”:What Python developers use for extension modules
;; “java”:  The default style for java-mode (see below)
;; “user”:  When you want to define your own style
;; **********************************************
(setq
 c-default-style "linux" ;; set style to "linux"
 )


(provide 'setup-company)
;;; setup-company.el ends here
